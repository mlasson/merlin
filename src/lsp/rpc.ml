
type 'state t = {
  input : Messages.Reader.t;
  output : Messages.Writer.t;
 
  mutable state : state;
  pending_actions : ('state -> ('state, string) result) Queue.t;
  pending_requests : (int, bool ref) Hashtbl.t;
}

and state =
  | Ready
  | Initialized of Protocol.Initialize.client_capabilities
  | Closed

let push {pending_actions; _} f =
  Queue.push f pending_actions

let {Logger. log} = Logger.for_section "lsp"

let cpt = ref 0
let next {pending_actions; _} state =
  incr cpt;
  match Queue.pop pending_actions with
  | exception Queue.Empty -> Ok state
  | f ->
    log ~title:"debug" "next: %d pendings %d" (Queue.length pending_actions) !cpt;
    f state

let send rpc json =
  log ~title:"debug" "send: %a" (fun () json -> Yojson.Safe.to_string json) json;
  Logger.log_flush ();
  let data = Yojson.Safe.to_string json in
  Messages.Writer.write rpc.output data; 
  log ~title:"debug" "send: %a" (fun () json -> Yojson.Safe.to_string json) json;
  Logger.log_flush ()

module Packet = struct
  type t = {
    id: int option [@default None];
    method_: string [@key "method"];
    params: Yojson.Safe.json;
  } [@@deriving yojson { strict = false }]
end

let rec read rpc =
  let timeout = if Queue.length rpc.pending_actions = 0 then 0.001 else 0.0 in
  let open Utils.Result.Infix in
  let parse_json content =
    match Yojson.Safe.from_string content with
    | json ->
      log ~title:"debug" "rcv: %a" (fun () json -> Yojson.Safe.to_string json) json;
      Ok json
    | exception Yojson.Json_error msg ->
      errorf "error parsing json: %s" msg
  in
  match Messages.Reader.read ~timeout rpc.input with
  | None -> None
  | Some content -> 
    Some (parse_json content >>= Packet.of_yojson)

module Response = struct
  type response = {
    id : int;
    jsonrpc: string;
    result : Yojson.Safe.json;
  } [@@deriving yojson]

  type response_error = {
    id : int;
    jsonrpc: string;
    error : error;
  } [@@deriving yojson]

  and error = {
    code : int;
    message : string;
  }

  type t =
    | Response of response
    | Response_error of response_error

  let make id result =
    Response {id; result; jsonrpc="2.0"}

  let make_error id code message =
    Response_error {id; error = {code; message;}; jsonrpc="2.0"}

  let to_yojson = function
    | Response v -> response_to_yojson v
    | Response_error v -> response_error_to_yojson v
end

module Inspector = struct

  type item = {
    type_: string [@key "type"];
    message: Yojson.Safe.json;
    timestamp: float;
  } [@@deriving yojson]

  type direction =
    | Send
    | Recv

  type kind =
    | Notification
    | Request
    | Response

  let logger = ref None

  let log (direction : direction) (f : direction -> _ -> item) x =
    match !logger with
    | None -> ()
    | Some oc ->
      let item = f direction x in
      Yojson.Safe.to_channel ~std:false oc (item_to_yojson item);
      Printf.fprintf oc "\n";
      flush oc

  let make direction kind message =
    let type_ =
       match direction, kind with
       | Send, Notification -> "send-notification"
       | Send, Request -> "send-request"
       | Send, Response -> "send-response"
       | Recv, Notification -> "recv-notification"
       | Recv, Request -> "recv-request"
       | Recv, Response -> "recv-response"
    in
    {
      type_;
      message;
      timestamp = 1000.0 *. Unix.time ()
    }

  let packet direction ({Packet.id; _} as packet) =
    let kind =
      match id with
      | None -> Notification
      | Some _ -> Request
    in
    make direction kind (Packet.to_yojson packet)

  let response direction response =
    make direction Response (Response.to_yojson response)

end

let inspector_logger filename f () =
  let t = !Inspector.logger in
  match filename with
  | None ->
    begin
      Inspector.logger := None;
      try
        let r = f () in
        Inspector.logger := t;
        r
      with e ->
        Inspector.logger := t;
        raise e
    end
  | Some "-" ->
    begin
      let oc = stderr in
      Inspector.logger := Some oc;
      try
        let r = f () in
        Inspector.logger := t;
        r
      with e ->
        Inspector.logger := t;
        raise e
    end
  | Some filename ->
    let oc = open_out_bin filename in
    Inspector.logger := Some oc;
    try
      let r = f () in
      Inspector.logger := t;
      close_out oc;
      r
    with e ->
      Inspector.logger := t;
      close_out oc;
      raise e

let send_response rpc (response : Response.t) =
  Inspector.log Recv Inspector.response response;
  let json = Response.to_yojson response in
  send rpc json

module Server_notification = struct
  open Protocol

  type t =
    | PublishDiagnostics of PublishDiagnostics.params

  let method_ = function
    | PublishDiagnostics _ -> "textDocument/publishDiagnostics"

  let params_to_yojson = function
    | PublishDiagnostics params -> PublishDiagnostics.params_to_yojson params

end

let send_notification rpc callback =
  push rpc
    (fun state ->
      let open Utils.Result.Infix in
      callback state >>= fun notif ->
        match notif with
        | None -> Ok state
        | Some notif ->
          let method_ = Server_notification.method_ notif in
          let params = Server_notification.params_to_yojson notif in
          let packet = {Packet.id = None; method_; params} in
          Inspector.log Recv Inspector.packet packet;
          send rpc (Packet.to_yojson packet);
          Ok state
    )

module Client_notification = struct
  open Protocol

  type t =
    | TextDocumentDidOpen of DidOpen.params
    | TextDocumentDidChange of DidChange.params
    | Initialized
    | Exit
    | UnknownNotification of string * Yojson.Safe.json
end

module Request = struct
  open Protocol

  type _ t =
    | Shutdown : unit t
    | TextDocumentHover : Hover.params -> Hover.result t
    | TextDocumentDefinition : Definition.params -> Definition.result t
    | TextDocumentTypeDefinition : TypeDefinition.params -> TypeDefinition.result t
    | TextDocumentCompletion : Completion.params -> Completion.result t
    | TextDocumentCodeLens : CodeLens.params -> CodeLens.result t
    | TextDocumentRename: Rename.params -> Rename.result t
    | DocumentSymbol : DocumentSymbol.params -> DocumentSymbol.result t
    | DebugEcho : DebugEcho.params -> DebugEcho.result t
    | DebugTextDocumentGet : DebugTextDocumentGet.params -> DebugTextDocumentGet.result t
    | TextDocumentReferences : References.params -> References.result t
    | TextDocumentHighlight : TextDocumentHighlight.params -> TextDocumentHighlight.result t
    | UnknownRequest : string * Yojson.Safe.json -> unit t

  let request_result_to_response (type a) id (req : a t) (result : a) =
    match req, result with
    | Shutdown, _resp -> None
    | TextDocumentHover _, result ->
      let json = Hover.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentDefinition _, result ->
      let json = Definition.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentTypeDefinition _, result ->
      let json = TypeDefinition.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentCompletion _, result ->
      let json = Completion.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentCodeLens _, result ->
      let json = CodeLens.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentRename _, result ->
      let json = Rename.result_to_yojson result in
      Some (Response.make id json)
    | DocumentSymbol _, result ->
      let json = DocumentSymbol.result_to_yojson result in
      Some (Response.make id json)
    | DebugEcho _, result ->
      let json = DebugEcho.result_to_yojson result in
      Some (Response.make id json)
    | DebugTextDocumentGet _, result ->
      let json = DebugTextDocumentGet.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentReferences _, result ->
      let json = References.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentHighlight _, result ->
      let json = TextDocumentHighlight.result_to_yojson result in
      Some (Response.make id json)
    | UnknownRequest _, _resp -> None
end

module Message = struct
  open Protocol

  module Cancel = struct
    type params = {
      id: int;
    } [@@deriving yojson { strict = false }]
  end

  type t =
    | Idle: t
    | Initialize : int * Protocol.Initialize.params -> t
    | Request : int * 'result Request.t -> t
    | Cancel_request : Cancel.params -> t
    | Client_notification : Client_notification.t -> t

  let parse packet =
    let open Utils.Result.Infix in
    match packet.Packet.id with
    | Some id ->
      begin match packet.method_ with
      | "initialize" ->
        Protocol.Initialize.params_of_yojson packet.params >>= fun params ->
        Ok (Initialize (id, params))
      | "shutdown" ->
        Ok (Request (id, Shutdown))
      | "textDocument/completion" ->
        Completion.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentCompletion params))
      | "textDocument/documentSymbol" ->
        DocumentSymbol.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, DocumentSymbol params))
      | "textDocument/hover" ->
        Hover.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentHover params))
      | "textDocument/definition" ->
        Definition.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentDefinition params))
      | "textDocument/typeDefinition" ->
        TypeDefinition.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentTypeDefinition params))
      | "textDocument/references" ->
        References.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentReferences params))
      | "textDocument/codeLens" ->
        CodeLens.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentCodeLens params))
      | "textDocument/documentHighlight" ->
        TextDocumentHighlight.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentHighlight params))
      | "textDocument/rename" ->
        Rename.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentRename params))
      | "debug/echo" ->
        DebugEcho.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, DebugEcho params))
      | "debug/textDocument/get" ->
        DebugTextDocumentGet.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, DebugTextDocumentGet params))
      | name ->
        Ok (Request (id, UnknownRequest (name, packet.params)))
      end
    | None ->
      begin match packet.method_ with
      | "textDocument/didOpen" ->
        DidOpen.params_of_yojson packet.params >>= fun params ->
        Ok (Client_notification (TextDocumentDidOpen params))
      | "textDocument/didChange" ->
        DidChange.params_of_yojson packet.params >>= fun params ->
        Ok (Client_notification (TextDocumentDidChange params))
      | "exit" ->
        Ok (Client_notification Exit)
      | "initialized" ->
        Ok (Client_notification Initialized)
      | "$/cancelRequest" ->
        Cancel.params_of_yojson packet.params >>= fun params ->
          Ok (Cancel_request params)
      | _ ->
        Ok (Client_notification (UnknownNotification (packet.method_, packet.params)))
      end

    let parse packet =
      Inspector.log Send Inspector.packet packet;
      parse packet

end

type 'state handler = {
  on_initialize :
    'state t
    -> Protocol.Initialize.params
    -> 'state
    -> ('state * Protocol.Initialize.result, string) result;

  on_request :
    'res.
    'state t
    -> Protocol.Initialize.client_capabilities
    -> 'res Request.t
    -> 'state
    -> ('state * 'res, string) result;

  on_notification :
    'state t
    -> Client_notification.t
    -> 'state
    -> ('state, string) result
}

let start init_state handler ic oc =
  let open Utils.Result.Infix in

  let read_message rpc =
    match read rpc with
    | None -> Ok (Message.Idle)
    | Some packet -> packet >>= Message.parse
  in

  let handle_message prev_state f =
    let next_state = f () in
    match next_state with
    | Ok next_state -> next_state
    | Error msg ->
      log ~title:"error" "%s" msg;
      prev_state
  in

  let rec loop rpc state =
    while not @@ Messages.Writer.flush ~timeout:0.0 rpc.output do () done;
    match rpc.state with
    | Closed -> ()
    | Ready ->
      
      let next_state =
        handle_message state (fun () ->
          read_message rpc >>= function
          | Message.Idle -> Ok state
          | Message.Initialize (id, params) ->
            handler.on_initialize rpc params state >>= fun (next_state, result) ->
            let json = Protocol.Initialize.result_to_yojson result in
            let response = Response.make id json in
            rpc.state <- Initialized params.client_capabilities;
            send_response rpc response;
            Ok next_state

          | Message.Client_notification Exit ->
            rpc.state <- Closed;
            Ok state
          | Message.Client_notification _ ->
            (* we drop all notifications per protocol before we initialized *)
            Ok state
          | Message.Cancel_request _ ->
            Ok state
          | Message.Request (id, _) ->
            (* we response with -32002 per protocol before we initialized  *)
            let response = Response.make_error id (-32002) "not initialized" in
            send_response rpc response;
            Ok state
        )
      in
      Logger.log_flush ();
      loop rpc next_state
    | Initialized client_capabilities ->
      let next_state =
        handle_message state (fun () ->
          read_message rpc >>= function
          | Message.Idle ->
            next rpc state
          | Message.Initialize _ ->
            errorf "received another initialize request"
          | Message.Client_notification (Exit as notif) ->
            rpc.state <- Closed;
            handler.on_notification rpc notif state
          | Message.Client_notification notif ->
            push rpc (handler.on_notification rpc notif);
            Ok state
          | Message.Cancel_request {id} ->
            begin match Hashtbl.find_opt rpc.pending_requests id with
            | None -> ()
            | Some r -> r := true
            end;
            Ok state
          | Message.Request (id, req) ->
            let cancelled = ref false in
            Hashtbl.add rpc.pending_requests id cancelled;
            push rpc
              (fun state ->
                Hashtbl.remove rpc.pending_requests id;
                if !cancelled then begin
                   let response = Response.make_error id (-32800) "request cancelled" in
                   send_response rpc response;
                   Ok state
                end else
                  handler.on_request rpc client_capabilities req state >>= fun (next_state, result) ->
                    begin match Request.request_result_to_response id req result with
                    | None ->
                      Ok next_state
                    | Some response ->
                      send_response rpc response;
                      Ok next_state
                    end
              );
            Ok state
        )
      in
      Logger.log_flush ();
      loop rpc next_state
  in

  set_binary_mode_in ic true;
  set_binary_mode_out oc true;
  let rpc = { input = Messages.Reader.from_channel ic; output = Messages.Writer.from_channel oc; state = Ready; pending_actions = (Queue.create ()); pending_requests = Hashtbl.create 17 } in
  loop rpc init_state

let stop (rpc : _ t) = rpc.state <- Closed
