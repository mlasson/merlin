(**
 * This encodes LSP RPC state machine.
 *)

module Server_notification : sig
  open Protocol

  type t = 
    | PublishDiagnostics of PublishDiagnostics.publishDiagnosticsParams
end

module Client_notification : sig
  open Protocol

  type t =
    | TextDocumentDidOpen of DidOpen.params
    | TextDocumentDidChange of DidChange.params
    | Initialized
    | Exit
    | UnknownNotification of string * Yojson.Safe.json
end

module Request : sig
  open Protocol

  type _ t =
    | Shutdown : unit t
    | TextDocumentHover : Hover.params -> Hover.result t
    | TextDocumentDefinition : Definition.params -> Definition.result t
    | TextDocumentTypeDefinition : TypeDefinition.params -> TypeDefinition.result t
    | TextDocumentCompletion : Completion.params -> Completion.result t
    | DocumentSymbol : DocumentSymbol.params -> DocumentSymbol.result t
    | DebugEcho : DebugEcho.params -> DebugEcho.result t
    | DebugTextDocumentGet : DebugTextDocumentGet.params -> DebugTextDocumentGet.result t
    | UnknownRequest : string * Yojson.Safe.json -> unit t
end

type 'state t

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

val start :
  'state
  -> 'state handler
  -> in_channel
  -> out_channel
  -> unit

val stop : 'a t -> unit

val send_notification : 'a t -> Server_notification.t -> unit
