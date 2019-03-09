exception ConnectionLost of string
exception LowLevelProtocolError of string

module AsyncInput = struct

  type t =
    {
      read: timeout:float -> int -> string option;
      read_line: timeout:float -> string option;
    }

  let from_channel ic =
    let first = ref 0 in
    let checked = ref 0 in
    let last = ref 0 in
    let buffer = ref (Bytes.make 1 '\x00') in
    let fd = Unix.descr_of_in_channel ic in
    let check_resize () =
      let length = Bytes.length !buffer in
      if !last = length then begin
        buffer := Bytes.extend !buffer (- !first) (if 2 * !first > length then !first else length + !first);
        last := !last - !first;
        checked := !checked - !first;
        first := 0;
      end
    in
    let rec unsafe_read_until ~timeout until =
      let open Unix in
      match until !buffer !first !checked !last with
      | -1 ->
        checked := !last;
        begin match select [fd] [] [] timeout with
        | [ _ ], [], [] ->
          check_resize ();
          let buffer_len = Bytes.length !buffer in
          let read_len = Unix.read fd !buffer !last (buffer_len - !last) in
          last := !last + read_len;
          if read_len = 0 then
            raise (ConnectionLost "End of file")
          else
            unsafe_read_until ~timeout until
        | _ -> None
        end
      | shift ->
        let result = Bytes.sub_string !buffer !first (shift - !first) in
        first := shift;
        checked := shift;
        Some result
    in
    let safe_read_until ~timeout until =
      try
        unsafe_read_until ~timeout until
      with Unix.Unix_error (err,_, _) -> 
        raise (ConnectionLost (Unix.error_message err))
    in
    let read_line ~timeout =
      safe_read_until ~timeout (fun buffer _ from last ->
        let k = ref from in
        while !k < last && Bytes.get buffer !k <> '\n' do
          incr k
        done;
        if !k = last then -1 else !k + 1
      )
    in
    let read ~timeout k =
      safe_read_until ~timeout (fun _ first _ last -> if last - first >= k then first + k else -1)
    in
    {
      read;
      read_line;
    }

end

module Reader = struct

  type partial_header =  {
    content_length: int option;
    content_type: string option;
  }

  type header = {
    content_length: int;
    content_type: string;
  }

  type state =
    | Partial of string list
    | Done of header

  type t = {
      input: AsyncInput.t; 
      mutable state: state;
  }

  let from_channel ic =
    {
      input = AsyncInput.from_channel ic; 
      state = Partial []; 
    } 
    
  let content_length_prefix = "Content-Length: "
  let content_type_prefix = "Content-Type: "

  let string_starts p s =
    let exception Stop in
    try
      String.iteri
        (fun j c ->
          if s.[j] <> c then
            raise Stop
        ) p;
      true
    with Stop -> false
 
  let parse_line prefix l =
    let prefix_len = String.length prefix in
    let len = String.length l in 
    if len >= (prefix_len + 2) && l.[len - 1] = '\n' && l.[len-2] = '\r' && string_starts prefix l then  
      Some (String.sub l prefix_len (len - prefix_len - 2))
    else 
      None 
  
  let parse_lines lines = 
    let rec loop : partial_header -> string list -> header = fun acc -> function 
      | [] -> 
        begin match acc with 
        | { content_type = Some content_type; content_length = Some content_length } -> 
          {content_type; content_length}
        | { content_length = None; _ } ->
          raise (LowLevelProtocolError "Missing Content-Length")
        | { content_type = None; content_length = Some content_length } ->
          {content_type = "utf-8"; content_length}
        end
      | line :: lines ->
        let fail () = 
          raise (LowLevelProtocolError (Printf.sprintf "Cannot parse line %S" line))
        in 
        let acc = 
          match parse_line content_length_prefix line with 
          | Some length ->  
            begin match int_of_string_opt length with 
            | None -> fail ()
            | (Some _) as content_length -> {acc with content_length}
            end
          | None -> 
             begin match parse_line content_type_prefix line with
             | Some "utf-8"
             | Some "utf8" -> {acc with content_type = Some "utf-8"}
             | _ -> fail ()
             end
        in
        loop acc lines
    in 
    loop {content_type = None; content_length = None} lines

  let rec read ~timeout ({input; state} as reader) =
    match state with
    | Partial lines ->
        begin match input.read_line ~timeout with
        | None -> None
        | Some "\r\n" -> 
           reader.state <- Done (parse_lines lines);
           read ~timeout reader
        | Some line -> 
            reader.state <- Partial (line :: lines);
            read ~timeout reader
        end
    | Done header ->
        match input.read ~timeout header.content_length with
        | None -> None
        | Some content ->
          reader.state <- Partial [];
          Some content             

end

module AsyncOutput = struct

  type t =
    {
      write: string -> unit;
      flush: timeout:float -> bool;
    }

  let rec double_length length n =
    if n < length then 
      length 
    else 
      double_length (2 * length) n 

  let from_channel oc =
    let first = ref 0 in
    let last = ref 0 in
    let buffer = ref (Bytes.make 1024 '\x00') in
    let fd = Unix.descr_of_out_channel oc in
    let write content =
      let content_len = String.length content in 
      let buffer_len = Bytes.length !buffer in
      let needed_space = !last + content_len - buffer_len in
      if needed_space > 0 then begin
        let new_space = 
          if 2 * needed_space < !first then 
            !first
          else
            !first + double_length buffer_len (needed_space - !first) 
        in
        buffer := Bytes.extend !buffer (- !first) new_space;
        last := !last - !first;
        first := 0;
      end;
      Bytes.blit_string content 0 !buffer !last content_len;
      last := !last + content_len
    in
    let rec unsafe_flush ~timeout =
      let open Unix in
      if !first < !last then begin
        match select [] [fd] [] timeout with
        | [], [ _ ], [] ->
          let write_len = Unix.write fd !buffer !first (!last - !first) in
          first := !first + write_len;
          if write_len = 0 then
            raise (ConnectionLost "End of file")
          else
            unsafe_flush ~timeout
        | _ -> false
      end else 
        true
    in
    let flush ~timeout = 
      try
        unsafe_flush ~timeout
      with Unix.Unix_error (err,_, _) -> 
        raise (ConnectionLost (Unix.error_message err))
    in 
    {
      write;
      flush;
    }

end

module Writer = struct
  type t = {
    output: AsyncOutput.t; 
  }

  let from_channel oc = { output = AsyncOutput.from_channel oc }

  let write {output} data =
    let open AsyncOutput in
    let length = String.length data in
    output.write "Content-Length: "; 
    output.write (string_of_int length);
    output.write "\r\n\r\n";
    output.write data

  let flush ~timeout {output} =
    output.flush ~timeout
     
end