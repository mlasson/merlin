open Std

(* Poor man's test framework *)
type name = string

type test =
  | Single of name * (unit -> unit)
  | Group of name * test list

let test name f = Single (name, f)

let group name tests = Group (name, tests)

exception Detail of exn * string
let () = Printexc.register_printer (function
    | (Detail (exn, msg)) ->
      Some (Printexc.to_string exn ^ "\nAdditional information:\n" ^ msg)
    | _ -> None
  )

let str_match ~re str =
  Str.string_match (Str.regexp (re ^ "$")) str 0

(* Setting up merlin *)
module M = Mpipeline

let process ?(with_config=fun x -> x) ?for_completion filename text =
  let config = with_config Mconfig.initial in
  let config = Mconfig.({config with query = {config.query with filename}}) in
  let source = Msource.make Trace.null config text in
  let pipeline = M.make Trace.null config source in
  match for_completion with
  | None -> pipeline
  | Some pos -> M.for_completion pos pipeline

(* All tests *)

let assert_errors ?with_config
    filename ?(lexer=0) ?(parser=0) ?(typer=0) ?(config=0) source =
  test filename (fun () ->
      let m = process ?with_config filename source in
      let lexer_errors  = M.reader_lexer_errors m in
      let parser_errors = M.reader_parser_errors m in
      let failures, typer_errors  =
        Mtyper.with_typer (M.typer_result m) @@ fun () ->
        Mconfig.((M.final_config m).merlin.failures),
        M.typer_errors m
      in
      let fmt_msg exn =
        match Location.error_of_exn exn with
        | None | Some `Already_displayed -> Printexc.to_string exn
        | Some (`Ok err) -> err.Location.msg
      in
      let expect ~count str errors =
        let count' = List.length errors in
        if count <> count' then failwith (
            "expecting " ^ string_of_int count ^ " " ^ str ^ " but got " ^
            string_of_int count' ^ " errors\n" ^
            String.concat "\n- " ("Errors: " :: List.map_end fmt_msg
                                    (lexer_errors @ parser_errors @ typer_errors)
                                    failures)
          )
      in
      expect ~count:lexer "lexer errors" lexer_errors;
      expect ~count:parser "parser errors" parser_errors;
      expect ~count:typer "typer errors" typer_errors;
      expect ~count:config "configuration failures" failures;
    )

let assertf b fmt =
  if b then
    Printf.ikfprintf ignore () fmt
  else
    Printf.ksprintf failwith fmt

let validate_output ?with_config filename source query pred =
  test filename (fun () ->
      let pipeline = process ?with_config filename source in
      let result = Query_commands.dispatch pipeline query in
      try pred result
      with exn ->
        let info = `Assoc [
            "query", Query_json.dump query;
            "result", Query_json.json_of_response query result;
          ] in
        raise (Detail (exn, Json.pretty_to_string info))
    )

(* FIXME: this sucks. improve. *)
let validate_failure ?with_config filename source query pred =
  test filename (fun () ->
      let pipeline = process ?with_config filename source in
      let for_info, wrapped =
        match Query_commands.dispatch pipeline query with
        | exception e -> ("failure", `String (Printexc.to_string e)), `Error e
        | res -> ("result", Query_json.json_of_response query res), `Ok res
      in
      try pred wrapped
      with exn ->
        let info = `Assoc [ "query", Query_json.dump query; for_info ] in
        raise (Detail (exn, Json.pretty_to_string info))
    )

let tests = [

  group "completion" (
    let assert_entry name result query =
      let open Query_protocol.Compl in
      assertf (List.exists ~f:(fun x -> x.name = name) result.entries)
        "expected %S in %s" name query;
    in
    [
      validate_output "expansion.ml"
        "let x = L.m"
        (Query_protocol.Expand_prefix ("L.m", `Logical(1,11), [], false))
        (fun result ->
           assert_entry "List.map" result "L.m expansion";
           assert_entry "ListLabels.map" result "L.m expansion");
      validate_output "expansion2.ml"
        "let x = Lsi.m"
        (Query_protocol.Expand_prefix ("Lsi.m", `Logical(1,11), [], false))
        (fun result ->
           assert_entry "List.map" result "Lsi.m expansion";
           assert_entry "ListLabels.map" result "Lsi.m expansion";
        )
    ]

  );

  group "misc" (
    [
      assert_errors "relaxed_external.ml"
        "external test : unit = \"bs\"";

      validate_output "occurrences.ml"
        "let foo _ = ()\nlet () = foo 4\n"
        (Query_protocol.Occurrences (`Ident_at (`Offset 5)))
        (fun locations ->
           assertf (List.length locations = 2) "expected two locations");
    ]
  );

  group "type-expr" (

    let test_file =
      "let x = 5\n\
       let y = 10\n
       type t = T\n\
       module M = List\n\
       module type MT = module type of List\n\
       let z = ()"
    in
    let unbound_pattern = `Start, "\\(Error: \\)?Unbound .*" in
    let queries = [
      "lident-value"       , "y"     , [unbound_pattern; `End, "int"];
      "lident-type"        , "t"     , [unbound_pattern; `End, "type t = T"];
      "expr"               , "x + y" , [unbound_pattern; `End, "int"];
      "uident-constructor" , "T"     , [unbound_pattern; `End, "t"];
      "uident-module"      , "M"     , [unbound_pattern; `End, "(module List)"];
      "uident-module-type" , "MT"    , [unbound_pattern; `End, "sig\\(.\\|\n\\)*end"];
      "parse-error"        , "f ("   , [`Start, "FIXME"];
    ] in
    let type_expr_match name expr pos re =
      validate_output name test_file (Query_protocol.Type_expr (expr, pos))
        (fun str -> assertf (str_match ~re str)
            "Output didn't match pattern %S: %S" re str)
    in
    List.concat_map queries ~f:(fun (name, expr, patterns) ->
        List.map patterns ~f:(fun (pos, re) ->
            type_expr_match (name ^ "-" ^ Msource.print_position () pos) expr pos re))
  );

  group "motion" (
    let check_position (l1,c1 as p1) pos =
      let l2,c2 as p2 = Lexing.split_pos pos in
      assertf (p1=p2) "Expecting to move to %d:%d, moved to %d:%d" l1 c1 l2 c2
    in
    let check_jump_position a b = match a, b with
      | None, `Error _ -> ()
      | None, `Found pos ->
        let l, c = Lexing.split_pos pos in
        assertf false "Expected to fail, moved to %d:%d" l c
      | Some (l, c), `Error msg ->
        assertf false
          "Expected to move to %d:%d, but failed with message %S" l c msg
      | Some pos1, `Found pos2 -> check_position pos1 pos2
    in
    let jump ?with_config ~from ?result feature source id =
      let name = match result with
        | Some (l,c) -> sprintf "jump_%d_to_%s_at_%d_%d.ml" id feature l c
        | None -> sprintf "jump_%d_to_%s_fail.ml" id feature
      in
      validate_output ?with_config name source
        (Query_protocol.Jump (feature, `Logical from))
        (check_jump_position result)
    in
    let phrase ?with_config ~from ~result direction source id =
      let (l,c) = result in
      let name = sprintf "phrase_%s_%d_from_%d_%d.ml"
          (if direction = `Next then "next" else "prev") id l c in
      validate_output ?with_config name source
        (Query_protocol.Phrase (direction, `Logical from))
        (check_position result)
    in
    List.mapi ~f:(fun i f -> f i) [
      jump "let" ~from:(2,2) ~result:(1,0) "let x =\n  5";
      jump "let" ~from:(1,8) "let x = 5"; (*Same line should fail*)
      jump "module" ~from:(2,2) "let x = \n 5";
      phrase `Next ~from:(1,0) "let x = 5\nlet y = 2" ~result:(2,0);
      phrase `Prev ~from:(2,0) "let x = 5\nlet y = 2" ~result:(1,0);
    ]
  );

  group "std" [

    group "glob" (
      let glob_match ~pattern str =
        Glob.match_pattern (Glob.compile_pattern pattern) str in
      let should_match name ~pattern str =
        test name (fun () -> assertf (glob_match ~pattern str)
                      "pattern %S should match %S" pattern str)
      and shouldn't_match name ~pattern str =
        test name (fun () -> assertf (not (glob_match ~pattern str))
                      "pattern %S shouldn't match %S" pattern str)
      in
      [
        should_match "empty" ~pattern:"" "";
        shouldn't_match "not-empty" ~pattern:"" "x";
        should_match "litteral" ~pattern:"x" "x";
        shouldn't_match "not-litteral" ~pattern:"x" "y";
        should_match "skip" ~pattern:"x?z" "xyz";
        shouldn't_match "not-skip" ~pattern:"x?yz" "xyz";
        should_match "joker1" ~pattern:"x*" "xyz";
        shouldn't_match "not-joker1" ~pattern:"y*" "xyz";
        should_match "joker2" ~pattern:"xy*xy*" "xyzxyz";
        shouldn't_match "not-joker2" ~pattern:"xy*yz*" "xyzyxz";
        should_match "joker3" ~pattern:"*bar*" "foobarbaz";
      ]
    );

    group "shell" (
      let string_list = function
        | [] -> "[]"
        | comps ->
          let comps = List.map ~f:String.escaped comps in
          "[\"" ^ String.concat ~sep:"\";\"" comps ^ "\"]"
      in
      let assert_split i (str, expected) =
        test ("split_command-" ^ string_of_int i) @@ fun () ->
        let result = Shell.split_command str in
        assertf (result = expected)
          "Shell.split_command %S = %s, expecting %s"
          str (string_list result) (string_list expected)
      in
      List.mapi ~f:assert_split [
        "a b c"     , ["a";"b";"c"];
        "a'b'c"     , ["abc"];
        "a 'b c'"   , ["a"; "b c"];
        "a\"b'c\""  , ["ab'c"];
        "a\\\"b'c'" , ["a\"bc"];
      ]
    );
  ];
]

(* Driver *)

let passed = ref 0
let failed = ref 0

let rec run_tests indent = function
  | [] -> ()
  | x :: xs ->
    run_test indent x;
    run_tests indent xs

and run_test indent = function
  | Single (name, f) ->
    Printf.printf "%s%s:\t%!" indent name;
    begin match f () with
      | () ->
        incr passed;
        Printf.printf "OK\n%!"
      | exception exn ->
        let bt = Printexc.get_backtrace () in
        incr failed;
        Printf.printf "KO\n%!";
        Printf.eprintf "%sTest %s failed with exception:\n%s%s\n%!"
          indent name
          indent
          (match exn with
           | Failure str -> str
           | exn -> Printexc.to_string exn);
        begin match Location.error_of_exn exn with
          | None | Some `Already_displayed -> ()
          | Some (`Ok {Location. msg; loc}) ->
            Printf.eprintf "%sError message:\n%s\n%!" indent msg
        end;
        Printf.eprintf "%sBacktrace:\n%s\n%!" indent bt
    end
  | Group (name, tests) ->
    Printf.printf "%s-> %s\n" indent name;
    run_tests (indent ^ "  ") tests

let () =
  Printexc.record_backtrace true;
  run_tests "  " tests;
  Printf.printf "Passed %d, failed %d\n" !passed !failed;
  if !failed > 0 then exit 1
