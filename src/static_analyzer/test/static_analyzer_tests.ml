open Static_analyzer

let _parse buf =
  Syntax.input Lexer.read_token buf |> Types.show |> print_endline

let test dir =
  Array.iter
    (fun file ->
      let fname = Filename.concat dir file in
      print_endline (Printf.sprintf "test file: %s" fname);
      let fin = open_in fname in
      let lexbuf = Lexing.from_channel fin in
      let () =
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname }
      in
      Parser.entry
        (Syntax.Incremental.input lexbuf.Lexing.lex_curr_p)
        Lexer.read_token lexbuf
      |> Types.show |> print_string;
      close_in fin)
    (Sys.readdir dir)

let () = test "./testcase"
