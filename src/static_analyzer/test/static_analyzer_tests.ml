open Static_analyzer

let test dir =
  Array.iter
    (fun file ->
      let fname = Filename.concat dir file in
      print_endline (Printf.sprintf "\n\ntest file: %s" fname);
      let fin = open_in fname in
      Lexer.initialize ();
      let lexbuf = Lexing.from_channel fin in
      let () =
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname }
      in
      (match
         Syntax.entry
           (Parser.Incremental.input lexbuf.Lexing.lex_curr_p)
           Lexer.read_token lexbuf
       with
      | Result.Ok ast -> print_string (Types.show ast)
      | Result.Error err -> raise err);
      close_in fin)
    (Sys.readdir dir)

let () = test "./testcase"
