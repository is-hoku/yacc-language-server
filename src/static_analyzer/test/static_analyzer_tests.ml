open Static_analyzer

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
      (match
         Parser.entry
           (Syntax.Incremental.input lexbuf.Lexing.lex_curr_p)
           Lexer.read_token lexbuf
       with
      | Result.Ok ast -> print_string (Types.show ast)
      | Result.Error err -> raise err);
      close_in fin)
    (Sys.readdir dir)

let () = test "./testcase"
