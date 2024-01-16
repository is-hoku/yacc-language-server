open Static_analyzer

let _parse buf =
  Syntax.input Lexer.read_token buf |> Types.show |> print_endline

let test dir =
  Array.iter
    (fun file ->
      let fname = Filename.concat dir file in
      print_endline (Printf.sprintf "test file: %s" fname);
      let fin = open_in fname in
      let buf = Lexing.from_channel fin in
      Parser.process fname buf |> Types.show |> print_string;
      close_in fin)
    (Sys.readdir dir)

let () = test "./testcase"
