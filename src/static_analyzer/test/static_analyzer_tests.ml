open Static_analyzer

let parse buf = Syntax.input Lexer.read_token buf |> Types.show |> print_endline

let test dir =
  Array.iter
    (fun file ->
      let fin = open_in (Filename.concat dir file) in
      let buf = Lexing.from_channel fin in
      parse buf;
      close_in fin)
    (Sys.readdir dir)

let () = test "./testcase"
