open Static_analyzer

let rec tokenize buf =
  match Lexer.read_token buf with
  | Syntax.EOF -> print_string "eof"
  | _ -> tokenize buf

let lex dir =
  Array.iter
    (fun file ->
      let fin = open_in (Filename.concat dir file) in
      let buf = Lexing.from_channel fin in
      tokenize buf)
    (Sys.readdir dir)

let () = lex "./testcase"
