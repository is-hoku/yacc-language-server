{
open Lexing
open Syntax
open Language_server.Import

exception Lex_error of position * string

type section =
    | Prologue
    | BisonDecls
    | GrammarRules
    | Epilogue
    | Code

let initial_stack =
    let stack = Stack.create () in
    Stack.push BisonDecls stack;
    stack

let get_range lexbuf =
    let start_p = lexeme_start_p lexbuf in
    let end_p = lexeme_end_p lexbuf in
    let start = Position.create ~character:(start_p.pos_cnum - start_p.pos_bol) ~line:start_p.pos_lnum in
    let end_ = Position.create ~character:(end_p.pos_cnum - end_p.pos_bol) ~line:end_p.pos_lnum in
    Range.create ~end_ ~start

let report_error msg lexbuf =
    let pos = lexeme_start_p lexbuf in
    raise (Lex_error (pos, msg))

let pop stack msg lexbuf =
    if Stack.length stack > 0 then
        Stack.pop stack |> ignore
    else
        report_error msg lexbuf
}

let white = [' ' '\t']+
let small = ['a'-'z']
let capital = ['A'-'Z']
let digit = ['0'-'9']
let symbol = ((small | capital | '_' | '.')+ (small | capital | digit | '_' | '.' | '-')*)
let hex = (['a'-'f'] | ['A'-'F'] | digit)+
let directive = (small (small | '-')*)

rule prologue stack = parse
| white { prologue stack lexbuf }
| '\n' { new_line lexbuf; prologue stack lexbuf }
| "/*" { comment stack lexbuf; prologue stack lexbuf }
| "*/" { report_error "expected character /*" lexbuf }
| "//" { comment_line stack lexbuf; prologue stack lexbuf }
| '"' { ignore_string stack lexbuf; prologue stack lexbuf }
| "%}" { pop stack "too many %}" lexbuf; Stack.push BisonDecls stack; RPROLOGUE(get_range lexbuf) }
| eof { report_error "unexpected end of file in a prologue section" lexbuf }

and bison_decls stack = parse
| white { prologue stack lexbuf }
| '\n' { new_line lexbuf; prologue stack lexbuf }
| "/*" { comment stack lexbuf; bison_decls stack lexbuf }
| "*/" { report_error "expected character /*" lexbuf }
| "//" { comment_line stack lexbuf; bison_decls stack lexbuf }
| "%}" { RPROLOGUE(get_range lexbuf) }
| "%{" { pop stack "too many %{" lexbuf; Stack.push Prologue stack; LPROLOGUE(get_range lexbuf) }
| "%%" { pop stack "too many %%" lexbuf; Stack.push GrammarRules stack; DELIMITER(get_range lexbuf) }
| '"' { string_literal stack 0 (Buffer.create 256) lexbuf }
| '%' (directive as d) {
    match d with
    | "require" -> REQUIRE(get_range lexbuf)
    | "union" -> UNION(get_range lexbuf)
    | "token" -> TOKEN(get_range lexbuf)
    | "right" -> RIGHT(get_range lexbuf)
    | "left" -> LEFT(get_range lexbuf)
    | "precedence" -> PRECEDENCE(get_range lexbuf)
    | "nonassoc" -> NONASSOC(get_range lexbuf)
    | "nterm" -> NTERM(get_range lexbuf)
    | "type" -> TYPE(get_range lexbuf)
    | "initial-action" -> INITIALACTION(get_range lexbuf)
    | "destructor" -> DESTRUCTOR(get_range lexbuf)
    | "printer" -> PRINTER(get_range lexbuf)
    | "expect" -> EXPECT(get_range lexbuf)
    | "expect-rr" -> EXPECTRR(get_range lexbuf)
    | "debug" -> DEBUG(get_range lexbuf)
    | "start" -> START(get_range lexbuf)
    | "code" -> CODE(get_range lexbuf)
    | "define" -> DEFINE(get_range lexbuf)
    | "defines" -> DEFINES(get_range lexbuf)
    | "file-prefix" -> FILEPREFIX(get_range lexbuf)
    | "header" -> HEADER(get_range lexbuf)
    | "language" -> LANGUAGE(get_range lexbuf)
    | "locations" -> LOCATIONS(get_range lexbuf)
    | "name-prefix" -> NAMEPREFIX(get_range lexbuf)
    | "no-lines" -> NOLINES(get_range lexbuf)
    | "output" -> OUTPUT(get_range lexbuf)
    | "pre-parser" -> PUREPARSER(get_range lexbuf)
    | "skeleton" -> SKELETON(get_range lexbuf)
    | "token-table" -> TOKENTABLE(get_range lexbuf)
    | "verbose" -> VERBOSE(get_range lexbuf)
    | "yacc" -> YACC(get_range lexbuf)
    | _ -> report_error "invalid directive" lexbuf
    }
| "<" { LT(get_range lexbuf) }
| ">" { GT(get_range lexbuf) }
| "*" { ASTERISK(get_range lexbuf) }
| "{" { Stack.push Code stack; LBRACE(get_range lexbuf) }
| "}" { RBRACE(get_range lexbuf) }
| "." { DOT(get_range lexbuf) }
| digit+ { DECIMAL ((get_range lexbuf), (int_of_string (lexeme lexbuf))) }
| ("0x" | "0X") hex+ { HEXADECIMAL ((get_range lexbuf), int_of_string (lexeme lexbuf))}
| symbol { SYMBOL ((get_range lexbuf), (lexeme lexbuf)) }
| _ { report_error (Printf.sprintf "unexpected character %s" (lexeme lexbuf)) lexbuf }
| eof { EOF(get_range lexbuf) }

and grammar_rules stack = parse
| white { grammar_rules stack lexbuf }
| '\n' { new_line lexbuf; prologue stack lexbuf }
| "/*" { comment stack lexbuf; grammar_rules stack lexbuf }
| "*/" { report_error "expected character /*" lexbuf }
| "//" { comment_line stack lexbuf; grammar_rules stack lexbuf }
| "%%" { pop stack "too many %%" lexbuf; Stack.push Epilogue stack; DELIMITER(get_range lexbuf) }
| "{" { Stack.push Code stack; LBRACE(get_range lexbuf) }
| "}" { RBRACE(get_range lexbuf) }
| "[" { LBRACK(get_range lexbuf) }
| "]" { RBRACK(get_range lexbuf) }
| ":" { COLON(get_range lexbuf) }
| ";" { SEMICOLON(get_range lexbuf) }
| "|" { PIPE(get_range lexbuf) }
| digit+ { DECIMAL ((get_range lexbuf), (int_of_string (lexeme lexbuf))) }
| ("0x" | "0X") hex+ { HEXADECIMAL ((get_range lexbuf), (int_of_string (lexeme lexbuf))) }
| symbol { SYMBOL ((get_range lexbuf), (lexeme lexbuf)) }
| _ { report_error (Printf.sprintf "unexpected character %s" (lexeme lexbuf)) lexbuf }
| eof { EOF(get_range lexbuf) }

and comment stack = parse
| "*/" { () }
| _ { comment stack lexbuf }
| eof { report_error "missing terminating */ character" lexbuf }

and comment_line stack = parse
| "\n" { new_line lexbuf }
| _ { comment_line stack lexbuf }

and string_literal stack length buf = parse
| '"' { if length >= 2 then STRING ((get_range lexbuf), (Buffer.contents buf)) else report_error "a literal string token must contain two or more characters" lexbuf }
| '\\' 'a' { (* OCaml does not support '\a' so ignore input *) string_literal stack (length + 1) buf lexbuf }
| '\\' 'b' { Buffer.add_char buf '\b'; string_literal stack (length + 1) buf lexbuf }
| '\\' 'f' { (* OCaml does not support '\f' so ignore input *) string_literal stack (length + 1) buf lexbuf }
| '\\' 'n' { Buffer.add_char buf '\n'; string_literal stack (length + 1) buf lexbuf }
| '\\' 'r' { Buffer.add_char buf '\r'; string_literal stack (length + 1) buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; string_literal stack (length + 1) buf lexbuf }
| '\\' 'v' { (* OCaml does not support '\v' so ignore input *) string_literal stack (length + 1) buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; string_literal stack (length + 1) buf lexbuf }
| '\\' '\'' { Buffer.add_char buf '\''; string_literal stack (length + 1) buf lexbuf }
| '\\' '"' { Buffer.add_char buf '"'; string_literal stack (length + 1) buf lexbuf }
| '\\' '?' { Buffer.add_char buf '?'; string_literal stack (length + 1) buf lexbuf }
(*
| '\\' 'x' hex+ { read_hex hex lexbuf; string_literal stack (length + 1) buf lexbuf }
| '\\' 'u' hex0 hex1 hex2 hex3 { read_unicode_4digits stack hex0 hex1 hex2 hex3 lexbuf; string_literal stack (length + 1) buf lexbuf }
| '\\' 'U' hex0 hex1 hex2 hex3 hex4 hex5 hex6 hex7 { read_unicode_8digits stack hex0 hex1 hex2 hex3 hex4 hex5 hex6 hex7 lexbuf; string_literal stack (length + 1) buf lexbuf }
| '\\' { read_octal stack buf lexbuf; string_literal stack (length + 1) buf lexbuf }
*)
| ( '!' | '#' | '%' | '&' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '[' | ']' | '^' | '_' | '{' | '|' | '}' | '~' | white | small | capital | digit )+ { Buffer.add_string buf (lexeme lexbuf); string_literal stack (length + 1) buf lexbuf }
| _ { report_error (Printf.sprintf "invalid character %s" (lexeme lexbuf)) lexbuf }
| eof { report_error "missing terminating \" character" lexbuf }


and ignore_string stack = parse
(* TODO: check escape sequences like '\"' *)
| '"' { () }
| _ { ignore_string stack lexbuf }
| eof { report_error "missing terminating \" character" lexbuf }

and epilogue stack = parse
| eof { EOF(get_range lexbuf) }

and code stack = parse
| white { code stack lexbuf }
| '\n' { new_line lexbuf; prologue stack lexbuf }
| "/*" { comment stack lexbuf; code stack lexbuf }
| "*/" { report_error "expected character /*" lexbuf }
| "//" { comment_line stack lexbuf; code stack lexbuf }
| '"' { ignore_string stack lexbuf; code stack lexbuf }
| '@' digit as num { POSITIONAL(get_range lexbuf, int_of_string num) }
| '@' '$' { RESULTPOSITIONAL(get_range lexbuf) }
| '@' '[' symbol as name ']' { NAMEDPOSITIONAL(get_range lexbuf, name) }
| '$' digit as num { SEMANTIC(get_range lexbuf, int_of_string num) }
| '$' '$' { RESULTSEMANTIC(get_range lexbuf) }
| '$' '[' symbol as name ']' { NAMEDSEMANTIC(get_range lexbuf, name) }
| '}' { pop stack "not found return section" lexbuf; RBRACE(get_range lexbuf) }
| eof { report_error "missing terminating } character" lexbuf }

{
let read_token stack lexbuf =
    match Stack.top stack with
    | Prologue -> prologue stack lexbuf
    | BisonDecls -> bison_decls stack lexbuf
    | GrammarRules -> grammar_rules stack lexbuf
    | Epilogue -> epilogue stack lexbuf
    | Code -> code stack lexbuf
}
