{
open Lexing
open Syntax
open Types
open Language_server.Import

exception LexError of string

type state =
    | INITIAL
    | SC_YACC_COMMENT (* A C-like comment in directives/rules. *)
    | SC_ESCAPED_CHARACTER (* Characters and strings in directives/rules. *)
    | SC_ESCAPED_STRING (* Characters and strings in directives/rules. *)
    | SC_ESCAPED_TSTRING (* Characters and strings in directives/rules. *)
    | SC_TAG (* POSIX says that a tag must be both an id and a C union member, but historically almost any character is allowed in a tag.  We disallow NUL, as this simplifies our implementation.  We match angle brackets in nested pairs: several languages use them for generics/template types. *)
    (* Four types of user code *)
    | SC_PROLOGUE (* prologue (code between '%{' '%}' in the first section, before %%) *)
    | SC_BRACED_CODE (* actions, printers, union, etc, (between braced in the middle section) *)
    | SC_EPILOGUE (* epilogue (everything after the second %%). *)
    | SC_PREDICATE (* predicate (code between '%?{' and '{' in middle section) *)
    | SC_BRACKETED_ID (* Bracketed identifiers support. *)
    | SC_COMMENT (* C and C++ comments in code. *)
    | SC_LINE_COMMENT (* C and C++ comments in code. *)
    | SC_CHARACTER (* Strings and characters in code. *)
    | SC_STRING (* Strings and characters in code. *)
[@@deriving show]

let state = ref INITIAL
let bracketed_id_str = ref None
let percent_percent_count = ref 0
let context_state = ref INITIAL
let nesting = ref 0

let begin_ new_state =
    state := new_state

let buf = Buffer.create 256

let complain msg lexbuf =
    let pos = lexeme_start_p lexbuf in
    (* TODO: push errors to a queue for later reporting *)
    print_endline (Printf.sprintf "%s, line %d, character %d" msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol))

let string_grow str =
  Buffer.add_string buf str

let string_finish () =
  let result = Buffer.contents buf in
  Buffer.clear buf;
  result

let string_1grow char_ =
  Buffer.add_char buf char_

let string_grow_escape c lexbuf =
    let valid = (0 < c && c <= 255) in
    if not valid then
        complain (Printf.sprintf "invalid number after \\-escape: %s" (lexeme lexbuf)) lexbuf;
    if !state == SC_ESCAPED_CHARACTER then(
        if valid then
            string_1grow (char_of_int c)
        else
            string_1grow ('?'))
    else
        string_grow (lexeme lexbuf)


let get_range lexbuf =
    let start_p = lexeme_start_p lexbuf in
    let end_p = lexeme_end_p lexbuf in
    let start = Position.create ~character:(start_p.pos_cnum - start_p.pos_bol) ~line:start_p.pos_lnum in
    let end_ = Position.create ~character:(end_p.pos_cnum - end_p.pos_bol) ~line:end_p.pos_lnum in
    Range.create ~end_ ~start

let unexpected_eof c lexbuf =
    complain (Printf.sprintf "missing %s at end of file" c) lexbuf

let unexpected_newline c lexbuf =
    complain (Printf.sprintf "missing %s at end of line" c) lexbuf;
    new_line lexbuf

let deprecated_directive d lexbuf =
    complain (Printf.sprintf "deprecated directive: %s" d) lexbuf
}

let letter = ['.' 'a'-'z' 'A'-'Z' '_']
let id = letter (letter | ['-' '0'-'9'])*
let int = ['0'-'9']+
let xint = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let eol = '\n' | '\r' '\n'

let char = ['\000'-'\009' '\011'-'\012' '\014'-'\255']
let mbchar =
    ['\x09' '\x0D' '\x20'-'\x7E']
  | ['\xC2'-'\xDF']['\x80'-'\xBF']
  | '\xE0'['\xA0'-'\xBF']['\x80'-'\xBF']
  | ['\xE1'-'\xEC' '\xEE' '\xEF']['\x80'-'\xBF']['\x80'-'\xBF']
  | '\xED'['\x80'-'\x9F']['\x80'-'\xBF']
  | '\xF0'['\x90'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
  | ['\xF1'-'\xF3']['\x80'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
  | '\xF4'['\x80'-'\x8F']['\x80'-'\xBF']['\x80'-'\xBF']

let splice = '\\' [' ' '\t' '\011' '\012']* eol

let sp = [' ' '\t']*
let eqopt = (sp '=')?

rule initial = parse
  | "%binary"                               { PERCENT_NONASSOC }
  | "%code"                                 { PERCENT_CODE }
  | "%debug"                                { PERCENT_FLAG ("parse.trace") }
  | "%default-prec"                         { PERCENT_DEFAULT_PREC }
  | "%define"                               { PERCENT_DEFINE }
  | "%defines"                              { PERCENT_HEADER } (* Deprecated in 3.8. *)
  | "%destructor"                           { PERCENT_DESTRUCTOR }
  | "%dprec"                                { PERCENT_DPREC }
  | "%empty"                                { PERCENT_EMPTY }
  | "%expect"                               { PERCENT_EXPECT }
  | "%expect-rr"                            { PERCENT_EXPECT_RR }
  | "%file-prefix"                          { PERCENT_FILE_PREFIX (lexeme lexbuf) }
  | "%glr-parser"                           { PERCENT_GLR_PARSER }
  | "%header"                               { PERCENT_HEADER }
  | "%initial-action"                       { PERCENT_INITIAL_ACTION }
  | "%language"                             { PERCENT_LANGUAGE }
  | "%left"                                 { PERCENT_LEFT }
  | "%lex-param"                            { PERCENT_PARAM (PARAM_LEX) }
  | "%locations"                            { PERCENT_FLAG ("locations") }
  | "%merge"                                { PERCENT_MERGE }
  | "%no-default-prec"                      { PERCENT_NO_DEFAULT_PREC }
  | "%no-lines"                             { PERCENT_NO_LINES }
  | "%nonassoc"                             { PERCENT_NONASSOC }
  | "%nondeterministic-parser"              { PERCENT_NONDETERMINISTIC_PARSER }
  | "%nterm"                                { PERCENT_NTERM }
  | "%output"                               { PERCENT_OUTPUT }
  | "%param"                                { PERCENT_PARAM (PARAM_BOTH) }
  | "%parse-param"                          { PERCENT_PARAM (PARAM_PARSE) }
  | "%prec"                                 { PERCENT_PREC }
  | "%precedence"                           { PERCENT_PRECEDENCE }
  | "%printer"                              { PERCENT_PRINTER }
  | "%require"                              { PERCENT_REQUIRE }
  | "%right"                                { PERCENT_RIGHT }
  | "%skeleton"                             { PERCENT_SKELETON }
  | "%start"                                { PERCENT_START }
  | "%term"                                 { PERCENT_TOKEN }
  | "%token"                                { PERCENT_TOKEN }
  | "%token-table"                          { PERCENT_TOKEN_TABLE }
  | "%type"                                 { PERCENT_TYPE }
  | "%union"                                { PERCENT_UNION }
  | "%verbose"                              { PERCENT_VERBOSE }
  | "%yacc"                                 { PERCENT_YACC }

  (* Deprecated since Bison 2.3b (2008-05-27), but the warning is
     issued only since Bison 3.4. *)
  | "%pure" ['-' '_'] "parser"              { PERCENT_PURE_PARSER (lexeme lexbuf) }

  (* Deprecated since Bison 3.0 (2013-07-25), but the warning is
     issued only since Bison 3.3. *)
  | "%error-verbose"                        { PERCENT_ERROR_VERBOSE }

  (* Deprecated since Bison 2.6 (2012-07-19), but the warning is
     issued only since Bison 3.3. *)
  | "%name" ['-' '_'] "prefix" eqopt sp     { PERCENT_NAME_PREFIX (lexeme lexbuf) }

  (* Deprecated since Bison 2.7.90, 2012. *)
  | "%default" ['-' '_'] "prec"             { deprecated_directive ("%default-prec") lexbuf; PERCENT_DEFAULT_PREC }
  | "%error" ['-' '_'] "verbose"            { PERCENT_ERROR_VERBOSE }
  | "%expect" ['-' '_'] "rr"                { deprecated_directive ("%expect-rr") lexbuf; PERCENT_EXPECT_RR }
  | "%file-prefix" eqopt                    { PERCENT_FILE_PREFIX (lexeme lexbuf) }
  | "%fixed" ['-' '_'] "output" ['-' '_'] "files"   { deprecated_directive ("%output \"y.tab.c\"") lexbuf; PERCENT_FIXED_OUTPUT_FILES ("\"y.tab.c\"") }
  | "%no" ['-' '_'] "default" ['-' '_'] "prec"      { deprecated_directive ("%no-default-prec") lexbuf; PERCENT_NO_DEFAULT_PREC }
  | "%no" ['-' '_'] "lines"                 { deprecated_directive ("%no-lines") lexbuf; PERCENT_NO_LINES }
  | "%output" eqopt                         { deprecated_directive ("%output") lexbuf; PERCENT_OUTPUT }
  | "%token" ['-' '_'] "table"              { deprecated_directive ("%token-table") lexbuf; PERCENT_TOKEN_TABLE }

  | "%" id                                  { complain (Printf.sprintf "invalid directive: %s" (lexeme lexbuf)) lexbuf; PERCENT_INVALID (lexeme lexbuf) }

  | ':'     { COLON }
  | '='     { EQUAL }
  | '|'     { PIPE }
  | ';'     { SEMICOLON }

  | id      { ID (lexeme lexbuf) }
  | int     { INT_LITERAL (int_of_string (lexeme lexbuf)) }
  | xint    { INT_LITERAL (int_of_string (lexeme lexbuf)) }
  (* Identifiers may not start with a digit.  Yet, don't silently
     accept "1FOO" as "1 FOO".  *)
  | int id  {
      complain (Printf.sprintf "invalid identifier: %s" (lexeme lexbuf)) lexbuf ;
      GRAM_error (Printf.sprintf "invalid identifier: %s" (lexeme lexbuf))
  }

  (* Characters. *)
  | "'"     { begin_ SC_ESCAPED_CHARACTER; sc_escaped_character lexbuf }

  (* Strings. *)
  | '"'    { string_1grow '"'; begin_ SC_ESCAPED_STRING; sc_escaped_string lexbuf }
  | "_(\""  { string_1grow '"'; begin_ SC_ESCAPED_TSTRING; sc_escaped_tstring lexbuf }

  (* Prologue. *)
  | "%{"    { begin_ SC_PROLOGUE; sc_prologue lexbuf }

  (* Code in between braces. *)
  | '{'     {
      string_grow (lexeme lexbuf);
      nesting := 0;
      begin_ SC_BRACED_CODE;
      sc_braced_code lexbuf
  }

  (* Semantic predicate. *)
  | "%?" [' ' '\t' '\011' '\012']* '{'    {
      string_grow (lexeme lexbuf);
      nesting := 0;
      begin_ SC_PREDICATE;
      sc_predicate lexbuf
  }

  (* A type. *)
  | "<*>"   { TAG_ANY }
  | "<>"    { TAG_NONE }
  | '<'     { nesting := 0; begin_ SC_TAG; sc_tag lexbuf }

  | "%%"    {
      incr percent_percent_count;
      if !percent_percent_count == 2 then
          begin_ SC_EPILOGUE;
          PERCENT_PERCENT
  }

  | '['     {
      bracketed_id_str := None;
      begin_ SC_BRACKETED_ID;
      sc_bracketed_id lexbuf
  }

  | [^ '\\' '[' 'A'-'Z' 'a'-'z' '0'-'9' '_' '<' '>' '{' '}' '"' '\'' '*' ';' '|' '=' '/' ',' '\r' '\n' '\t' '\011' '\012' ' ']+ {
      complain (Printf.sprintf "262:invalid character %s" (lexeme lexbuf)) lexbuf;
      GRAM_error (Printf.sprintf "invalid character %s" (lexeme lexbuf))
  }

  (* Comments and white space. *)
  | ',' { complain "stray ',' treated as white space" lexbuf; initial lexbuf }
  | "//" [^ '\n' '\r']* eol { new_line lexbuf; initial lexbuf }
  | "/*" { context_state := !state; begin_ SC_YACC_COMMENT; sc_yacc_comment lexbuf }
  | "#line " int (" \"" [^ '"']* '"')? eol { new_line lexbuf; initial lexbuf }
  | sp  { initial lexbuf }
  | eol {
      new_line lexbuf; initial lexbuf }
  | eof { EOF }
  | _ {
      complain (Printf.sprintf "invalid character %s" (lexeme lexbuf)) lexbuf;
      GRAM_error (Printf.sprintf "invalid character %s" (lexeme lexbuf))
  }

(* This implementation does not support \0. *)

and sc_bracketed_id = parse
  | id      {
      match !bracketed_id_str with
      | Some _ ->
              complain (Printf.sprintf "unexpected identifier in bracketed name: %s" (lexeme lexbuf)) lexbuf;
              GRAM_error (Printf.sprintf "unexpected identifier in bracketed name: %s" (lexeme lexbuf))
      | None ->
              bracketed_id_str := Some (lexeme lexbuf);
              sc_bracketed_id lexbuf
  }
  | ']'     {
      begin_ !state;
      match !bracketed_id_str with
      | Some s ->
                  bracketed_id_str := None;
                  BRACKETED_ID (s)
      | None ->
              complain "identifiers expected" lexbuf;
              GRAM_error "identifier expected"
  }
  | [^ ']' '.' 'A'-'Z' 'a'-'z' '0'-'9' '_' '/' ' ' '\t' '\r' '\n' '\011' '\012']+   {
      complain "invalid characters in bracketed name" lexbuf;
      GRAM_error "invalid characters in bracketed name"
  }
  | eof     {
      begin_ !state;
      let token = match !bracketed_id_str with
      | Some s ->
                  bracketed_id_str := None;
                  BRACKETED_ID(s)
      | None ->
              BRACKETED_ID("?") in
      unexpected_eof "]" lexbuf;
      token
  }

  (* Comments and white space. *)
  | ',' { complain "stray ',' treated as white space" lexbuf; sc_bracketed_id lexbuf }
  | "//" [^ '\n' '\r']* eol { new_line lexbuf; sc_bracketed_id lexbuf }
  | "/*" { context_state := !state; begin_ SC_YACC_COMMENT; sc_yacc_comment lexbuf }
  | "#line " int (" \"" [^ '"']* '"')? eol { new_line lexbuf; sc_bracketed_id lexbuf }
  | sp  { sc_bracketed_id lexbuf }
  | eol { new_line lexbuf; sc_bracketed_id lexbuf }
  | _ {
      complain (Printf.sprintf "invalid character %s" (lexeme lexbuf)) lexbuf;
      GRAM_error (Printf.sprintf "invalid character %s" (lexeme lexbuf))
  }

and sc_yacc_comment = parse
  | "*/"            {
      begin_ !context_state;
      match !context_state with
      | INITIAL -> initial lexbuf
      | SC_BRACKETED_ID -> sc_bracketed_id lexbuf
      | _ -> raise (LexError "invalid state")
  }
  | eol             {
      new_line lexbuf;
      sc_yacc_comment lexbuf
  }
  | eof             {
      unexpected_eof "*/" lexbuf;
      begin_ !context_state;
      match !context_state with
      | INITIAL -> initial lexbuf
      | SC_BRACKETED_ID -> sc_bracketed_id lexbuf
      | _ -> raise (LexError "invalid state")
  }
  | _              { sc_yacc_comment lexbuf }

and sc_comment = parse
  | '*' splice '/'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | '*' '/'         {
      string_grow (lexeme lexbuf);
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | eol             {
      new_line lexbuf;
      sc_comment lexbuf
  }
  | eof             {
      unexpected_eof "*/" lexbuf;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | _               { sc_comment lexbuf }

and sc_line_comment = parse
  | eol             {
      new_line lexbuf;
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | splice          {
      new_line lexbuf;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | eof             {
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | _               { sc_line_comment lexbuf }

and sc_escaped_string = parse
  | '"'    {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      complain "POSIX Yacc does not support string literals" lexbuf;
      STRING(last_string)
  }
  | eol     {
      unexpected_newline "\"" lexbuf;
      sc_escaped_string lexbuf
  }
  | eof     {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      unexpected_eof "\"" lexbuf;
      STRING(last_string)
  }
  | (mbchar | char) {
      string_grow (lexeme lexbuf);
      sc_escaped_string lexbuf
  }
  | '\\' (['0'-'7'] ['0'-'7']? ['0'-'7']? as octal) {
      string_grow_escape (int_of_string ("0o" ^ octal)) lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\x" (['0'-'9' 'a'-'f' 'A'-'F']+ as hex) {
      string_grow_escape (int_of_string ("0x" ^ hex)) lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\a"   {
      string_grow_escape (Char.code '\007') lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\b"   {
      string_grow_escape (Char.code '\b') lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\f"   {
      string_grow_escape (Char.code '\012') lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\n"   {
      string_grow_escape (Char.code '\n') lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\r"   {
      string_grow_escape (Char.code '\r') lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\t"   {
      string_grow_escape (Char.code '\t') lexbuf;
      sc_escaped_string lexbuf
  }
  | "\\v"   {
      string_grow_escape (Char.code '\011') lexbuf;
      sc_escaped_string lexbuf
  }
  | '\\' ['"' '\'' '?' '\\'] as ch {
      string_grow_escape (Char.code ch.[1]) lexbuf;
      sc_escaped_string lexbuf
  }
  (*
  TODO: support "u0000" and "U00000000"
  | '\\' ('u' | 'U') ['0'-'9' 'a'-'f' 'A'-'F']{4} ['0'-'9' 'a'-'f' 'A'-'F']{4} as ucn {
      string_grow_escape (convert_ucn_to_byte ucn)
  }
  *)
  | '\\' char {
      complain (Printf.sprintf "invalid character after \\-escape: %s" (lexeme lexbuf)) lexbuf;
      string_1grow '?';
      sc_escaped_string lexbuf
  }
  | '\\' {
      unexpected_eof "?\"" lexbuf;
      sc_escaped_string lexbuf
  }
  | _ {
      string_grow (lexeme lexbuf);
      sc_escaped_string lexbuf
  }

and sc_escaped_tstring = parse
  | "\")"    {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      complain "POSIX Yacc does not support string literals" lexbuf;
      TSTRING(last_string)
  }| eol     {
      unexpected_newline "\")" lexbuf;
      sc_escaped_tstring lexbuf
  }
  | eof     {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      unexpected_eof "\")" lexbuf;
      TSTRING(last_string)
  }
  | (mbchar | char) {
      string_grow (lexeme lexbuf);
      sc_escaped_tstring lexbuf
  }
  | '\\' (['0'-'7'] ['0'-'7']? ['0'-'7']? as octal) {
      string_grow_escape (int_of_string ("0o" ^ octal)) lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\x" (['0'-'9' 'a'-'f' 'A'-'F']+ as hex) {
      string_grow_escape (int_of_string ("0x" ^ hex)) lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\a"   {
      string_grow_escape (Char.code '\007') lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\b"   {
      string_grow_escape (Char.code '\b') lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\f"   {
      string_grow_escape (Char.code '\012') lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\n"   {
      string_grow_escape (Char.code '\n') lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\r"   {
      string_grow_escape (Char.code '\r') lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\t"   {
      string_grow_escape (Char.code '\t') lexbuf;
      sc_escaped_tstring lexbuf
  }
  | "\\v"   {
      string_grow_escape (Char.code '\011') lexbuf;
      sc_escaped_tstring lexbuf
  }
  | '\\' ['"' '\'' '?' '\\'] as ch {
      string_grow_escape (Char.code ch.[1]) lexbuf;
      sc_escaped_tstring lexbuf
  }
  (*
  TODO: support "u0000" and "U00000000"
  | '\\' ('u' | 'U') ['0'-'9' 'a'-'f' 'A'-'F']{4} ['0'-'9' 'a'-'f' 'A'-'F']{4} as ucn {
      string_grow_escape (convert_ucn_to_byte ucn)
  }
  *)
  | '\\' char {
      complain (Printf.sprintf "invalid character after \\-escape: %s" (lexeme lexbuf)) lexbuf;
      string_1grow '?';
      sc_escaped_tstring lexbuf
  }
  | '\\' {
      unexpected_eof "?\")" lexbuf;
      sc_escaped_tstring lexbuf
  }
  | _ {
      string_grow (lexeme lexbuf);
      sc_escaped_tstring lexbuf
  }

and sc_escaped_character = parse
  | "'"    {
      let last_string = string_finish () in
      begin_ INITIAL;
      if (String.length last_string) != 1 then (
          complain "extra characters in character literal" lexbuf;
          GRAM_error "extra characters in character literal"
      ) else
          CHAR_LITERAL(last_string)
  }
  | eol     {
      unexpected_newline "'" lexbuf;
      sc_escaped_character lexbuf
  }
  | eof     {
      let last_string = string_finish () in
      begin_ INITIAL;
      let token =
      if (String.length last_string) != 1 then (
          complain "extra characters in character literal" lexbuf;
          GRAM_error "extra characters in character literal"
      ) else
          CHAR_LITERAL(last_string) in
      unexpected_eof "'" lexbuf;
      token
  }
  | (mbchar | char) {
      string_grow (lexeme lexbuf);
      sc_escaped_character lexbuf
  }
  | '\\' (['0'-'7'] ['0'-'7']? ['0'-'7']? as octal) {
      string_grow_escape (int_of_string ("0o" ^ octal)) lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\x" (['0'-'9' 'a'-'f' 'A'-'F']+ as hex) {
      string_grow_escape (int_of_string ("0x" ^ hex)) lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\a"   {
      string_grow_escape (Char.code '\007') lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\b"   {
      string_grow_escape (Char.code '\b') lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\f"   {
      string_grow_escape (Char.code '\012') lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\n"   {
      string_grow_escape (Char.code '\n') lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\r"   {
      string_grow_escape (Char.code '\r') lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\t"   {
      string_grow_escape (Char.code '\t') lexbuf;
      sc_escaped_character lexbuf
  }
  | "\\v"   {
      string_grow_escape (Char.code '\011') lexbuf;
      sc_escaped_character lexbuf
  }
  | '\\' ['"' '\'' '?' '\\'] as ch {
      string_grow_escape (Char.code ch.[1]) lexbuf;
      sc_escaped_character lexbuf
  }
  (*
  TODO: support "u0000" and "U00000000"
  | '\\' ('u' | 'U') ['0'-'9' 'a'-'f' 'A'-'F']{4} ['0'-'9' 'a'-'f' 'A'-'F']{4} as ucn {
      string_grow_escape (convert_ucn_to_byte ucn)
  }
  *)
  | '\\' char {
      complain (Printf.sprintf "invalid character after \\-escape: %s" (lexeme lexbuf)) lexbuf;
      string_1grow '?';
      sc_escaped_character lexbuf
  }
  | '\\' {
      unexpected_eof "?'" lexbuf;
      sc_escaped_character lexbuf
  }
  | _ {
      string_grow (lexeme lexbuf);
      sc_escaped_character lexbuf
  }

and sc_tag = parse
  | '>'     {
      decr nesting;
      if !nesting < 0 then
          let last_string = string_finish () in
          begin_ INITIAL;
          TAG(last_string)
      else(
          string_grow (lexeme lexbuf);
          sc_tag lexbuf
      )
  }

  | ([^ '<' '>'] | "->")+   {
      string_grow (lexeme lexbuf);
      sc_tag lexbuf
  }
  | '<'+                    {
      string_grow (lexeme lexbuf);
      nesting := !nesting + (String.length (lexeme lexbuf));
      sc_tag lexbuf
  }

  | eof                     {
      let last_string = string_finish () in
      begin_ INITIAL;
      unexpected_eof ">" lexbuf;
      TAG(last_string)
  }

and sc_character = parse
  | '\''            {
      string_grow (lexeme lexbuf);
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | eol             {
      unexpected_newline "'" lexbuf;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | eof             {
      unexpected_eof "'" lexbuf;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | splice          {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_character lexbuf
  }
  | _               {
      string_grow (lexeme lexbuf);
      sc_character lexbuf
  }

and sc_string = parse
  | '"'             {
      string_grow (lexeme lexbuf);
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | eol             {
      unexpected_newline "\"" lexbuf;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | eof             {
      unexpected_eof "\"" lexbuf;
      match !context_state with
      | SC_BRACED_CODE -> sc_braced_code lexbuf
      | SC_PROLOGUE -> sc_prologue lexbuf
      | SC_EPILOGUE -> sc_epilogue lexbuf
      | SC_PREDICATE -> sc_predicate lexbuf
      | _ -> raise (LexError "invalid token")
  }
  | splice          {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_string lexbuf
  }
  | _               {
      string_grow (lexeme lexbuf);
      sc_string lexbuf
  }

and sc_braced_code = parse
  | '}' {
      string_1grow '}';
      decr nesting;
      if !nesting < 0 then
          let last_string = string_finish () in
          begin_ INITIAL;
          BRACED_CODE (last_string)
      else
          sc_braced_code lexbuf
  }
  | ('{' | '<' '%')  {
      string_grow (lexeme lexbuf);
      incr nesting;
      sc_braced_code lexbuf
  }
  | '<' splice '%'   {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      incr nesting;
      sc_braced_code lexbuf
  }
  | '%' '>'          {
      string_grow (lexeme lexbuf);
      decr nesting;
      sc_braced_code lexbuf
  }
  | '%' splice '>'          {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      decr nesting;
      sc_braced_code lexbuf
  }
  | '<' '<'          {
      string_grow (lexeme lexbuf);
      sc_braced_code lexbuf
  }
  | '<' splice '<'          {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_braced_code lexbuf
  }
  | eof                     {
      let last_string = string_finish () in
      unexpected_eof "}" lexbuf;
      BRACED_CODE(last_string)
  }
  | '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_CHARACTER;
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_STRING;
      sc_string lexbuf
  }
  | '/' splice '*'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' '*'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' splice '/'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | '/' '/'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | eol     {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_braced_code lexbuf
  }
  | sp      {
      string_grow (lexeme lexbuf);
      sc_braced_code lexbuf
  }
  | _   { string_grow (lexeme lexbuf); sc_braced_code lexbuf }

and sc_predicate = parse
  | '}' {
      decr nesting;
      if !nesting < 0 then
          let last_string = string_finish () in
          begin_ INITIAL;
          BRACED_PREDICATE(last_string)
      else (
          string_1grow '}';
          sc_predicate lexbuf
      )
  }
  | (mbchar | char)         {
      string_grow (lexeme lexbuf);
      sc_predicate lexbuf
  }
  | ('{' | '<' '%')         {
      string_grow (lexeme lexbuf);
      incr nesting;
      sc_predicate lexbuf
  }
  | '<' splice '%'          {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      incr nesting;
      sc_predicate lexbuf
  }
  | '%' '>'          {
      string_grow (lexeme lexbuf);
      decr nesting;
      sc_predicate lexbuf
  }
  | '%' splice '>'          {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      decr nesting;
      sc_predicate lexbuf
  }
  | '<' '<'          {
      string_grow (lexeme lexbuf);
      sc_predicate lexbuf
  }
  | '<' splice '<'          {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_predicate lexbuf
  }
  | eof                     {
      let last_string = string_finish () in
      unexpected_eof "}" lexbuf;
      BRACED_PREDICATE(last_string)
  }| '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_CHARACTER;
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_STRING;
      sc_string lexbuf
  }
  | '/' splice '*'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' '*'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' splice '/'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | '/' '/'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | eol     {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_predicate lexbuf
  }
  | sp      {
      string_grow (lexeme lexbuf);
      sc_predicate lexbuf
  }
  | _   { string_grow (lexeme lexbuf); sc_predicate lexbuf }

and sc_prologue = parse
  | "%}" {
      let last_string = string_finish () in
      begin_ INITIAL;
      PROLOGUE(last_string)
  }
  | eof {
      let last_string = string_finish () in
      unexpected_eof "%}" lexbuf;
      PROLOGUE(last_string)
  }
  | '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_CHARACTER;
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_STRING;
      sc_string lexbuf
  }
  | '/' splice '*'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' '*'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' splice '/'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | '/' '/'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | eol     {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_prologue lexbuf
  }
  | sp      {
      string_grow (lexeme lexbuf);
      sc_prologue lexbuf
  }
  | _   { string_grow (lexeme lexbuf); sc_prologue lexbuf }

and sc_epilogue = parse
  | eof {
      let last_string = string_finish () in
      begin_ INITIAL;
      EPILOGUE(last_string)
  }
  | '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_CHARACTER;
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_STRING;
      sc_string lexbuf
  }
  | '/' splice '*'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' '*'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_COMMENT;
      sc_comment lexbuf
  }
  | '/' splice '/'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | '/' '/'  {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ SC_LINE_COMMENT;
      sc_line_comment lexbuf
  }
  | eol     {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      sc_epilogue lexbuf
  }
  | sp      {
      string_grow (lexeme lexbuf);
      sc_epilogue lexbuf
  }
  | _   { string_grow (lexeme lexbuf); sc_epilogue lexbuf }

{
let read_token lexbuf =
    match !state with
    | INITIAL -> initial lexbuf
    | SC_YACC_COMMENT -> sc_yacc_comment lexbuf
    | SC_ESCAPED_CHARACTER -> sc_escaped_character lexbuf
    | SC_ESCAPED_STRING -> sc_escaped_string lexbuf
    | SC_ESCAPED_TSTRING -> sc_escaped_tstring lexbuf
    | SC_TAG -> sc_tag lexbuf
    | SC_PROLOGUE -> sc_prologue lexbuf
    | SC_BRACED_CODE -> sc_braced_code lexbuf
    | SC_EPILOGUE -> sc_epilogue lexbuf
    | SC_PREDICATE -> sc_predicate lexbuf
    | SC_BRACKETED_ID -> sc_bracketed_id lexbuf
    | SC_COMMENT -> sc_comment lexbuf
    | SC_LINE_COMMENT -> sc_line_comment lexbuf
    | SC_CHARACTER -> sc_character lexbuf
    | SC_STRING -> sc_string lexbuf
}
