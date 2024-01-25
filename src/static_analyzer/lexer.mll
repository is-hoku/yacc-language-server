{
open Lexing
open Syntax
open Types
open Language_server.Import

let pp_position fmt p =
  Format.fprintf fmt
    "{ line = %d; character = %d }" p.pos_lnum (p.pos_cnum - p.pos_bol)

type state =
    | INITIAL
    | SC_YACC_COMMENT (* A C-like comment in directives/rules. *)
    | SC_ESCAPED_CHARACTER of position (* Characters and strings in directives/rules. *)
    | SC_ESCAPED_STRING of position (* Characters and strings in directives/rules. *)
    | SC_ESCAPED_TSTRING of position (* Characters and strings in directives/rules. *)
    | SC_TAG of position (* POSIX says that a tag must be both an id and a C union member, but historically almost any character is allowed in a tag.  We disallow NUL, as this simplifies our implementation.  We match angle brackets in nested pairs: several languages use them for generics/template types. *)
    (* Four types of user code *)
    | SC_PROLOGUE of position (* prologue (code between '%{' '%}' in the first section, before %%) *)
    | SC_BRACED_CODE of position (* actions, printers, union, etc, (between braced in the middle section) *)
    | SC_EPILOGUE of position (* epilogue (everything after the second %%). *)
    | SC_PREDICATE of position (* predicate (code between '%?{' and '{' in middle section) *)
    | SC_BRACKETED_ID of position (* Bracketed identifiers support. *)
    | SC_COMMENT (* C and C++ comments in code. *)
    | SC_LINE_COMMENT (* C and C++ comments in code. *)
    | SC_CHARACTER of position (* Strings and characters in code. *)
    | SC_STRING of position (* Strings and characters in code. *)
[@@deriving show]

let state = ref INITIAL
let bracketed_id_str = ref None
let percent_percent_count = ref 0
let context_state = ref INITIAL
let nesting = ref 0

let begin_ new_state =
    state := new_state

let buf = Buffer.create 256

let get_range lexbuf =
    let start_p = lexeme_start_p lexbuf in
    let end_p = lexeme_end_p lexbuf in
    let start = Position.create ~character:(start_p.pos_cnum - start_p.pos_bol) ~line:start_p.pos_lnum in
    let end_ = Position.create ~character:(end_p.pos_cnum - end_p.pos_bol) ~line:end_p.pos_lnum in
    Range.create ~end_ ~start

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
    (if not valid then
        let message = Printf.sprintf "invalid number after \\-escape: %s" (lexeme lexbuf) in
        let range = get_range lexbuf in
        append_diagnostics (Diagnostic.create ~message ~range ()) 
    );
    match !state with
    | SC_ESCAPED_CHARACTER _ -> (
        if valid then
            string_1grow (char_of_int c)
        else
            string_1grow ('?'))
    | _ -> string_grow (lexeme lexbuf)

let unexpected_eof c lexbuf =
    let message = Printf.sprintf "missing %s at end of file" c in
    let range = get_range lexbuf in
    append_diagnostics (Diagnostic.create ~message ~range ())

let unexpected_newline c lexbuf =
    let message = Printf.sprintf "missing %s at end of line" c in
    let range = get_range lexbuf in
    append_diagnostics (Diagnostic.create ~message ~range ());
    new_line lexbuf

let deprecated_directive d lexbuf =
    let message = Printf.sprintf "deprecated directive: %s" d in
    let range = get_range lexbuf in
    append_diagnostics (Diagnostic.create ~message ~range ())
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

  | "%" id                                  {
      let message = Printf.sprintf "invalid directive: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
      append_diagnostics (Diagnostic.create ~message ~range ());
      ERROR (message, pos)
  }

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
      let message = Printf.sprintf "invalid identifier: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
      append_diagnostics (Diagnostic.create ~message ~range ());
      ERROR (message, pos)
  }

  (* Characters. *)
  | "'"     { begin_ (SC_ESCAPED_CHARACTER (lexeme_start_p lexbuf)); sc_escaped_character lexbuf }

  (* Strings. *)
  | '"'    { string_1grow '"'; begin_ (SC_ESCAPED_STRING (lexeme_start_p lexbuf)); sc_escaped_string lexbuf }
  | "_(\""  { string_1grow '"'; begin_ (SC_ESCAPED_TSTRING (lexeme_start_p lexbuf)); sc_escaped_tstring lexbuf }

  (* Prologue. *)
  | "%{"    { begin_ (SC_PROLOGUE (lexeme_start_p lexbuf)); sc_prologue lexbuf }

  (* Code in between braces. *)
  | '{'     {
      string_grow (lexeme lexbuf);
      nesting := 0;
      begin_ (SC_BRACED_CODE (lexeme_start_p lexbuf));
      sc_braced_code lexbuf
  }

  (* Semantic predicate. *)
  | "%?" [' ' '\t' '\011' '\012']* '{'    {
      string_grow (lexeme lexbuf);
      nesting := 0;
      begin_ (SC_PREDICATE (lexeme_start_p lexbuf));
      sc_predicate lexbuf
  }

  (* A type. *)
  | "<*>"   { TAG_ANY }
  | "<>"    { TAG_NONE }
  | '<'     { nesting := 0; begin_ (SC_TAG (lexeme_start_p lexbuf)); sc_tag lexbuf }

  | "%%"    {
      incr percent_percent_count;
      if !percent_percent_count == 2 then
          begin_ (SC_EPILOGUE (lexeme_start_p lexbuf));
          PERCENT_PERCENT
  }

  | '['     {
      bracketed_id_str := None;
      begin_ (SC_BRACKETED_ID (lexeme_start_p lexbuf));
      sc_bracketed_id lexbuf
  }

  | [^ '\\' '[' 'A'-'Z' 'a'-'z' '0'-'9' '_' '<' '>' '{' '}' '"' '\'' '*' ';' '|' '=' '/' ',' '\r' '\n' '\t' '\011' '\012' ' ']+ {
      let message = Printf.sprintf "invalid character: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
      append_diagnostics (Diagnostic.create ~message ~range ());
      ERROR (message, pos)
  }

  (* Comments and white space. *)
  | ','  {
    let message = "stray ',' treated as white space" in
    let range = get_range lexbuf in
    append_diagnostics (Diagnostic.create ~message ~range ~severity:DiagnosticSeverity.Warning ());
    initial lexbuf
  }
  | "//" [^ '\n' '\r']* eol { new_line lexbuf; initial lexbuf }
  | "/*" { context_state := !state; begin_ SC_YACC_COMMENT; sc_yacc_comment lexbuf }
  | "#line " int (" \"" [^ '"']* '"')? eol { new_line lexbuf; initial lexbuf }
  | sp  { initial lexbuf }
  | eol {
      new_line lexbuf; initial lexbuf }
  | eof { EOF }
  | _ {
      let message = Printf.sprintf "invalid character: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
      append_diagnostics (Diagnostic.create ~message ~range ());
      ERROR (message, pos)
  }

(* This implementation does not support \0. *)

and sc_bracketed_id = parse
  | id      {
      match !bracketed_id_str with
      | Some _ ->
              let message = Printf.sprintf "unexpected identifier in bracketed name: %s" (lexeme lexbuf) in
              let range = get_range lexbuf in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              append_diagnostics (Diagnostic.create ~message ~range ());
              ERROR (message, pos)
      | None ->
              bracketed_id_str := Some (lexeme lexbuf);
              sc_bracketed_id lexbuf
  }
  | ']'     {
      begin_ !state;
      match !bracketed_id_str with
      | Some s ->
                  bracketed_id_str := None;
                  let startpos =
                      match !state with
                      | SC_BRACKETED_ID pos -> pos
                      | _ -> dummy_pos in
                  BRACKETED_ID (s, (startpos, (lexeme_end_p lexbuf)))
      | None ->
              let message = "identifiers expected" in
              let range = get_range lexbuf in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              append_diagnostics (Diagnostic.create ~message ~range ());
              ERROR (message, pos)
  }
  | [^ ']' '.' 'A'-'Z' 'a'-'z' '0'-'9' '_' '/' ' ' '\t' '\r' '\n' '\011' '\012']+   {
      let message = "invalid characters in bracketed name" in
      let range = get_range lexbuf in
      let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
      append_diagnostics (Diagnostic.create ~message ~range ());
      ERROR (message, pos)
  }
  | eof     {
  let token =
      let startpos =
          match !state with
          | SC_BRACKETED_ID pos -> pos
          | _ -> dummy_pos in
      match !bracketed_id_str with
      | Some s ->
              bracketed_id_str := None;
              BRACKETED_ID (s, (startpos, (lexeme_end_p lexbuf)))
      | None ->
              BRACKETED_ID("?", (startpos, (lexeme_end_p lexbuf))) in
      unexpected_eof "]" lexbuf;
      token
  }

  (* Comments and white space. *)
  | ',' {
    let message = "stray ',' treated as white space" in
    let range = get_range lexbuf in
    append_diagnostics (Diagnostic.create ~message ~range ~severity:DiagnosticSeverity.Warning ());
    initial lexbuf
  }
  | "//" [^ '\n' '\r']* eol { new_line lexbuf; sc_bracketed_id lexbuf }
  | "/*" { context_state := !state; begin_ SC_YACC_COMMENT; sc_yacc_comment lexbuf }
  | "#line " int (" \"" [^ '"']* '"')? eol { new_line lexbuf; sc_bracketed_id lexbuf }
  | sp  { sc_bracketed_id lexbuf }
  | eol { new_line lexbuf; sc_bracketed_id lexbuf }
  | _ {
      let message = Printf.sprintf "invalid character: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
      append_diagnostics (Diagnostic.create ~message ~range ());
      ERROR (message, pos)
  }

and sc_yacc_comment = parse
  | "*/"            {
      begin_ !context_state;
      match !context_state with
      | INITIAL -> initial lexbuf
      | SC_BRACKETED_ID _ -> sc_bracketed_id lexbuf
      | _ ->
              let msg = "invalid state in SC_YACC_COMMENT" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | eol             {
      new_line lexbuf;
      sc_yacc_comment lexbuf
  }
  | eof             {
      unexpected_eof "*/" lexbuf;
      EOF
  }
  | _              { sc_yacc_comment lexbuf }

and sc_comment = parse
  | '*' splice '/'  {
      new_line lexbuf;
      string_grow (lexeme lexbuf);
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_COMMENT" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | '*' '/'         {
      string_grow (lexeme lexbuf);
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_COMMENT" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | eol             {
      new_line lexbuf;
      sc_comment lexbuf
  }
  | eof             {
      unexpected_eof "*/" lexbuf;
      EOF
  }
  | _               { sc_comment lexbuf }

and sc_line_comment = parse
  | eol             {
      new_line lexbuf;
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_LINE_COMMENT" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | splice          {
      new_line lexbuf;
      match !context_state with
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_LINE_COMMENT" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | eof             { EOF }
  | _               { sc_line_comment lexbuf }

and sc_escaped_string = parse
  | '"'    {
      string_1grow '"';
      let last_string = string_finish () in
      let startpos =
          match !state with
          | SC_ESCAPED_STRING pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      let message = "POSIX Yacc does not support string literals" in
      let range = get_range lexbuf in
      append_diagnostics (Diagnostic.create ~message ~range ());
      STRING(last_string, (startpos, lexeme_end_p lexbuf))
  }
  | eol     {
      unexpected_newline "\"" lexbuf;
      sc_escaped_string lexbuf
  }
  | eof     {
      unexpected_eof "\"" lexbuf;
      string_1grow '"';
      let last_string = string_finish () in
      let startpos =
          match !state with
          | SC_ESCAPED_STRING pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      STRING(last_string, (startpos, lexeme_end_p lexbuf))
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
      let message = Printf.sprintf "invalid character after \\-escape: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      append_diagnostics (Diagnostic.create ~message ~range ());
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
      let startpos =
          match !state with
          | SC_ESCAPED_TSTRING pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      let message = "POSIX Yacc does not support string literals" in
      let range = get_range lexbuf in
      append_diagnostics (Diagnostic.create ~message ~range ());
      TSTRING(last_string, (startpos, lexeme_end_p lexbuf))
  }| eol     {
      unexpected_newline "\")" lexbuf;
      sc_escaped_tstring lexbuf
  }
  | eof     {
      unexpected_eof "\")" lexbuf;
      string_1grow '"';
      let last_string = string_finish () in
      let startpos =
          match !state with
          | SC_ESCAPED_TSTRING pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      TSTRING(last_string, (startpos, lexeme_end_p lexbuf))
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
      let message = Printf.sprintf "invalid character after \\-escape: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      append_diagnostics (Diagnostic.create ~message ~range ());
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
      let startpos =
          match !state with
          | SC_ESCAPED_CHARACTER pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      if (String.length last_string) != 1 then (
          let message = "extra characters in character literal" in
          let range = get_range lexbuf in
          let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
          append_diagnostics (Diagnostic.create ~message ~range ());
          ERROR (message, pos)
      ) else
          CHAR_LITERAL(last_string, (startpos, lexeme_end_p lexbuf))
  }
  | eol     {
      unexpected_newline "'" lexbuf;
      sc_escaped_character lexbuf
  }
  | eof     {
      let last_string = string_finish () in
      let startpos =
          match !state with
          | SC_ESCAPED_CHARACTER pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      let token =
      if (String.length last_string) != 1 then (
          let message = "extra characters in character literal" in
          let range = get_range lexbuf in
          let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
          append_diagnostics (Diagnostic.create ~message ~range ());
          ERROR (message, pos)
      ) else
          CHAR_LITERAL(last_string, (startpos, lexeme_end_p lexbuf)) in
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
      let message = Printf.sprintf "invalid character after \\-escape: %s" (lexeme lexbuf) in
      let range = get_range lexbuf in
      append_diagnostics (Diagnostic.create ~message ~range ());
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
          let startpos =
            match !state with
            | SC_TAG pos -> pos
            | _ -> dummy_pos in
          begin_ INITIAL;
          TAG(last_string, (startpos, lexeme_end_p lexbuf))
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
      unexpected_eof ">" lexbuf;
      decr nesting;
      if !nesting < 0 then
          let last_string = string_finish () in
          let startpos =
            match !state with
            | SC_TAG pos -> pos
            | _ -> dummy_pos in
          begin_ INITIAL;
          TAG(last_string, (startpos, lexeme_end_p lexbuf))
      else(
          EOF
      )
  }

and sc_character = parse
  | '\''            {
      string_grow (lexeme lexbuf);
      begin_ !context_state;
      match !context_state with
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_CHARACTER" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | eol             {
      unexpected_newline "'" lexbuf;
      match !context_state with
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_CHARACTER" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | eof             {
      unexpected_eof "'" lexbuf;
      EOF
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
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_STRING" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | eol             {
      unexpected_newline "\"" lexbuf;
      match !context_state with
      | SC_BRACED_CODE _ -> sc_braced_code lexbuf
      | SC_PROLOGUE _ -> sc_prologue lexbuf
      | SC_EPILOGUE _ -> sc_epilogue lexbuf
      | SC_PREDICATE _ -> sc_predicate lexbuf
      | _ ->
              let msg = "invalid state in SC_STRING" in
              let pos = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
              ERROR (msg, pos)
  }
  | eof             {
      unexpected_eof "\"" lexbuf;
      EOF
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
          let startpos =
              match !state with
              | SC_BRACED_CODE pos -> pos
              | _ -> dummy_pos in
          begin_ INITIAL;
          BRACED_CODE (last_string, (startpos, lexeme_end_p lexbuf))
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
      unexpected_eof "}" lexbuf;
      EOF
  }
  | '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_CHARACTER (lexeme_start_p lexbuf));
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_STRING (lexeme_start_p lexbuf));
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
          let startpos =
              match !state with
              | SC_PREDICATE pos -> pos
              | _ -> dummy_pos in
          begin_ INITIAL;
          BRACED_PREDICATE(last_string, (startpos, lexeme_end_p lexbuf))
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
      unexpected_eof "}" lexbuf;
      EOF
  }| '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_CHARACTER (lexeme_start_p lexbuf));
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_STRING (lexeme_start_p lexbuf));
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
      let startpos =
          match !state with
          | SC_PROLOGUE pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      PROLOGUE(last_string, (startpos, (lexeme_end_p lexbuf)))
  }
  | eof {
      unexpected_eof "%}" lexbuf;
      let last_string = string_finish () in
      let startpos =
          match !state with
          | SC_PROLOGUE pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      PROLOGUE(last_string, (startpos, (lexeme_end_p lexbuf)))
  }
  | '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_CHARACTER (lexeme_start_p lexbuf));
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_STRING (lexeme_start_p lexbuf));
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
      let startpos =
          match !state with
          | SC_EPILOGUE pos -> pos
          | _ -> dummy_pos in
      begin_ INITIAL;
      EPILOGUE(last_string, (startpos, lexeme_end_p lexbuf))
  }
  | '\''    {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_CHARACTER (lexeme_start_p lexbuf));
      sc_character lexbuf
  }
  | '"'     {
      string_grow (lexeme lexbuf);
      context_state := !state;
      begin_ (SC_STRING (lexeme_start_p lexbuf));
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
    | SC_ESCAPED_CHARACTER _ -> sc_escaped_character lexbuf
    | SC_ESCAPED_STRING _ -> sc_escaped_string lexbuf
    | SC_ESCAPED_TSTRING _ -> sc_escaped_tstring lexbuf
    | SC_TAG _ -> sc_tag lexbuf
    | SC_PROLOGUE _ -> sc_prologue lexbuf
    | SC_BRACED_CODE _ -> sc_braced_code lexbuf
    | SC_EPILOGUE _ -> sc_epilogue lexbuf
    | SC_PREDICATE _ -> sc_predicate lexbuf
    | SC_BRACKETED_ID _ -> sc_bracketed_id lexbuf
    | SC_COMMENT -> sc_comment lexbuf
    | SC_LINE_COMMENT -> sc_line_comment lexbuf
    | SC_CHARACTER _ -> sc_character lexbuf
    | SC_STRING _ -> sc_string lexbuf

let initialize () =
    state := INITIAL;
    bracketed_id_str := None;
    percent_percent_count := 0;
    context_state := INITIAL;
    nesting := 0;
}
