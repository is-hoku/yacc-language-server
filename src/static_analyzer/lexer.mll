{
open Lexing
open Syntax
open Types
open Language_server.Import

exception Lex_error of position * string

type state =
    | INITIAL
    | SC_YACC_COMMENT (* A C-like comment in directives/rules. *)
    | SC_ESCAPED_CHARACTER (* Characters and strings in directives/rules. *)
    | SC_ESCAPED_STRING (* Characters and strings in directives/rules. *)
    | SC_ESCAPED_TSTRING (* Characters and strings in directives/rules. *)
    | SC_AFTER_IDENTIFIER (* A identifier was just read in directives/rules.  Special state to capture the sequence 'identifier :'. *)
    | SC_TAG (* POSIX says that a tag must be both an id and a C union member, but historically almost any character is allowed in a tag.  We disallow NUL, as this simplifies our implementation.  We match angle brackets in nested pairs: several languages use them for generics/template types. *)
    (* Four types of user code *)
    | SC_PROLOGUE (* prologue (code between '%{' '%}' in the first section, before %%) *)
    | SC_BRACED_CODE (* actions, printers, union, etc, (between braced in the middle section) *)
    | SC_EPILOGUE (* epilogue (everything after the second %%). *)
    | SC_PREDICATE (* predicate (code between '%?{' and '{' in middle section) *)
    | SC_BRACKETED_ID (* Bracketed identifiers support. *)
    | SC_RETURN_BRACKETED_ID (* Bracketed identifiers support. *)
    | SC_COMMENT (* C and C++ comments in code. *)
    | SC_LINE_COMMENT (* C and C++ comments in code. *)
    | SC_CHARACTER (* Strings and characters in code. *)
    | SC_STRING (* Strings and characters in code. *)
    | RETURN_EOF (* return EOF *)

let state = ref INITIAL
let bracketed_id_context_state = ref INITIAL
let bracketed_id_str = ref None
let saved_pos = ref {
    pos_fname = "initial";
    pos_lnum = 0;
    pos_bol = 0;
    pos_cnum = 0;
}
let percent_percent_count = ref 0
let context_state = ref INITIAL
let nesting = ref 0

let begin_ new_state =
    state := new_state

let buf = Buffer.create 256

let complain msg lexbuf =
    let pos = lexeme_start_p lexbuf in
    raise (Lex_error (pos, msg))

let string_grow str =
  Buffer.add_string buf str

let string_finish () =
  let result = Buffer.contents buf in
  Buffer.clear buf;
  result

let string_1grow char_ =
  Buffer.add_char buf char_

let string_grow_escape c =
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

let save_pos lexbuf =
  saved_pos := lexbuf.lex_curr_p

let rollback lexbuf =
  lexbuf.lex_curr_p <- !saved_pos

let report_error msg lexbuf =
    let pos = lexeme_start_p lexbuf in
    raise (Lex_error (pos, msg))

let return_eof () =
    EOF

let unexpected_eof c token lexbuf =
    begin_ RETURN_EOF;
    complain (Printf.sprintf "missing %s at end of file" c) lexbuf;
    match token with
    | Some t -> token
    | None -> ()

let unexpected_newline c lexbuf =
    complain (Printf.sprintf "missing %s at end of line" c) lexbuf;
    newline lexbuf

let deprecated_directive d =
    complain (Printf.sprintf "deprecated directive: %s" d) lexbuf;
}

let letter = ['.' 'a'-'z' 'A'-'Z' '_']
let id = letter (letter | ['-' '0'-'9'])*
let int = ['0'-'9']+
let xint = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let eol = '\n' | '\r' '\n'

let char = ['\000'-'\255']
let mbchar =
    ['\x09' '\x0A' '\x0D' '\x20'-'\x7E']
  | ['\xC2'-'\xDF']['\x80'-'\xBF']
  | '\xE0'['\xA0'-'\xBF']['\x80'-'\xBF']
  | ['\xE1'-'\xEC' '\xEE' '\xEF']['\x80'-'\xBF']['\x80'-'\xBF']
  | '\xED'['\x80'-'\x9F']['\x80'-'\xBF']
  | '\xF0'['\x90'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
  | ['\xF1'-'\xF3']['\x80'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF']
  | '\xF4'['\x80'-'\x8F']['\x80'-'\xBF']['\x80'-'\xBF']

let splice = ('\\' [' ' '\t' '\011' '\012']* eol)*

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
  | "%default" ['-' '_'] "prec"             { deprecated_directive ("%default-prec") }
  | "%error" ['-' '_'] "verbose"            { PERCENT_ERROR_VERBOSE (lexeme lexbuf) }
  | "%expect" ['-' '_'] "rr"                { deprecated_directive ("%expect-rr") }
  | "%file-prefix" eqopt                    { PERCENT_FILE_PREFIX (lexeme lexbuf) }
  | "%fixed" ['-' '_'] "output" ['-' '_'] "files"   { deprecated_directive ("%output \"y.tab.c\"") }
  | "%no" ['-' '_'] "default" ['-' '_'] "prec"      { deprecated_directive ("%no-default-prec") }
  | "%no" ['-' '_'] "lines"                 { deprecated_directive ("%no-lines") }
  | "%output" eqopt                         { deprecated_directive ("%output") }
  | "%token" ['-' '_'] "table"              { deprecated_directive ("%token-table") }

  | "%" id                                  { complain (Printf.sprintf "invalid directive: %s" (lexeme lexbuf)) lexbuf }

  | ':'     { COLON }
  | '='     { EQUAL }
  | '|'     { PIPE }
  | ';'     { SEMICOLON }

  | id      {
      bracketed_id_str := None;
      begin_ SC_AFTER_IDENTIFIER
  }
  | int     { INT_LITERAL (int_of_string (lexeme lexbuf)) }
  | xint    { INT_LITERAL (int_of_string (lexeme lexbuf)) }
  (* Identifiers may not start with a digit.  Yet, don't silently
     accept "1FOO" as "1 FOO".  *)
  | int id  {
      complain (Printf.sprintf "invalid identifier: %s" (lexeme lexbuf)) lexbuf ;
      GRAM_error
  }

  (* Characters. *)
  | "'"     { begin_ SC_ESCAPED_CHARACTER }

  (* Strings. *)
  | '"'    { string_1grow '"'; begin_ SC_ESCAPED_STRING }
  | "_(\""  { string_1grow '"'; begin_ SC_ESCAPED_TSTRING }

  (* Prologue. *)
  | "%{"    { begin_ SC_PROLOGUE }

  (* Code in between braces. *)
  | '{'     {
      string_grow (lexeme lexbuf);
      nesting := 0;
      begin_ SC_BRACED_CODE
  }

  (* Semantic predicate. *)
  | "%?" ([' ' '\t' '\011' '\012'] | eol)* '{'    {
      string_grow (lexeme lexbuf);
      nesting := 0;
      begin_ SC_PREDICATE
  }

  (* A type. *)
  | "<*>"   { TAG_ANY }
  | "<>"    { TAG_NONE }
  | '<'     { nesting := 0; begin_ SC_TAG }

  | "%%"    {
      if (incr percent_percent_count) == 2 then
          begin_ SC_EPILOGUE
          PERCENT_PERCENT
  }

  | '['     {
      bracketed_id_str := None
      bracketed_id_context_state := !state;
      begin_ SC_BRACKETED_ID
  }

  | [^ '\\' '[' 'A'-'Z' 'a'-'z' '0'-'9' '_' '<' '>' '{' '}' '"' '\'' '*' ';' '|' '=' '/' ',' '\r' '\n' '\t' '\011' '\012']+ | char {
      complain (Printf.sprintf "invalid character %s" (lexeme lexbuf)) lexbuf;
      GRAM_error
  }

  | eof { EOF }
  | _   { common_initial_scafteridentifier_scbracketedid_screturnbracketedid lexbuf }

(* INITIAL,SC_AFTER_IDENTIFIER,SC_BRACKETED_ID,SC_RETURN_BRACKETED_ID *)
and common_initial_scafteridentifier_scbracketedid_screturnbracketedid = parse
  (* Comments and white space. *)
  | ','                                     { complain "stray ',' treated as white space" lexbuf }
  | [' ' '\t' '\r' '\011' '\012'] eol       { new_line lexbuf }
  | "//" [^ '\n' '\r']* eol                       { (* ignore *) }
  | "/*"                                    { context_state := !state; begin_ SC_YACC_COMMENT }
  | "#line " int (" \"" [^ '"']* '"')? eol { (* ignore *) }

(* This implementation does not support \0. *)

and sc_after_identifier = parse
  | '['     {
      match !bracketed_id_str with
      | Some s ->
              rollback lexbuf;
              save_pos lexbuf;
              begin_ SC_RETURN_BRACKETED_ID;
              ID(s)
      | None ->
              bracketed_id_context_state := !state;
              begin_ SC_BRACKETED_ID
  }
  | ':'     {
      rollback lexbuf;
      match !bracketed_id_str with
      | Some _ ->
              save_pos lexbuf;
              begin_ SC_RETURN_BRACKETED_ID
      | None -> begin_ INITIAL;
      ID_COLON
  }
  | char   {
      rollback lexbuf;
      match !bracketed_id_str with
      | Some _ ->
              save_pos lexbuf;
              begin_ SC_RETURN_BRACKETED_ID
      | None -> begin_ INITIAL;
      ID
  }
  | eof     {
      rollback lexbuf;
      match !bracketed_id_str with
      | Some _ ->
              save_pos lexbuf;
              begin_ SC_RETURN_BRACKETED_ID
      | None -> begin_ INITIAL;
      ID
  }
  | _       { common_initial_scafteridentifier_scbracketedid_screturnbracketedid lexbuf }

and sc_bracketed_id = parse
  | id      {
      match !bracketed_id_str with
      | Some _ ->
              complain (Printf.sprintf "unexpected identifier in bracketed name: %s" lexeme lexbuf) lexbuf;
              GRAM_error
      | None ->
              bracketed_id_str := lexeme lexbuf
  }
  | ']'     {
      begin_ !bracketed_id_context_state;
      match !bracketed_id_str with
      | Some s ->
              if INITIAL = !bracketed_id_context_state then
                  bracketed_id_str := None;
                  BRACKETED_ID(s)
      | None ->
              complain "an identifier expected" lexbuf;
              GRAM_error
  }
  | [^ ']' '.' 'A'-'Z' 'a'-'z' '0'-'9' '_' '/' ' ' '\t' '\r' '\n' '\011' '\012']+ | char    {
      complain "invalid characters in bracketed name" lexbuf;
      GRAM_error
  }
  | eof     {
      begin_ !bracketed_id_context_state;
      let token = match !bracketed_id_str with
      | Some s ->
              if INITIAL = !bracketed_id_context_state then
                  bracketed_id_str := None;
                  BRACKETED_ID(s)
      | None ->
              BRACKETED_ID("?") in
      unexpected_eof "]" Some(token) lexbuf
  }
  | _       { common_initial_scafteridentifier_scbracketedid_screturnbracketedid lexbuf }

and sc_return_bracketed_id = parse
  | char    {
      rollback lexbuf;
      let id = !bracketed_id_str in
      bracketed_id_str := None;
      begin_ INITIAL;
      BRACKETED_ID(id)
  }
  | _       { common_initial_scafteridentifier_scbracketedid_screturnbracketedid lexbuf }

and sc_yacc_comment = parse
  | "*/"            { begin_ !context_state }
  | [^ '\n' '\r']   { (* continue *) }
  | eof             { unexpected_eof "*/" None lexbuf; begin_ !context_state }

and sc_comment = parse
  | '*' splice '/'  { string_grow (lexeme lexbuf); begin_ !context_state }
  | eof             { unexpected_eof "*/" None lexbuf }
  | (mbchar | char) { string_grow (lexeme lexbuf) }

and sc_line_comment = parse
  | eol             { string_grow (lexeme lexbuf); begin_ !context_state }
  | splice          { string_grow (lexeme lexbuf) }
  | eof             { begin_ !context_state }
  | (mbchar | char) { string_grow (lexeme lexbuf) }

and sc_escaped_string = parse
  | '"'    {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      complain "POSIX Yacc does not support string literals" lexbuf;
      STRING(last_string)
  }
  | eof     {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      unexpected_eof "\"" (Some(STRING(last_string))) lexbuf
  }
  | '\n'    { unexpected_newline (get_range lexbuf) '"' }
  | (mbchar | char) { string_grow (lexeme lexbuf) }
  | _               { common_scescapedcharacter_scescapedstring_scescapedtstring lexbuf }

and sc_escaped_tstring = parse
  | "\")"    {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      complain "POSIX Yacc does not support string literals" lexbuf;
      TSTRING(last_string)
  }
  | eof     {
      string_1grow '"';
      let last_string = string_finish () in
      begin_ INITIAL;
      unexpected_eof "\")" (Some(TSTRING(last_string))) lexbuf
  }
  | '\n'            { unexpected_newline (get_range lexbuf) "\")" }
  | (mbchar | char) { string_grow (lexeme lexbuf) }
  | _               { common_scescapedcharacter_scescapedstring_scescapedtstring lexbuf }

and sc_escaped_character = parse
  | "'"    {
      let last_string = string_finish () in
      begin_ INITIAL;
      if (String.length last_string) != 1 then (
          complain "extra characters in character literal" lexbuf;
          GRAM_error
      ) else
          CHAR_LITERAL(last_string)
  }
  | eol     { unexpected_newline (get_range lexbuf) "'" }
  | eof     {
      let last_string = string_finish () in
      begin_ INITIAL;
      let token =
      if (String.length last_string) != 1 then (
          complain "extra characters in character literal" lexbuf;
          GRAM_error
      ) else
          CHAR_LITERAL(last_string) in
      unexpected_eof "'" Some(token) lexbuf
  }
  | (mbchar | char) { string_grow (lexeme lexbuf) }
  | _               { common_scescapedcharacter_scescapedstring_scescapedtstring lexbuf }

and sc_tag = parse
  | '>'     {
      decr nesting;
      if !nesting < 0 then
          let last_string = string_finish () in
          begin_ INITIAL;
          TAG(last_string)
      else
          string_grow (lexeme lexbuf)
  }

  | ([^ '<' '>'] | "->")+   { string_grow (lexeme lexbuf) }
  | '<'+                    { string_grow (lexeme lexbuf); nesting := nesting + (String.length (lexeme lexbuf)) }

  | eof                     {
      let last_string = string_finish () in
      begin_ INITIAL;
      unexpected_eof ">" Some(TAG(last_string)) lexbuf
  }

(* SC_ESCAPED_CHARACTER,SC_ESCAPED_STRING,SC_ESCAPED_TSTRING *)
and common_scescapedcharacter_scescapedstring_scescapedtstring = parse
  | '\\' (['0'-'7'] ['0'-'7']? ['0'-'7']? as octal) {
      string_grow_escape (int_of_string ("0o" ^ octal))
  }
  | "\\x" (['0'-'9' 'a'-'f' 'A'-'F']+ as hex) {
      string_grow_escape (int_of_string ("0x" ^ hex))
  }
  | "\\a"   { string_grow_escape '\007' }
  | "\\b"   { string_grow_escape '\b' }
  | "\\f"   { string_grow_escape '\012' }
  | "\\n"   { string_grow_escape '\n' }
  | "\\r"   { string_grow_escape '\r' }
  | "\\t"   { string_grow_escape '\t' }
  | "\\v"   { string_grow_escape '\011' }
  | '\\' ['"' '\'' '?' '\\'] as ch { string_grow_escape ch.[1] }
  (*
  TODO: support "u0000" and "U00000000"
  | '\\' ('u' | 'U') ['0'-'9' 'a'-'f' 'A'-'F']{4} ['0'-'9' 'a'-'f' 'A'-'F']{4} as ucn {
      string_grow_escape (convert_ucn_to_byte ucn)
  }
  *)
  | '\\' (char | eol) {
      complain (Printf.sprintf "invalid character after \\-escape: %s" (lexeme lexbuf)) lexbuf;
      string_1grow '?'
  }
  | '\\' {
      let c = match !state with
      | SC_ESCAPED_CHARACTER -> "?'"
      | SC_ESCAPED_STRING -> "?\""
      | _ -> "?\")" in
      unexpected_eof c None lexbuf
  }
(* SC_CHARACTER,SC_STRING *)
and common_sccharacter_scstringn = parse
  | (splice | '\\' splice [^ '\n' '[' ']']) { string_grow (lexeme lexbuf) }

and sc_character = parse
  | '\''            { string_grow (lexeme lexbuf); begin_ !context_state }
  | eol             { unexpected_newline (get_range lexbuf) '\'' }
  | eof             { unexpected_eof "'" None lexbuf }
  | (mbchar | char) { string_grow (lexeme lexbuf) }
  | _               { common_scharacter_scstring lexbuf }

and sc_string = parse
  | '"'             { string_grow (lexeme lexbuf); begin_ !context_state }
  | eol             { unexpected_newline (get_range lexbuf) '"' }
  | eof             { unexpected_eof "\"" None lexbuf }
  | (mbchar | char) { string_grow (lexeme lexbuf) }
  | _               { common_scharacter_scstring lexbuf }

(* SC_BRACED_CODE,SC_PROLOGUE,SC_EPILOGUE,SC_PREDICATE *)
and common_scbracedcode_scprologue_scepilogue_scpredicate = parse
  | '\''    {
      string_grow (lexeme lexbuf)
      context_state := !state;
      begin_ SC_CHARACTER
  }
  | '"'     {
      string_grow (lexeme lexbuf)
      context_state := !state;
      begin_ SC_STRING
  }
  | '/' splice '*'  {
      string_grow (lexeme lexbuf)
      context_state := !state;
      begin_ SC_COMMENT
  }
  | '/' splice '/'  {
      string_grow (lexeme lexbuf)
      context_state := !state;
      begin_ SC_LINE_COMMENT
  }

and sc_braced_code = parse
  | '}' {
      string_1grow '}';
      decr nesting;
      if !nesting < 0 then
          let last_string = string_finish () in
          begin_ INITIAL;
          BRACED_CODE(last_string)
  }
  | (mbchar | char)         { string_grow (lexeme lexbuf) }
  | ('{' | '<' splice '%')  { string_grow (lexeme lexbuf); incr nesting }
  | '%' splice '>'          { string_grow (lexeme lexbuf); decr nesting }
  | '<' splice '<'          { string_grow (lexeme lexbuf) }
  | eof                     {
      let last_string = string_finish () in
      unexpected_eof "}" Some(BRACED_CODE(last_string)) lexbuf
  }
  | _   { common_scbracedcode_scprologue_scepilogue_scpredicate lexbuf }

and sc_predicate = parse
  | '}' {
      decr nesting;
      if !nesting < 0 then
          let last_string = string_finish () in
          begin_ INITIAL;
          BRACED_PREDICATE(last_string)
      else
          string_1grow '}'
  }
  | (mbchar | char)         { string_grow (lexeme lexbuf) }
  | ('{' | '<' splice '%')  { string_grow (lexeme lexbuf); incr nesting }
  | '%' splice '>'          { string_grow (lexeme lexbuf); decr nesting }
  | '<' splice '<'          { string_grow (lexeme lexbuf) }
  | eof                     {
      let last_string = string_finish () in
      unexpected_eof "}" Some(BRACED_PREDICATE(last_string)) lexbuf
  }
  | _ { common_scbracedcode_scprologue_scepilogue_scpredicate lexbuf }

and sc_prologue = parse
  | "%}" {
      let last_string = string_finish () in
      begin_ INITIAL;
      PROLOGUE(last_string)
  }
  | eof {
      let last_string = string_finish () in
      unexpected_eof "%}" Some(PROLOGUE(last_string)) lexbuf
  }
  | (mbchar | char) { string_grow (lexeme lexbuf) }
  | _               { common_scbracedcode_scprologue_scepilogue_scpredicate lexbuf }

and sc_epilogue = parse
  | eof {
      let last_string = string_finish () in
      begin_ INITIAL;
      EPILOGUE(last_string)
  }
  | (mbchar | char) { string_grow (lexeme lexbuf) }
  | _               { common_scbracedcode_scprologue_scepilogue_scpredicate lexbuf }

{
let read_token lexbuf =
    match !state with
    | INITIAL -> initial lexbuf
    | SC_YACC_COMMENT -> sc_yacc_comment lexbuf
    | SC_ESCAPED_CHARACTER -> sc_escaped_character lexbuf
    | SC_ESCAPED_STRING -> sc_escaped_string lexbuf
    | SC_ESCAPED_TSTRING -> sc_escaped_tstring lexbuf
    | SC_AFTER_IDENTIFIER -> sc_after_identifier lexbuf
    | SC_TAG -> sc_tag lexbuf
    | SC_PROLOGUE -> sc_prologue lexbuf
    | SC_BRACED_CODE -> sc_bracked_code lexbuf
    | SC_EPILOGUE -> sc_epilogue lexbuf
    | SC_PREDICATE -> sc_predicate lexbuf
    | SC_BRACKETED_ID -> sc_bracketed_id lexbuf
    | SC_RETURN_BRACKETED_ID -> sc_return_bracketed_id lexbuf
    | SC_COMMENT -> sc_comment lexbuf
    | SC_LINE_COMMENT -> sc_line_comment lexbuf
    | SC_CHARACTER -> sc_character lexbuf
    | SC_STRING -> sc_string lexbuf
    | RETURN_EOF -> return_eof ()
}
