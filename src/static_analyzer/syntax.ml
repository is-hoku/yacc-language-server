open Lexing
open Types
open Language_server.Import

exception RecoveryError of string * (position * position)

module I = Parser.MenhirInterpreter

module Printer = struct
  open I

  let string_of_element = function
    | Element (s, v, _, _) -> (
        match incoming_symbol s with
        | T T_PERCENT_TOKEN -> "%token"
        | T T_PERCENT_NTERM -> "%nterm"
        | T T_PERCENT_TYPE -> "%type"
        | T T_PERCENT_DESTRUCTOR -> "%destructor"
        | T T_PERCENT_PRINTER -> "%printer"
        | T T_PERCENT_LEFT -> "%left"
        | T T_PERCENT_RIGHT -> "%right"
        | T T_PERCENT_NONASSOC -> "%nonassoc"
        | T T_PERCENT_PRECEDENCE -> "%precedence"
        | T T_PERCENT_PREC -> "%prec"
        | T T_PERCENT_DPREC -> "%dprec"
        | T T_PERCENT_MERGE -> "%merge"
        | T T_PERCENT_CODE -> "%code"
        | T T_PERCENT_DEFAULT_PREC -> "%default-prec"
        | T T_PERCENT_DEFINE -> "%define"
        | T T_PERCENT_ERROR_VERBOSE -> "%error-verbose"
        | T T_PERCENT_FILE_PREFIX -> v
        | T T_PERCENT_FLAG -> v
        | T T_PERCENT_NAME_PREFIX -> v
        | T T_PERCENT_PURE_PARSER -> v
        | T T_PERCENT_EXPECT -> "%expect"
        | T T_PERCENT_EXPECT_RR -> "%expect-rr"
        | T T_PERCENT_GLR_PARSER -> "%glr-parser"
        | T T_PERCENT_HEADER -> "%header"
        | T T_PERCENT_INITIAL_ACTION -> "%initial-action"
        | T T_PERCENT_LANGUAGE -> "%language"
        | T T_PERCENT_NO_DEFAULT_PREC -> "%no-default-prec"
        | T T_PERCENT_NO_LINES -> "%no-lines"
        | T T_PERCENT_NONDETERMINISTIC_PARSER -> "%nondeterministic-parser"
        | T T_PERCENT_OUTPUT -> "%output"
        | T T_PERCENT_REQUIRE -> "%require"
        | T T_PERCENT_SKELETON -> "%skeleton"
        | T T_PERCENT_START -> "%start"
        | T T_PERCENT_TOKEN_TABLE -> "%token-table"
        | T T_PERCENT_VERBOSE -> "%verbose"
        | T T_PERCENT_YACC -> "%yacc"
        | T T_ID -> v
        | T T_PERCENT_FIXED_OUTPUT_FILES -> v
        | T T_COLON -> ":"
        | T T_EQUAL -> "="
        | T T_PERCENT_PERCENT -> "%%"
        | T T_PIPE -> "|"
        | T T_SEMICOLON -> ";"
        | T T_TAG_ANY -> "<*>"
        | T T_TAG_NONE -> "<>"
        | T T_EOF -> "eof"
        | T T_PROLOGUE -> fst v
        | T T_CHAR_LITERAL -> fst v
        | T T_STRING -> fst v
        | T T_TSTRING -> fst v
        | T T_TAG -> fst v
        | T T_BRACED_CODE -> fst v
        | T T_BRACED_PREDICATE -> fst v
        | T T_BRACKETED_ID -> fst v
        | T T_EPILOGUE -> fst v
        | T T_INT_LITERAL -> string_of_int v
        | T T_PERCENT_PARAM -> show_param_type v
        | T T_PERCENT_UNION -> "%union"
        | T T_PERCENT_EMPTY -> "%empty"
        | T T_error -> "error"
        | T T_ERROR -> fst v
        | N N_input -> show v
        | N N_prologue_declarations ->
            String.concat ", " (List.map show_prologue_declaration v)
        | N N_prologue_declaration -> show_prologue_declaration v
        | N N_params -> String.concat ", " (List.map show_string_ v)
        | N N_grammar_declaration -> show_grammar_declaration v
        | N N_code_props_type -> show_grammar_declaration v
        | N N_union_name -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (show_string_ s)
            | None -> "None")
        | N N_symbol_declaration ->
            String.concat ", " (List.map show_grammar_declaration v)
        | N N_percent_symbol -> "(percent_symbol)"
        | N N_precedence_declarator -> "(precedence_declarator)"
        | N N_string_opt -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (fst s)
            | None -> "None")
        | N N_tag_opt -> (
            match v with
            | Some tag -> Printf.sprintf "Some (%s)" (show_tag tag)
            | None -> "None")
        | N N_generic_symlist -> String.concat ", " (List.map show_symbol_tag v)
        | N N_generic_symlist_item -> show_symbol_tag v
        | N N_tag -> show_tag v
        | N N_token_decls ->
            String.concat ", " (List.map show_grammar_declaration v)
        | N N_token_decl_1 ->
            String.concat ", " (List.map show_grammar_declaration v)
        | N N_token_decl -> show_grammar_declaration v
        | N N_int_opt -> (
            match v with
            | Some i -> Printf.sprintf "Some (%d)" (fst i)
            | None -> "None")
        | N N_alias -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (show_string_ s)
            | None -> "None")
        | N N_token_decls_for_prec ->
            String.concat ", " (List.map show_grammar_declaration v)
        | N N_token_decl_for_prec_1 ->
            String.concat ", " (List.map show_grammar_declaration v)
        | N N_token_decl_for_prec -> show_grammar_declaration v
        | N N_symbols_1 -> String.concat ", " (List.map show_symbol v)
        | N N_grammar -> String.concat ", " (List.map show_grammar v)
        | N N_rules_or_grammar_declaration -> show_grammar v
        | N N_rules -> show_grammar v
        | N N_rhses_1 -> String.concat ", " (List.map show_rhs v)
        | N N_rhs -> String.concat ", " (List.map show_rhs v)
        | N N_named_ref_opt -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (show_string_ s)
            | None -> "None")
        | N N_variable -> show_string_ v
        | N N_value -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (show_value s)
            | None -> "None")
        | N N_id -> show_string_ v
        | N N_symbol -> show_symbol v
        | N N_string_as_id -> show_string_ v
        | N N_epilogue_opt -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (show_string_ s)
            | None -> "None"))

  let string_of_symbol = function
    | X (T T_PERCENT_TOKEN) -> "%token"
    | X (T T_PERCENT_NTERM) -> "%nterm"
    | X (T T_PERCENT_TYPE) -> "%type"
    | X (T T_PERCENT_DESTRUCTOR) -> "%destructor"
    | X (T T_PERCENT_PRINTER) -> "%printer"
    | X (T T_PERCENT_LEFT) -> "%left"
    | X (T T_PERCENT_RIGHT) -> "%right"
    | X (T T_PERCENT_NONASSOC) -> "%nonassoc"
    | X (T T_PERCENT_PRECEDENCE) -> "%precedence"
    | X (T T_PERCENT_PREC) -> "%prec"
    | X (T T_PERCENT_DPREC) -> "%dprec"
    | X (T T_PERCENT_MERGE) -> "%merge"
    | X (T T_PERCENT_CODE) -> "%code"
    | X (T T_PERCENT_DEFAULT_PREC) -> "%default-prec"
    | X (T T_PERCENT_DEFINE) -> "%define"
    | X (T T_PERCENT_ERROR_VERBOSE) -> "%error-verbose"
    | X (T T_PERCENT_FILE_PREFIX) -> "%file-prefix"
    | X (T T_PERCENT_FLAG) -> "%flag"
    | X (T T_PERCENT_NAME_PREFIX) -> "%name-prefix"
    | X (T T_PERCENT_PURE_PARSER) -> "%pure-parser"
    | X (T T_PERCENT_EXPECT) -> "%expect"
    | X (T T_PERCENT_EXPECT_RR) -> "%expect-rr"
    | X (T T_PERCENT_GLR_PARSER) -> "%glr-parser"
    | X (T T_PERCENT_HEADER) -> "%header"
    | X (T T_PERCENT_INITIAL_ACTION) -> "%initial-action"
    | X (T T_PERCENT_LANGUAGE) -> "%language"
    | X (T T_PERCENT_NO_DEFAULT_PREC) -> "%no-default-prec"
    | X (T T_PERCENT_NO_LINES) -> "%no-lines"
    | X (T T_PERCENT_NONDETERMINISTIC_PARSER) -> "%nondeterministic-parser"
    | X (T T_PERCENT_OUTPUT) -> "%output"
    | X (T T_PERCENT_REQUIRE) -> "%require"
    | X (T T_PERCENT_SKELETON) -> "%skeleton"
    | X (T T_PERCENT_START) -> "%start"
    | X (T T_PERCENT_TOKEN_TABLE) -> "%token-table"
    | X (T T_PERCENT_VERBOSE) -> "%verbose"
    | X (T T_PERCENT_YACC) -> "%yacc"
    | X (T T_ID) -> "(id)"
    | X (T T_PERCENT_FIXED_OUTPUT_FILES) -> "%fixed-output-files"
    | X (T T_COLON) -> ":"
    | X (T T_EQUAL) -> "="
    | X (T T_PERCENT_PERCENT) -> "%%"
    | X (T T_PIPE) -> "|"
    | X (T T_SEMICOLON) -> ";"
    | X (T T_TAG_ANY) -> "<*>"
    | X (T T_TAG_NONE) -> "<>"
    | X (T T_EOF) -> "eof"
    | X (T T_PROLOGUE) -> "%{...%}"
    | X (T T_CHAR_LITERAL) -> "character literal"
    | X (T T_STRING) -> "string"
    | X (T T_TSTRING) -> "translatable string"
    | X (T T_TAG) -> "<tag>"
    | X (T T_BRACED_CODE) -> "{...}"
    | X (T T_BRACED_PREDICATE) -> "%?{...}"
    | X (T T_BRACKETED_ID) -> "[identifier]"
    | X (T T_EPILOGUE) -> "epilogue"
    | X (T T_INT_LITERAL) -> "integer literal"
    | X (T T_PERCENT_PARAM) -> "%param"
    | X (T T_PERCENT_UNION) -> "%union"
    | X (T T_PERCENT_EMPTY) -> "%empty"
    | X (T T_error) -> "error"
    | X (T T_ERROR) -> "ERROR"
    | X (N N_input) -> "input"
    | X (N N_prologue_declarations) -> "prologue_declarations"
    | X (N N_prologue_declaration) -> "prologue_declaration"
    | X (N N_params) -> "params"
    | X (N N_grammar_declaration) -> "grammar_declaration"
    | X (N N_code_props_type) -> "code_props_type"
    | X (N N_union_name) -> "union_name"
    | X (N N_symbol_declaration) -> "symbol_declaration"
    | X (N N_percent_symbol) -> "percent_symbol"
    | X (N N_precedence_declarator) -> "precedence_declarator"
    | X (N N_string_opt) -> "string_opt"
    | X (N N_tag_opt) -> "tag_opt"
    | X (N N_generic_symlist) -> "generic_symlist"
    | X (N N_generic_symlist_item) -> "generic_symlist_item"
    | X (N N_tag) -> "tag"
    | X (N N_token_decls) -> "token_decls"
    | X (N N_token_decl_1) -> "token_decl_1"
    | X (N N_token_decl) -> "token_decl"
    | X (N N_int_opt) -> "int_opt"
    | X (N N_alias) -> "alias"
    | X (N N_token_decls_for_prec) -> "token_decls_for_prec"
    | X (N N_token_decl_for_prec_1) -> "token_decl_for_prec_1"
    | X (N N_token_decl_for_prec) -> "token_decl_for_prec"
    | X (N N_symbols_1) -> "symbols_1"
    | X (N N_grammar) -> "grammar"
    | X (N N_rules_or_grammar_declaration) -> "rules_or_grammar_declaration"
    | X (N N_rules) -> "rules"
    | X (N N_rhses_1) -> "rhses_1"
    | X (N N_rhs) -> "rhs"
    | X (N N_named_ref_opt) -> "named_ref_opt"
    | X (N N_variable) -> "variable"
    | X (N N_value) -> "value"
    | X (N N_id) -> "id"
    | X (N N_symbol) -> "symbol"
    | X (N N_string_as_id) -> "string_as_id"
    | X (N N_epilogue_opt) -> "epilogue_opt"

  let print s = print_string s
  let print_element = Some (fun s -> print (string_of_element s))
  let print_symbol s = print (string_of_symbol s)
end

module P = MenhirLib.Printers.Make (I) (Printer)

let print_debug_info penv state =
  print_endline "--- top ---";
  (match I.top penv with
  | Some e -> P.print_element_as_symbol e
  | None -> print_string "No symbol on top of the stack");
  print_newline ();
  print_endline "--- env ---";
  P.print_env penv;
  print_endline "--- current state ---";
  P.print_current_state penv;
  print_endline "--- incoming symbol ---";
  P.print_symbols [ I.X (I.incoming_symbol state) ];
  print_newline ();
  let items = I.items state in
  print_endline "--- items ---";
  let _ = List.map P.print_item items in
  print_newline ();
  print_endline "--- production ---";
  P.print_production (I.find_production (I.current_state_number penv))

let terminal2token xsym =
  let open I in
  let open Parser in
  match xsym with
  | X (T T_PERCENT_TOKEN) -> PERCENT_TOKEN
  | X (T T_PERCENT_NTERM) -> PERCENT_NTERM
  | X (T T_PERCENT_TYPE) -> PERCENT_TYPE
  | X (T T_PERCENT_DESTRUCTOR) -> PERCENT_DESTRUCTOR
  | X (T T_PERCENT_PRINTER) -> PERCENT_PRINTER
  | X (T T_PERCENT_LEFT) -> PERCENT_LEFT
  | X (T T_PERCENT_RIGHT) -> PERCENT_RIGHT
  | X (T T_PERCENT_NONASSOC) -> PERCENT_NONASSOC
  | X (T T_PERCENT_PRECEDENCE) -> PERCENT_PRECEDENCE
  | X (T T_PERCENT_PREC) -> PERCENT_PREC
  | X (T T_PERCENT_DPREC) -> PERCENT_DPREC
  | X (T T_PERCENT_MERGE) -> PERCENT_MERGE
  | X (T T_PERCENT_CODE) -> PERCENT_CODE
  | X (T T_PERCENT_DEFAULT_PREC) -> PERCENT_DEFAULT_PREC
  | X (T T_PERCENT_DEFINE) -> PERCENT_DEFINE
  | X (T T_PERCENT_ERROR_VERBOSE) -> PERCENT_ERROR_VERBOSE
  | X (T T_PERCENT_FILE_PREFIX) -> PERCENT_FILE_PREFIX "%file-prefix"
  | X (T T_PERCENT_FLAG) -> PERCENT_FLAG ""
  | X (T T_PERCENT_NAME_PREFIX) -> PERCENT_NAME_PREFIX "%name-prefix"
  | X (T T_PERCENT_PURE_PARSER) -> PERCENT_PURE_PARSER "%pure-parser"
  | X (T T_PERCENT_EXPECT) -> PERCENT_EXPECT
  | X (T T_PERCENT_EXPECT_RR) -> PERCENT_EXPECT_RR
  | X (T T_PERCENT_GLR_PARSER) -> PERCENT_GLR_PARSER
  | X (T T_PERCENT_HEADER) -> PERCENT_HEADER
  | X (T T_PERCENT_INITIAL_ACTION) -> PERCENT_INITIAL_ACTION
  | X (T T_PERCENT_LANGUAGE) -> PERCENT_LANGUAGE
  | X (T T_PERCENT_NO_DEFAULT_PREC) -> PERCENT_NO_DEFAULT_PREC
  | X (T T_PERCENT_NO_LINES) -> PERCENT_NO_LINES
  | X (T T_PERCENT_NONDETERMINISTIC_PARSER) -> PERCENT_NONDETERMINISTIC_PARSER
  | X (T T_PERCENT_OUTPUT) -> PERCENT_OUTPUT
  | X (T T_PERCENT_REQUIRE) -> PERCENT_REQUIRE
  | X (T T_PERCENT_SKELETON) -> PERCENT_SKELETON
  | X (T T_PERCENT_START) -> PERCENT_START
  | X (T T_PERCENT_TOKEN_TABLE) -> PERCENT_TOKEN_TABLE
  | X (T T_PERCENT_VERBOSE) -> PERCENT_VERBOSE
  | X (T T_PERCENT_YACC) -> PERCENT_YACC
  | X (T T_ID) -> ID ""
  | X (T T_PERCENT_FIXED_OUTPUT_FILES) ->
      PERCENT_FIXED_OUTPUT_FILES "\"y.tab.c\""
  | X (T T_COLON) -> COLON
  | X (T T_EQUAL) -> EQUAL
  | X (T T_PERCENT_PERCENT) -> PERCENT_PERCENT
  | X (T T_PIPE) -> PIPE
  | X (T T_SEMICOLON) -> SEMICOLON
  | X (T T_TAG_ANY) -> TAG_ANY
  | X (T T_TAG_NONE) -> TAG_NONE
  | X (T T_EOF) -> EOF
  | X (T T_PROLOGUE) -> PROLOGUE ("", (dummy_pos, dummy_pos))
  | X (T T_CHAR_LITERAL) -> CHAR_LITERAL ("", (dummy_pos, dummy_pos))
  | X (T T_STRING) -> STRING ("", (dummy_pos, dummy_pos))
  | X (T T_TSTRING) -> TSTRING ("", (dummy_pos, dummy_pos))
  | X (T T_TAG) -> TAG ("", (dummy_pos, dummy_pos))
  | X (T T_BRACED_CODE) -> BRACED_CODE ("", (dummy_pos, dummy_pos))
  | X (T T_BRACED_PREDICATE) -> BRACED_PREDICATE ("", (dummy_pos, dummy_pos))
  | X (T T_BRACKETED_ID) -> BRACKETED_ID ("", (dummy_pos, dummy_pos))
  | X (T T_EPILOGUE) -> EPILOGUE ("", (dummy_pos, dummy_pos))
  | X (T T_INT_LITERAL) -> INT_LITERAL 0
  | X (T T_PERCENT_PARAM) -> PERCENT_PARAM PARAM_BOTH
  | X (T T_PERCENT_UNION) -> PERCENT_UNION
  | X (T T_PERCENT_EMPTY) -> PERCENT_EMPTY
  | X (T T_error) -> ERROR ("", (dummy_pos, dummy_pos))
  | X (T T_ERROR) -> ERROR ("", (dummy_pos, dummy_pos))
  | X (N _) -> assert false

let feed_terminal terminal p1 p2 env =
  let open I in
  match terminal with
  | X (T T_PERCENT_TOKEN) -> feed (T T_PERCENT_TOKEN) p1 () p2 env
  | X (T T_PERCENT_NTERM) -> feed (T T_PERCENT_NTERM) p1 () p2 env
  | X (T T_PERCENT_TYPE) -> feed (T T_PERCENT_TYPE) p1 () p2 env
  | X (T T_PERCENT_DESTRUCTOR) -> feed (T T_PERCENT_DESTRUCTOR) p1 () p2 env
  | X (T T_PERCENT_PRINTER) -> feed (T T_PERCENT_PRINTER) p1 () p2 env
  | X (T T_PERCENT_LEFT) -> feed (T T_PERCENT_LEFT) p1 () p2 env
  | X (T T_PERCENT_RIGHT) -> feed (T T_PERCENT_RIGHT) p1 () p2 env
  | X (T T_PERCENT_NONASSOC) -> feed (T T_PERCENT_NONASSOC) p1 () p2 env
  | X (T T_PERCENT_PRECEDENCE) -> feed (T T_PERCENT_PRECEDENCE) p1 () p2 env
  | X (T T_PERCENT_PREC) -> feed (T T_PERCENT_PREC) p1 () p2 env
  | X (T T_PERCENT_DPREC) -> feed (T T_PERCENT_DPREC) p1 () p2 env
  | X (T T_PERCENT_MERGE) -> feed (T T_PERCENT_MERGE) p1 () p2 env
  | X (T T_PERCENT_CODE) -> feed (T T_PERCENT_CODE) p1 () p2 env
  | X (T T_PERCENT_DEFAULT_PREC) -> feed (T T_PERCENT_DEFAULT_PREC) p1 () p2 env
  | X (T T_PERCENT_DEFINE) -> feed (T T_PERCENT_DEFINE) p1 () p2 env
  | X (T T_PERCENT_ERROR_VERBOSE) ->
      feed (T T_PERCENT_ERROR_VERBOSE) p1 () p2 env
  | X (T T_PERCENT_FILE_PREFIX) ->
      feed (T T_PERCENT_FILE_PREFIX) p1 "%file-prefix" p2 env
  | X (T T_PERCENT_FLAG) -> feed (T T_PERCENT_FLAG) p1 "" p2 env
  | X (T T_PERCENT_NAME_PREFIX) ->
      feed (T T_PERCENT_NAME_PREFIX) p1 "%name-prefix" p2 env
  | X (T T_PERCENT_PURE_PARSER) ->
      feed (T T_PERCENT_PURE_PARSER) p1 "%pure-parser" p2 env
  | X (T T_PERCENT_EXPECT) -> feed (T T_PERCENT_EXPECT) p1 () p2 env
  | X (T T_PERCENT_EXPECT_RR) -> feed (T T_PERCENT_EXPECT_RR) p1 () p2 env
  | X (T T_PERCENT_GLR_PARSER) -> feed (T T_PERCENT_GLR_PARSER) p1 () p2 env
  | X (T T_PERCENT_HEADER) -> feed (T T_PERCENT_HEADER) p1 () p2 env
  | X (T T_PERCENT_INITIAL_ACTION) ->
      feed (T T_PERCENT_INITIAL_ACTION) p1 () p2 env
  | X (T T_PERCENT_LANGUAGE) -> feed (T T_PERCENT_LANGUAGE) p1 () p2 env
  | X (T T_PERCENT_NO_DEFAULT_PREC) ->
      feed (T T_PERCENT_NO_DEFAULT_PREC) p1 () p2 env
  | X (T T_PERCENT_NO_LINES) -> feed (T T_PERCENT_NO_LINES) p1 () p2 env
  | X (T T_PERCENT_NONDETERMINISTIC_PARSER) ->
      feed (T T_PERCENT_NONDETERMINISTIC_PARSER) p1 () p2 env
  | X (T T_PERCENT_OUTPUT) -> feed (T T_PERCENT_OUTPUT) p1 () p2 env
  | X (T T_PERCENT_REQUIRE) -> feed (T T_PERCENT_REQUIRE) p1 () p2 env
  | X (T T_PERCENT_SKELETON) -> feed (T T_PERCENT_SKELETON) p1 () p2 env
  | X (T T_PERCENT_START) -> feed (T T_PERCENT_START) p1 () p2 env
  | X (T T_PERCENT_TOKEN_TABLE) -> feed (T T_PERCENT_TOKEN_TABLE) p1 () p2 env
  | X (T T_PERCENT_VERBOSE) -> feed (T T_PERCENT_VERBOSE) p1 () p2 env
  | X (T T_PERCENT_YACC) -> feed (T T_PERCENT_YACC) p1 () p2 env
  | X (T T_ID) -> feed (T T_ID) p1 "" p2 env
  | X (T T_PERCENT_FIXED_OUTPUT_FILES) ->
      feed (T T_PERCENT_FIXED_OUTPUT_FILES) p1 "\"y.tab.c\"" p2 env
  | X (T T_COLON) -> feed (T T_COLON) p1 () p2 env
  | X (T T_EQUAL) -> feed (T T_EQUAL) p1 () p2 env
  | X (T T_PERCENT_PERCENT) -> feed (T T_PERCENT_PERCENT) p1 () p2 env
  | X (T T_PIPE) -> feed (T T_PIPE) p1 () p2 env
  | X (T T_SEMICOLON) -> feed (T T_SEMICOLON) p1 () p2 env
  | X (T T_TAG_ANY) -> feed (T T_TAG_ANY) p1 () p2 env
  | X (T T_TAG_NONE) -> feed (T T_TAG_NONE) p1 () p2 env
  | X (T T_EOF) -> feed (T T_EOF) p1 () p2 env
  | X (T T_PROLOGUE) -> feed (T T_PROLOGUE) p1 ("", (p1, p2)) p2 env
  | X (T T_CHAR_LITERAL) -> feed (T T_CHAR_LITERAL) p1 ("", (p1, p2)) p2 env
  | X (T T_STRING) -> feed (T T_STRING) p1 ("", (p1, p2)) p2 env
  | X (T T_TSTRING) -> feed (T T_TSTRING) p1 ("", (p1, p2)) p2 env
  | X (T T_TAG) -> feed (T T_TAG) p1 ("", (p1, p2)) p2 env
  | X (T T_BRACED_CODE) -> feed (T T_BRACED_CODE) p1 ("", (p1, p2)) p2 env
  | X (T T_BRACED_PREDICATE) ->
      feed (T T_BRACED_PREDICATE) p1 ("", (p1, p2)) p2 env
  | X (T T_BRACKETED_ID) -> feed (T T_BRACKETED_ID) p1 ("", (p1, p2)) p2 env
  | X (T T_EPILOGUE) -> feed (T T_EPILOGUE) p1 ("", (p1, p2)) p2 env
  | X (T T_INT_LITERAL) -> feed (T T_INT_LITERAL) p1 0 p2 env
  | X (T T_PERCENT_PARAM) -> feed (T T_PERCENT_PARAM) p1 PARAM_BOTH p2 env
  | X (T T_PERCENT_UNION) -> feed (T T_PERCENT_UNION) p1 () p2 env
  | X (T T_PERCENT_EMPTY) -> feed (T T_PERCENT_EMPTY) p1 () p2 env
  | X (T T_error) -> feed (T T_ERROR) p1 ("", (p1, p2)) p2 env
  | X (T T_ERROR) -> feed (T T_ERROR) p1 ("", (p1, p2)) p2 env
  | X (N _) -> env

let get_range startpos endpos =
  let start =
    Position.create
      ~character:(startpos.pos_cnum - startpos.pos_bol)
      ~line:startpos.pos_lnum
  in
  let end_ =
    Position.create
      ~character:(endpos.pos_cnum - endpos.pos_bol)
      ~line:endpos.pos_lnum
  in
  Range.create ~end_ ~start

let success ast = Result.ok ast

let rec fail cnt supplier chkpt1 chkpt2 =
  match chkpt2 with
  | I.HandlingError env2 -> (
      let lposS, lposE = I.positions env2 in
      let pos = (lposS, lposE) in
      match I.top env2 with
      | Some (I.Element (state2, _v, _startpos, _endpos)) -> (
          (if cnt = 0 then
             match supplier () with
             | Parser.EOF, startpos, endpos ->
                 let message = "unexpected end of file" in
                 let range = get_range startpos endpos in
                 append_diagnostics (Diagnostic.create ~message ~range ())
             | _ -> ());
          (* production rules and index (terminal or nonterminal symbols) *)
          let items = I.items state2 in
          (* productions for reduce *)
          let reductions =
            List.fold_left
              (fun acc (prod, i) ->
                let rhs = I.rhs prod in
                (* For example, [token_decls -> token_decl_1 .] this production can reduce to [token_decls] *)
                if i = List.length rhs then prod :: acc else acc)
              [] items
          in
          match reductions with
          | hd :: _ ->
              (* Reduce *)
              let new_env = I.force_reduction hd env2 in
              I.loop_handle_undo success
                (fail (cnt + 1) supplier)
                supplier (I.input_needed new_env)
          | [] -> (
              (* acceptable terminal symbols computed from the FIRST set *)
              let symbols =
                List.fold_left
                  (fun acc (prod, i) ->
                    let rhs = I.rhs prod in
                    if i < List.length rhs then
                      let symbol = List.nth rhs i in
                      let first_terminals =
                        I.foreach_terminal_but_error
                          (fun x acc ->
                            match x with
                            | X (T t) ->
                                if I.xfirst symbol t then x :: acc else acc
                            | X (N _) -> acc)
                          []
                      in
                      first_terminals
                    else acc)
                  [] items
              in
              match symbols with
              | [] -> (
                  (* Pop, if there is no acceptable terminals *)
                  match I.pop env2 with
                  | None -> Result.error (RecoveryError ("Stack is empty", pos))
                  | Some new_env ->
                      I.loop_handle_undo success
                        (fail (cnt + 1) supplier)
                        supplier (I.input_needed new_env))
              | hd :: _ ->
                  let inserted_token = terminal2token hd in
                  if I.acceptable chkpt1 inserted_token dummy_pos then
                    (* Feed (Shift) the token *)
                    let new_env = feed_terminal hd dummy_pos dummy_pos env2 in
                    I.loop_handle_undo success
                      (fail (cnt + 1) supplier)
                      supplier (I.input_needed new_env)
                  else
                    Result.error
                      (RecoveryError ("Acceptable token not found", pos))))
      | _ ->
          Result.error (RecoveryError ("No symbols on top of the stack", pos)))
  | _ -> Result.error (RecoveryError ("Invalid error", (dummy_pos, dummy_pos)))

let entry start lexer lexbuf =
  let supplier = I.lexer_lexbuf_to_supplier lexer lexbuf in
  I.loop_handle_undo success (fail 0 supplier) supplier start
