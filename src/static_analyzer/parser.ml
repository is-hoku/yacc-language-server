open Lexing

exception Error of (position * position)

module I = Syntax.MenhirInterpreter

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
        | T T_PERCENT_PARAM -> Types.show_param_type v
        | T T_PERCENT_UNION -> "%union"
        | T T_PERCENT_EMPTY -> "%empty"
        | T T_error -> "error"
        | T T_ERROR -> fst v
        | N N_input -> Types.show v
        | N N_prologue_declarations ->
            String.concat ", " (List.map Types.show_prologue_declaration v)
        | N N_prologue_declaration -> Types.show_prologue_declaration v
        | N N_params -> String.concat ", " (List.map Types.show_string_ v)
        | N N_grammar_declaration -> Types.show_grammar_declaration v
        | N N_code_props_type -> Types.show_grammar_declaration v
        | N N_union_name -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (Types.show_string_ s)
            | None -> "None")
        | N N_symbol_declaration ->
            String.concat ", " (List.map Types.show_grammar_declaration v)
        | N N_percent_symbol -> "(percent_symbol)"
        | N N_precedence_declarator -> "(precedence_declarator)"
        | N N_string_opt -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (fst s)
            | None -> "None")
        | N N_tag_opt -> (
            match v with
            | Some tag -> Printf.sprintf "Some (%s)" (Types.show_tag tag)
            | None -> "None")
        | N N_generic_symlist ->
            String.concat ", " (List.map Types.show_symbol_tag v)
        | N N_generic_symlist_item -> Types.show_symbol_tag v
        | N N_tag -> Types.show_tag v
        | N N_token_decls ->
            String.concat ", " (List.map Types.show_grammar_declaration v)
        | N N_token_decl_1 ->
            String.concat ", " (List.map Types.show_grammar_declaration v)
        | N N_token_decl -> Types.show_grammar_declaration v
        | N N_int_opt -> (
            match v with
            | Some i -> Printf.sprintf "Some (%d)" (fst i)
            | None -> "None")
        | N N_alias -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (Types.show_string_ s)
            | None -> "None")
        | N N_token_decls_for_prec ->
            String.concat ", " (List.map Types.show_grammar_declaration v)
        | N N_token_decl_for_prec_1 ->
            String.concat ", " (List.map Types.show_grammar_declaration v)
        | N N_token_decl_for_prec -> Types.show_grammar_declaration v
        | N N_symbols_1 -> String.concat ", " (List.map Types.show_symbol v)
        | N N_grammar -> String.concat ", " (List.map Types.show_grammar v)
        | N N_rules_or_grammar_declaration -> Types.show_grammar v
        | N N_rules -> Types.show_grammar v
        | N N_rhses_1 -> String.concat ", " (List.map Types.show_rhs v)
        | N N_rhs -> String.concat ", " (List.map Types.show_rhs v)
        | N N_named_ref_opt -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (Types.show_string_ s)
            | None -> "None")
        | N N_variable -> Types.show_string_ v
        | N N_value -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (Types.show_value s)
            | None -> "None")
        | N N_id -> Types.show_string_ v
        | N N_symbol -> Types.show_symbol v
        | N N_string_as_id -> Types.show_string_ v
        | N N_epilogue_opt -> (
            match v with
            | Some s -> Printf.sprintf "Some (%s)" (Types.show_string_ s)
            | None -> "None"))

  let print s = prerr_string s

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

  let print_element = Some (fun s -> print (string_of_element s))
  let print_symbol s = print (string_of_symbol s)
end

module P = MenhirLib.Printers.Make (I) (Printer)

let success ast = ast

let fail chkpt =
  match chkpt with
  | I.HandlingError penv -> (
      print_endline "handling error!!!!";
      let lposS, lposE = I.positions penv in
      let pos = (lposS, lposE) in
      match I.top penv with
      | Some (I.Element (_state, _v, _startpos, _endpos)) ->
          P.print_current_state penv;
          raise (Error pos)
      | _ -> raise (Error pos))
  | I.InputNeeded penv ->
      print_endline "input needed!!!!";
      let lposS, lposE = I.positions penv in
      let pos = (lposS, lposE) in
      raise (Error pos)
  | _ -> assert false

let process fname lexbuf =
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname } in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read_token lexbuf in
  I.loop_handle success fail supplier
    (Syntax.Incremental.input lexbuf.Lexing.lex_curr_p)
