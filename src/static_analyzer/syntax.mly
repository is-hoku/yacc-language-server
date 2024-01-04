%{
open Types
let current_param = ref PARAM_NONE
let current_class = ref UNKNOWN_SYM
%}

%token <string>
STRING     "string"
TSTRING    "translatable string"

%token
PERCENT_TOKEN       "%token"
PERCENT_NTERM       "%nterm"

PERCENT_TYPE        "%type"
PERCENT_DESTRUCTOR  "%destructor"
PERCENT_PRINTER     "%printer"

PERCENT_LEFT        "%left"
PERCENT_RIGHT       "%right"
PERCENT_NONASSOC    "%nonassoc"
PERCENT_PRECEDENCE  "%precedence"

PERCENT_PREC        "%prec"
PERCENT_DPREC       "%dprec"
PERCENT_MERGE       "%merge"

PERCENT_CODE            "%code"
PERCENT_DEFAULT_PREC    "%default-prec"
PERCENT_DEFINE          "%define"
PERCENT_ERROR_VERBOSE   "%error-verbose"

%token <string>
PERCENT_FILE_PREFIX     "%file-prefix"
PERCENT_FLAG            "%<flag>"
PERCENT_NAME_PREFIX     "%name-prefix"
PERCENT_PURE_PARSER     "%pure-parser"

%token
PERCENT_EXPECT          "%expect"
PERCENT_EXPECT_RR       "%expect-rr"
PERCENT_GLR_PARSER      "%glr-parser"
PERCENT_HEADER          "%header"
PERCENT_INITIAL_ACTION  "%initial-action"
PERCENT_LANGUAGE        "%language"
PERCENT_NO_DEFAULT_PREC "%no-default-prec"
PERCENT_NO_LINES        "%no-lines"
PERCENT_NONDETERMINISTIC_PARSER "%nondeterministic-parser"
PERCENT_OUTPUT          "%output"
PERCENT_REQUIRE         "%require"
PERCENT_SKELETON        "%skeleton"
PERCENT_START           "%start"
PERCENT_TOKEN_TABLE     "%token-table"
PERCENT_VERBOSE         "%verbose"
PERCENT_YACC            "%yacc"

%token <string>
BRACED_CODE       "{...}"
BRACED_PREDICATE  "%?{...}"
BRACKETED_ID      "[identifier]"
EPILOGUE          "epilogue"
ID                "identifier"
ID_COLON          "identifier:"
PROLOGUE          "%{...%}"
TAG               "<tag>"

%token
COLON             ":"
EQUAL             "="
PERCENT_PERCENT   "%%"
PIPE              "|"
SEMICOLON         ";"
TAG_ANY           "<*>"
TAG_NONE          "<>"

%token <char> CHAR_LITERAL "character literal"
%type <string> tag_opt variable
%token <int> INT_LITERAL "integer literal"
(*%type <symbol> id id_colon string_as_id symbol token_decl token_decl_for_prec*)
(*%type <assoc> precedence_declarator*)
(*%type <named_ref> named_ref_opt*)

(* %param. *)
%token <Types.param_type> PERCENT_PARAM "%param"


(* Grammar. *)
(*%type <code_props_type> code_props_type;*)
%token PERCENT_UNION "%union"
(*%type <symbol_list> nterm_decls symbol_decls symbols_1 token_decls token_decls_for_prec token_decl_1 token_decl_for_prec_1;*)
%type <string> string_opt
(*%type <symbol_list> generic_symlist generic_symlist_item;*)
%type <int> int_opt
(*%type <symbol> alias;*)
%token PERCENT_EMPTY "%empty"
(*%type <value_type> value;*)

%type <unit> input
%start input

%%

input:
| prologue_declarations "%%" grammar epilogue_opt {}
;

(* Declarations: before the first %%. *)
prologue_declarations:
| {}
| prologue_declarations prologue_declaration {}
;

prologue_declaration:
| grammar_declaration {}
| "%{...%}" {}
| "%<flag>" {}
| "%define" variable value {}
| "%header" string_opt {}
| "%error-verbose" {}
| "%expect" INT_LITERAL {}
| "%expect-rr" INT_LITERAL {}
| "%file-prefix" STRING {}
| "%glr-parser" {}
| "%initial-action" "{...}" {}
| "%language" STRING {}
| "%name-prefix" STRING {}
| "%no-lines" {}
| "%nondeterministic-parser" {}
| "%output" STRING {}
| midrule("%param" { current_param := $1; }) params { current_param := PARAM_NONE }
| "%pure-parser" {}
| "%require" STRING {}
| "%skeleton" STRING {}
| "%token-table" {}
| "%verbose" {}
| "%yacc" {}
| error ";" {}
;

params:
| params "{...}" {}
| "{...}" {}
;

(* grammar_declaration. *)
grammar_declaration:
| symbol_declaration {}
| "%start" symbols_1 {}
| code_props_type "{...}" generic_symlist {}
| "%default-prec" {}
| "%no-default-prec" {}
| "%code" "{...}" {}
| "%code" ID "{...}" {}
| "%union" union_name "{...}" {}
;

code_props_type:
| "%destructor" {}
| "%printer" {}
;

(* %union. *)
union_name:
| {}
| ID {}
;

symbol_declaration:
| "%nterm" midrule({ current_class := NTERM_SYM }) syms=nterm_decls {}
| "%token" midrule({ current_class := TOKEN_SYM }) syms=token_decls {}
| "%type" midrule({ current_class := PCT_TYP_SYM }) syms=symbol_decls {}
| precedence_declarator syms=token_decls_for_prec {}
;

precedence_declarator:
| "%left" {}
| "%right" {}
| "%nonassoc" {}
| "%precedence" {}
;

string_opt:
| {}
| STRING {}
;

tag_opt:
| {}
| TAG {}
;

generic_symlist:
| generic_symlist_item {}
| generic_symlist generic_symlist_item {}
;

generic_symlist_item:
| symbol {}
| tag {}
;

tag:
| TAG {}
| "<*>" {}
| "<>" {}

(* nterm_decls (%nterm). *)
nterm_decls:
| token_decls {}
;

(* token_decls (%token, and %nterm). *)
token_decls:
| syms=token_decl_1 {}
| TAG syms=token_decl_1 {}
| token_decls TAG syms=token_decl_1 {}
;

(* One or more symbol declarations for %token or %nterm. *)
token_decl_1:
| token_decl {}
| token_decl_1 token_decl {}

(* One symbol declaration for %token or %nterm. *)
token_decl:
| id num=int_opt alias {}
;

int_opt:
| {}
| INT_LITERAL {}
;

alias:
| {}
| string_as_id {}
| TSTRING {}
;

(* token_decls_for_prec (%left, etc.). *)
token_decls_for_prec:
| syms=token_decl_for_prec_1 {}
| TAG syms=token_decl_for_prec_1 {}
| token_decls_for_prec TAG syms=token_decl_for_prec_1 {}
;

(* One or more token declarations for precedence declaration. *)
token_decl_for_prec_1:
| token_decl_for_prec {}
| token_decl_for_prec_1 token_decl_for_prec {}

(* One token declaration for precedence declaration. *)
token_decl_for_prec:
| id num=int_opt {}
| string_as_id {}
;


(* symbol_decls (argument of %type). *)

(* A non empty list of typed symbols (for %type). *)
symbol_decls:
| syms=symbols_1 {}
| TAG syms=symbols_1 {}
| symbol_decls TAG syms=symbols_1 {}
;

(* One or more symbols. *)
symbols_1:
| symbol {}
| symbols_1 symbol {}
;


(* The grammar section: between the two %%. *)

grammar:
| rules_or_grammar_declaration {}
| grammar rules_or_grammar_declaration {}
;

(* As a Bison extension, one can use the grammar declarations in the body of the grammar. *)
rules_or_grammar_declaration:
| rules {}
| grammar_declaration ";" {}
| error ";" {}
;

rules:
| id_colon named_ref_opt midrule({(* current_lhs($1, @1, $2)*) }) ":" rhses_1 {}
;

rhses_1:
| rhs {}
| rhses_1 "|" rhs {}
| rhses_1 ";" {}
;

rhs:
| {}
| rhs symbol named_ref_opt {}
| rhs tag_opt action="{...}" name=named_ref_opt {}
| rhs "%?{...}" {}
| rhs "%empty" {}
| rhs "%prec" symbol {}
| rhs "%dprec" INT_LITERAL {}
| rhs "%merge" TAG {}
| rhs "%expect" INT_LITERAL {}
| rhs "%expect-rr" INT_LITERAL {}
;

named_ref_opt:
| {}
| BRACKETED_ID {}
;


(* variable and value. *)

variable:
| ID {}
;

(* Some content or empty by default. *)
value:
| {}
| ID {}
| STRING {}
| "{...}" {}
;


(* Identifiers. *)

id:
| ID {}
| CHAR_LITERAL {}
;

id_colon:
| ID_COLON {}
;

symbol:
| id {}
| string_as_id {}
;

(* A string used as an ID. *)
string_as_id:
| STRING {}
;

epilogue_opt:
| {}
| "%%" EPILOGUE {}
;
