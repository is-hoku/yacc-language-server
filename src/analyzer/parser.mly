(*
    Yacc Language Server

    Copyright (C) 2002-2015, 2018-2021 Free Software Foundation, Inc.
    Copyright (C) 2023-2024, Hoku Ishibe.

    This file is a port of the Bison 3.8.1 implementation.
    This software is released under the GNU General Public License.
    see https://www.gnu.org/licenses/
*)

%{
open Types

let dummy_string_ = ("", (Lexing.dummy_pos, Lexing.dummy_pos))
let current_class = ref (UNKNOWN_SYM (Lexing.dummy_pos, Lexing.dummy_pos))
let current_prec = ref UNKNOWN_PREC

let tagging tag syms =
    List.map (fun s ->
        match s with
        | PercentNterm p -> PercentNterm{ p with tag = Some tag }
        | PercentToken p -> PercentToken{ p with tag = Some tag }
        | PercentType p -> PercentType{ p with tag = Some tag }
        | _ -> raise (SyntaxError "invalid directive")
    ) syms

let rec prologuedeclarations = function
    | [] -> None
    | x :: xs ->
        match x with
        | Prologue p -> Some (Prologue{ p with next = (prologuedeclarations xs) })
        | PercentFlag p -> Some (PercentFlag{ p with next = (prologuedeclarations xs) })
        | PercentDefine p -> Some (PercentDefine{ p with next = (prologuedeclarations xs) })
        | PercentHeader p -> Some (PercentHeader{ p with next = (prologuedeclarations xs) })
        | PercentErrorVerbose p -> Some (PercentErrorVerbose{ p with next = (prologuedeclarations xs) })
        | PercentExpect p -> Some (PercentExpect{ p with next = (prologuedeclarations xs) })
        | PercentExpectRR p -> Some (PercentExpectRR{ p with next = (prologuedeclarations xs) })
        | PercentFilePrefix p -> Some (PercentFilePrefix{ p with next = (prologuedeclarations xs) })
        | PercentGlrParser p -> Some (PercentGlrParser{ p with next = (prologuedeclarations xs) })
        | PercentInitialAction p -> Some (PercentInitialAction { p with next = (prologuedeclarations xs) })
        | PercentLanguage p -> Some (PercentLanguage{ p with next = (prologuedeclarations xs) })
        | PercentNamePrefix p -> Some (PercentNamePrefix{ p with next = (prologuedeclarations xs) })
        | PercentNoLines p -> Some (PercentNoLines{ p with next = (prologuedeclarations xs) })
        | PercentNondeterministicParser p -> Some (PercentNondeterministicParser{ p with next = (prologuedeclarations xs) })
        | PercentOutput p -> Some (PercentOutput{ p with next = (prologuedeclarations xs) })
        | PercentParam p -> Some (PercentParam{ p with next = (prologuedeclarations xs) })
        | PercentPureParser p -> Some (PercentPureParser{ p with next = (prologuedeclarations xs) })
        | PercentRequire p -> Some (PercentRequire{ p with next = (prologuedeclarations xs) })
        | PercentSkeleton p -> Some (PercentSkeleton{ p with next = (prologuedeclarations xs) })
        | PercentTokenTable p -> Some (PercentTokenTable{ p with next = (prologuedeclarations xs) })
        | PercentVerbose p -> Some (PercentVerbose{ p with next = (prologuedeclarations xs) })
        | PercentYacc p -> Some (PercentYacc{ p with next = (prologuedeclarations xs) })
        | GrammarDirective _ as first ->
                let rec append v =
                      match v with
                      | GrammarDirective { next = Some next; directive } ->
                              Some (GrammarDirective{ directive; next = append next })
                      | GrammarDirective { next = None; directive } ->
                              Some (GrammarDirective{ directive; next = (prologuedeclarations xs) })
                      | _ -> raise (SyntaxError "invalid input")
                in
                append first
        | _ -> raise (SyntaxError "invalid input")

let rec grammarrules = function
    | [] -> None
    | x :: xs ->
        match x with
        | Rule p -> Some (Rule{ p with next = (grammarrules xs) })
        | GrammarDecl p -> Some (GrammarDecl{ p with next = (grammarrules xs) })
        | _ -> raise (SyntaxError "invalid input")

let rec grammardirective = function
    | [] -> None
    | x :: xs -> Some (GrammarDirective{ directive = x; next = grammardirective xs })

let rec grammardecl = function
    | [] -> None
    | x :: xs -> Some (GrammarDecl{ directive = x; next = grammardecl xs })
%}

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
ID                "identifier"
(*ID_COLON          "identifier:"*)
PERCENT_FIXED_OUTPUT_FILES  "%fixed-output-files"

%token
COLON             ":"
EQUAL             "="
PERCENT_PERCENT   "%%"
PIPE              "|"
SEMICOLON         ";"
TAG_ANY     "<*>"
TAG_NONE    "<>"
EOF         "eof"

%token <string * Types.pos>
PROLOGUE          "%{...%}"
CHAR_LITERAL      "character literal"
STRING            "string"
TSTRING           "translatable string"
TAG               "<tag>"
BRACED_CODE       "{...}"
BRACED_PREDICATE  "%?{...}"
BRACKETED_ID      "[identifier]"
EPILOGUE          "epilogue"

%type <Types.tag option> tag_opt
%token <int> INT_LITERAL "integer literal"

(* %param. *)
%token <Types.param_type> PERCENT_PARAM "%param"


(* Grammar. *)
%token PERCENT_UNION "%union"
%type <(string * Types.pos) option> string_opt
%type <(int * Types.pos)  option> int_opt
%token PERCENT_EMPTY "%empty"

(* Error (ignored token) *)
%token <string * Types.pos> ERROR

%type <Types.t> input
%start input

%%

input:
| p=prologue_declarations; "%%" g=grammar; e=epilogue_opt "eof" {
    let pd =
        match (prologuedeclarations p) with
        | None -> raise (SyntaxError "empty prologue/declarations")
        | Some v -> v
    in
    let gr =
        match (grammarrules g) with
        | None -> raise (SyntaxError "empty prologue/declarations")
        | Some v -> v
    in
    {
        prologue_declarations = pd;
        grammar_rules = gr;
        epilogue = e;
    }
}
;

(* Declarations: before the first %%. *)
prologue_declarations:
| { [] }
| p1=prologue_declarations; p2=prologue_declaration {
    p1 @ [ p2 ]
}
;

prologue_declaration:
| grammar_declaration {
    match $1 with
    | GrammarDeclarationList s -> (
        match (grammardirective s) with
        | None -> raise (SyntaxError "empty directives")
        | Some v -> v
    )
    | _ -> GrammarDirective{ directive = $1; next = None }
}
| "%{...%}" { Prologue{ code = fst $1; pos = snd $1; next = None } }
| "%<flag>" { PercentFlag{ flag = $1; pos = $loc; next = None} }
| "%define"; var=variable; v=value { PercentDefine{ variable = var; value = v; pos = $loc($1); next = None } }
| "%header"; header=string_opt { PercentHeader{ header; pos = $loc($1); next = None } }
| "%error-verbose" { PercentErrorVerbose{ pos = $loc; next = None} }
| "%expect"; i=INT_LITERAL { PercentExpect{ conflict = (i, $loc(i)); pos = $loc($1); next = None } }
| "%expect-rr"; i=INT_LITERAL { PercentExpectRR{ conflict = (i, $loc(i)); pos = $loc($1); next = None} }
| "%file-prefix"; prefix=STRING { PercentFilePrefix{ prefix; pos = $loc($1); next = None } }
| "%glr-parser" { PercentGlrParser{ pos = $loc; next = None } }
| "%initial-action"; code="{...}" { PercentInitialAction{ code; pos = $loc($1); next = None } }
| "%language"; language=STRING { PercentLanguage{ language; pos = $loc($1); next = None } }
| "%name-prefix"; directive=STRING { PercentNamePrefix{ directive; pos = $loc($1); next = None } }
| "%no-lines" { PercentNoLines{ pos = $loc; next = None } }
| "%nondeterministic-parser" { PercentNondeterministicParser{ pos = $loc; next = None } }
| "%output"; file=STRING { PercentOutput{ file; pos = $loc($1); next = None } }
| "%fixed-output-files" { PercentOutput{ file = ($1, $loc); pos = $loc($1); next = None } }
| param_type="%param"; params=params { PercentParam{ param_type; params; pos = $loc(param_type); next = None } }
| "%pure-parser" { PercentPureParser{ directive = $1; pos = $loc; next = None } }
| "%require"; version=STRING { PercentRequire{ version; pos = $loc($1); next = None } }
| "%skeleton"; file=STRING { PercentSkeleton{ file; pos = $loc($1); next = None } }
| "%token-table" { PercentTokenTable{ pos = $loc; next = None } }
| "%verbose" { PercentVerbose{ pos = $loc; next = None } }
| "%yacc" { PercentYacc{ pos = $loc; next = None } }
;

params:
| params; p="{...}" { $1 @ [ p ] }
| "{...}" { [ $1 ] }
;

(* grammar_declaration. *)
grammar_declaration:
| symbol_declaration { GrammarDeclarationList ($1) }
| "%start"; symbols=symbols_1 { PercentStart{ symbols; pos = $loc($1) } }
| p=code_props_type; code="{...}"; symlist=generic_symlist {
    match p with
    | PercentDestructor v -> PercentDestructor{ v with code; symlist } 
    | PercentPrinter v -> PercentPrinter{ v with code; symlist }
    | _ -> raise (SyntaxError "invalid directive")
}
| "%default-prec" { PercentDefaultPrec($loc) }
| "%no-default-prec" { PercentNoDefaultPrec($loc) }
| "%code"; code="{...}" { PercentCode{ id = None; code; pos = $loc($1) } }
| "%code"; id=ID; code="{...}" { PercentCode{ id = Some (id, $loc(id)); code; pos = $loc($1) } }
| "%union"; name=union_name; code="{...}" { PercentUnion{ name; code; pos = $loc($1) } }
;

code_props_type:
| "%destructor" { PercentDestructor{ code = dummy_string_; symlist = []; pos = $loc } }
| "%printer" { PercentPrinter{ code = dummy_string_; symlist = []; pos = $loc } }
;

(* %union. *)
union_name:
| { None }
| id=ID { Some (id, $loc) }
;

symbol_declaration:
| percent_symbol syms=token_decls { syms }
| precedence_declarator syms=token_decls_for_prec { syms }
;

percent_symbol:
| "%nterm"  { current_class := NTERM_SYM $loc }
| "%token"  { current_class := TOKEN_SYM $loc }
| "%type"   { current_class := PCT_TYP_SYM $loc }

precedence_declarator:
| "%left"       { current_prec := LEFT_PREC }
| "%right"      { current_prec := RIGHT_PREC }
| "%nonassoc"   { current_prec := NONASSOC_PREC }
| "%precedence" { current_prec := PRECEDENCE_PREC }
;

string_opt:
| { None }
| STRING { Some $1 }
;

tag_opt:
| { None }
| TAG { Some (TagName ($1)) }
;

generic_symlist:
| generic_symlist_item { [ $1 ] }
| generic_symlist generic_symlist_item { $1 @ [ $2 ] }
;

generic_symlist_item:
| symbol { Sym $1 }
| tag { Tag $1 }
;

tag:
| TAG { TagName ($1) }
| "<*>" { TagAny }
| "<>" { TagNone }

(* nterm_decls (%nterm). *)
nterm_decls:
| token_decls { $1 }
;

(* token_decls (%token, and %nterm). *)
token_decls:
| syms=token_decl_1 { syms }
| t=TAG; syms=token_decl_1 { tagging (TagName t) syms }
| tds1=token_decls; t=TAG; tds2=token_decl_1 {
    tds1 @ (tagging (TagName t) tds2)
}
;

(* One or more symbol declarations for %token or %nterm. *)
token_decl_1:
| token_decl { [ $1 ] }
| td=token_decl_1; directive=token_decl {
    td @ [ directive ]
}

(* One symbol declaration for %token or %nterm. *)
token_decl:
| id=id; number=int_opt; alias=alias {
    match !current_class with
    | NTERM_SYM pos -> PercentNterm{ tag = None; id; pos }
    | TOKEN_SYM pos -> PercentToken{ tag = None; id; number; alias; pos }
    | _ -> raise (SyntaxError "invalid type")
}
;

int_opt:
| { None }
| INT_LITERAL { Some ($1, $loc) }
;

alias:
| { None }
| string_as_id { Some $1 }
| TSTRING { Some $1 }
;

(* token_decls_for_prec (%left, etc.). *)
token_decls_for_prec:
| syms=token_decl_for_prec_1 { syms }
| t=TAG; syms=token_decl_for_prec_1 { tagging (TagName t) syms }
| tds1=token_decls_for_prec; t=TAG; tds2=token_decl_for_prec_1 {
    tds1 @ (tagging (TagName t) tds2)
}
;

(* One or more token declarations for precedence declaration. *)
token_decl_for_prec_1:
| token_decl_for_prec { [ $1 ] }
| td=token_decl_for_prec_1; directive=token_decl_for_prec {
    td @ [ directive ]
}

(* One token declaration for precedence declaration. *)
token_decl_for_prec:
| id=id; number=int_opt {
    match !current_prec with
    | LEFT_PREC -> PercentLeft{ tag = None; id; number; pos = $loc }
    | RIGHT_PREC -> PercentRight{ tag = None; id; number; pos = $loc }
    | NONASSOC_PREC -> PercentNonassoc{ tag = None; id; number; pos = $loc }
    | PRECEDENCE_PREC -> PercentPrecedence{ tag = None; id; number; pos = $loc }
    | UNKNOWN_PREC -> raise (SyntaxError "prec directives not found")
}
| id=string_as_id {
    match !current_prec with
    | LEFT_PREC -> PercentLeft{ tag = None; id; number = None; pos = $loc }
    | RIGHT_PREC -> PercentRight{ tag = None; id; number = None; pos = $loc }
    | NONASSOC_PREC -> PercentNonassoc{ tag = None; id; number = None; pos = $loc }
    | PRECEDENCE_PREC -> PercentPrecedence{ tag = None; id; number = None; pos = $loc }
    | UNKNOWN_PREC -> raise (SyntaxError "prec directives not found")
}
;


(* symbol_decls (argument of %type). *)

(* A non empty list of typed symbols (for %type). *)
symbol_decls:
| syms=symbols_1 { List.map (fun id -> PercentType{ tag = None; id; pos = $loc }) syms }
| sds1=symbol_decls; t=TAG; sds2=symbols_1 {
    sds1 @ (tagging (TagName t) (List.map (fun id -> PercentType{ tag = None; id; pos = $loc }) sds2))
}
;

(* One or more symbols. *)
symbols_1:
| symbol { [ $1 ] }
| ss=symbols_1; s=symbol { ss @ [ s ] }
;


(* The grammar section: between the two %%. *)

grammar:
| rules_or_grammar_declaration { [ $1 ] }
| g=grammar; rg=rules_or_grammar_declaration {
    g @ [ rg ]
}
;

(* As a Bison extension, one can use the grammar declarations in the body of the grammar. *)
rules_or_grammar_declaration:
| rules { $1 }
| grammar_declaration ";" {
    match $1 with
    | GrammarDeclarationList s -> (
        match (grammardecl s) with
        | None -> raise (SyntaxError "empty directives")
        | Some v -> v
    )
    | _ -> GrammarDecl{ directive = $1; next = None }
}
;

rules:
(*| id=id_colon; named_ref=named_ref_opt; ":"; rhs=rhses_1 {
    Rule{ id; named_ref; rhs; pos=$loc; next=None }
}*)
| id=id; named_ref=named_ref_opt; ":"; rhs=rhses_1 {
    Rule{ id; named_ref; rhs; pos=$loc; next=None }
}
;

rhses_1:
| rhs { $1 }
| r1=rhses_1; "|"; r2=rhs { r1 @ r2 }
| rhses_1 ";" { $1 }
;

rhs:
| { [] }
| rhs; symbol=symbol; named_ref=named_ref_opt { $1 @ [ RhsSym{ symbol; named_ref } ] }
| rhs; tag=tag_opt; code="{...}"; named_ref=named_ref_opt {
    $1 @ [ RhsMidrule{tag; code; named_ref} ]
}
| rhs; "%?{...}" { $1 @ [ RhsPredicate $2 ] }
| rhs; "%empty" { $1 @ [ RhsPercentEmpty ($loc($2)) ] }
| rhs; "%prec"; s=symbol { $1 @ [ RhsPercentPrec s ] }
| rhs; "%dprec"; i=INT_LITERAL { $1 @ [ RhsPercentDprec (i, $loc(i)) ] }
| rhs; "%merge"; t=TAG { $1 @ [ RhsPercentMerge (TagName t) ] }
| rhs; "%expect"; i=INT_LITERAL { $1 @ [ RhsPercentExpect (i, $loc(i)) ] }
| rhs; "%expect-rr"; i=INT_LITERAL { $1 @ [ RhsPercentExpectRR (i, $loc(i)) ] }
;

named_ref_opt:
| { None }
| BRACKETED_ID { Some $1 }
;


(* variable and value. *)

variable:
| ID { ($1, $loc) }
;

(* Some content or empty by default. *)
value:
| { None }
| ID { Some (ValID($1, $loc)) }
| STRING { Some (ValString($1)) }
| "{...}" { Some (ValCode $1) }
;


(* Identifiers. *)

id:
| ID { ($1, $loc) }
| CHAR_LITERAL { (fst $1, snd $1) }
;

(*
id_colon:
| ID_COLON { ($1, $loc) }
;
*)

symbol:
| id { SymID $1 }
| string_as_id { SymString $1 }
;

(* A string used as an ID. *)
string_as_id:
| STRING { $1 }
;

epilogue_opt:
| { None }
| "%%"; e=EPILOGUE { Some e }
;
