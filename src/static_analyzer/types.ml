type param_type = PARAM_NONE | PARAM_LEX | PARAM_PARSE | PARAM_BOTH
[@@deriving show]

type symbol_class = UNKNOWN_SYM | NTERM_SYM | TOKEN_SYM | PCT_TYP_SYM
[@@deriving show]

type prec_class =
  | UNKNOWN_PREC
  | LEFT_PREC
  | RIGHT_PREC
  | NONASSOC_PREC
  | PRECEDENCE_PREC
[@@deriving show]

type pos = Lexing.position * Lexing.position

let pp_pos fmt (pos : Lexing.position * Lexing.position) =
  let s = fst pos in
  let e = snd pos in
  Format.fprintf fmt
    "({ line = %d; character = %d }, { line = %d; character = %d })" s.pos_lnum
    (s.pos_cnum - s.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol)

exception SyntaxError of string

type t = {
  prologue_declarations : prologue_declaration;
  grammar_rules : grammar;
  epilogue : epilogue option;
}

(* Declarations. *)
and prologue_declaration =
  | Prologue of { pos : pos; next : prologue_declaration option }
  | PercentFlag of {
      flag : string;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentDefine of {
      variable : string_;
      value : value option;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentHeader of {
      header : string_ option;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentErrorVerbose of { pos : pos; next : prologue_declaration option }
  | PercentExpect of {
      conflict : int_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentExpectRR of {
      conflict : int_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentFilePrefix of {
      prefix : string_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentGlrParser of { pos : pos; next : prologue_declaration option }
  | PercentInitialAction of {
      code : string_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentLanguage of {
      language : string_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentNamePrefix of {
      directive : string_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentNoLines of { pos : pos; next : prologue_declaration option }
  | PercentNondeterministicParser of {
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentOutput of {
      file : string_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentParam of {
      param_type : param_type;
      params : string_ list;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentPureParser of {
      directive : string;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentRequire of {
      version : string_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentSkeleton of {
      file : string_;
      pos : pos;
      next : prologue_declaration option;
    }
  | PercentTokenTable of { pos : pos; next : prologue_declaration option }
  | PercentVerbose of { pos : pos; next : prologue_declaration option }
  | PercentYacc of { pos : pos; next : prologue_declaration option }
  | GrammarDirective of {
      directive : grammar_declaration;
      next : prologue_declaration option;
    }
  | DirectiveList of grammar_declaration list
  | Error of { msg : string; pos : pos; next : prologue_declaration option }

(* grammar declaration *)
and grammar_declaration =
  | PercentStart of { symbols : symbol list; pos : pos }
  | PercentDefaultPrec of pos
  | PercentNoDefaultPrec of pos
  | PercentCode of { id : string_ option; code : string_; pos : pos }
  | PercentUnion of { name : string_ option; code : string_; pos : pos }
  (* code props type *)
  | PercentDestructor of {
      code : string_;
      symlist : symbol_tag list;
      pos : pos;
    }
  | PercentPrinter of { code : string_; symlist : symbol_tag list; pos : pos }
  (* symbol declaration *)
  | PercentNterm of { tag : tag option; id : string_; pos : pos }
  | PercentToken of {
      tag : tag option;
      id : string_;
      number : int_ option;
      alias : string_ option;
      pos : pos;
    }
  | PercentType of { tag : tag option; id : symbol; pos : pos }
  (* precedence declarator *)
  | PercentLeft of {
      tag : tag option;
      id : string_;
      number : int_ option;
      pos : pos;
    }
  | PercentRight of {
      tag : tag option;
      id : string_;
      number : int_ option;
      pos : pos;
    }
  | PercentNonassoc of {
      tag : tag option;
      id : string_;
      number : int_ option;
      pos : pos;
    }
  | PercentPrecedence of {
      tag : tag option;
      id : string_;
      number : int_ option;
      pos : pos;
    }
  | GrammarDeclarationList of grammar_declaration list

and string_ = string * pos
and int_ = int * pos

and value =
  | ValID of (string * pos)
  | ValString of (string * pos)
  | ValCode of (string * pos)

and symbol = SymID of (string * pos) | SymString of (string * pos)
and tag = TagAny | TagNone | TagName of (string * pos)
and symbol_tag = Sym of symbol | Tag of tag

(* grammar *)
and grammar =
  | Rule of {
      id : string_;
      named_ref : string_ option;
      rhs : rhs list;
      pos : pos;
      next : grammar option;
    }
  | GrammarDecl of { directive : grammar_declaration; next : grammar option }
  | DeclList of grammar_declaration list

and rhs =
  | RhsSym of { symbol : symbol; named_ref : string_ option }
  | RhsMidrule of {
      tag : tag option;
      code : string_;
      named_ref : string_ option;
    }
  | RhsPredicate of string_
  | RhsPercentEmpty of pos
  | RhsPercentPrec of symbol
  | RhsPercentDprec of int_
  | RhsPercentMerge of tag
  | RhsPercentExpect of int_
  | RhsPercentExpectRR of int_

and epilogue = string_ [@@deriving show]
