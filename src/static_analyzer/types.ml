open Language_server.Import

type param_type = PARAM_NONE | PARAM_LEX | PARAM_PARSE | PARAM_BOTH
type symbol_class = UNKNOWN_SYM | NTERM_SYM | TOKEN_SYM | PCT_TYP_SYM

type input = {
  prologu_declarations : prologue_declaration;
  grammar : grammar;
  epilogue : epilogue option;
}

(* Declarations. *)
and prologue_declaration =
  | Prologue of { range : Range.t; next : prologue_declaration option option }
  | PercentFlag of {
      flag : string;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentDefine of {
      variable : string_;
      value : value option;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentHeader of {
      header : string_ option;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentErrorVerbose of {
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentExpect of {
      conflict : int_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentExpectRR of {
      conflict : int_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentFilePrefix of {
      prefix : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentGlrParser of { range : Range.t; next : prologue_declaration option }
  | PercentInitialAction of {
      code : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentLanguage of {
      language : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentNamePrefix of {
      directive : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentNoLines of { range : Range.t; next : prologue_declaration option }
  | PercentNondeterministicParser of {
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentOutput of {
      file : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentParam of {
      params : string_ list;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentPureParser of {
      directive : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentRequire of {
      version : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentSkeleton of {
      file : string_;
      range : Range.t;
      next : prologue_declaration option;
    }
  | PercentTokenTable of { range : Range.t; next : prologue_declaration option }
  | PercentVerbose of { range : Range.t; next : prologue_declaration option }
  | PercentYacc of { range : Range.t; next : prologue_declaration option }
  | GrammarDirective of {
      directive : grammar_declaration;
      next : prologue_declaration option;
    }

(* grammar declaration *)
and grammar_declaration =
  | PercentStart of { symbol : symbol; range : Range.t }
  | PercentDefaultPrec of Range.t
  | PercentCode of { id : string_ option; code : string_; range : Range.t }
  | PercentUnion of { name : string_; code : string_; range : Range.t }
  (* code props type *)
  | PercentDestructor of {
      code : string_;
      symlist : symbol_tag list;
      range : Range.t;
    }
  | PercentPrinter of {
      code : string_;
      symlist : symbol_tag list;
      range : Range.t;
    }
  (* symbol declaration *)
  | PercentNterm of { tag : tag option; id : string_; range : Range.t }
  | PercentToken of {
      tag : tag option;
      id : string_;
      number : int_ option;
      alias : string_ option;
      range : Range.t;
    }
  | PercentType of { tag : tag option; id : string_; range : Range.t }
  (* precedence declarator *)
  | PercentLeft of {
      tag : tag option;
      id : string_;
      number : int_ option;
      range : Range.t;
    }
  | PercentRight of {
      tag : tag option;
      id : string_;
      number : int_ option;
      range : Range.t;
    }
  | PercentNonassoc of {
      tag : tag option;
      id : string_;
      number : int_ option;
      range : Range.t;
    }
  | PercentPrecedence of {
      tag : tag option;
      id : string_;
      number : int_ option;
      range : Range.t;
    }

and string_ = string * Range.t
and int_ = int * Range.t

and value =
  | ValID of (string * Range.t)
  | ValString of (string * Range.t)
  | ValCode of (string * Range.t)

and symbol = SymID of (string * Range.t) | SymString of (string * Range.t)
and tag = TagAny | TagNone | TagName of (string * Range.t)
and symbol_tag = Sym of symbol | Tag of tag

(* grammar *)
and grammar =
  | Rule of {
      id : string_;
      named_ref : string_ option;
      rhs : rhs list;
      range : Range.t;
      next : grammar option;
    }
  | GrammarDecl of { directive : grammar_declaration; next : grammar option }
  | Error of { msg : string; range : Range.t; next : grammar option }

and rhs =
  | RhsSym of { symbol : symbol; named_ref : string_ option }
  | RhsMidrule of {
      tag : tag option;
      code : string_;
      named_ref : string_ option;
    }
  | RhsPredicate of string_
  | RhsPercentEmpty of Range.t
  | RhsPercentPrec of symbol
  | RhsPercentDprec of int_
  | RhsPercentMerge of tag
  | RhsPercentExpect of int_
  | RhsPercentExpectRR of int_

and epilogue = string_
