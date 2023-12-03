%token <Language_server.Import.Range.t> LPROLOGUE "%{"
%token <Language_server.Import.Range.t> RPROLOGUE "%}"
%token <Language_server.Import.Range.t> DELIMITER "%%"
%token <Language_server.Import.Range.t> REQUIRE "%require"
%token <Language_server.Import.Range.t> UNION "%union"
%token <Language_server.Import.Range.t> TOKEN "%token"
%token <Language_server.Import.Range.t> RIGHT "%right"
%token <Language_server.Import.Range.t> LEFT "%left"
%token <Language_server.Import.Range.t> PRECEDENCE "%precedence"
%token <Language_server.Import.Range.t> NONASSOC "%nonassoc"
%token <Language_server.Import.Range.t> NTERM "%nterm"
%token <Language_server.Import.Range.t> TYPE "%type"
%token <Language_server.Import.Range.t> INITIALACTION "%initial-action"
%token <Language_server.Import.Range.t> DESTRUCTOR "%destructor"
%token <Language_server.Import.Range.t> PRINTER "%printer"
%token <Language_server.Import.Range.t> EXPECT "%expect"
%token <Language_server.Import.Range.t> EXPECTRR "%expect-rr"
%token <Language_server.Import.Range.t> DEBUG "%debug"
%token <Language_server.Import.Range.t> START "%start"
%token <Language_server.Import.Range.t> CODE "%code"
%token <Language_server.Import.Range.t> DEFINE "%define"
%token <Language_server.Import.Range.t> DEFINES "%defines"
%token <Language_server.Import.Range.t> FILEPREFIX "%file-prefix"
%token <Language_server.Import.Range.t> HEADER "%header"
%token <Language_server.Import.Range.t> LANGUAGE "%language"
%token <Language_server.Import.Range.t> LOCATIONS "%locations"
%token <Language_server.Import.Range.t> NAMEPREFIX "%name-prefix"
%token <Language_server.Import.Range.t> NOLINES "%no-lines"
%token <Language_server.Import.Range.t> OUTPUT "%output"
%token <Language_server.Import.Range.t> PUREPARSER "%pre-parser"
%token <Language_server.Import.Range.t> SKELETON "%skeleton"
%token <Language_server.Import.Range.t> TOKENTABLE "%token-table"
%token <Language_server.Import.Range.t> VERBOSE "%verbose"
%token <Language_server.Import.Range.t> YACC "%yacc"
%token <Language_server.Import.Range.t> LT "<"
%token <Language_server.Import.Range.t> GT ">"
%token <Language_server.Import.Range.t> ASTERISK "*"
%token <Language_server.Import.Range.t * int> POSITIONAL "@i"
%token <Language_server.Import.Range.t * string> NAMEDPOSITIONAL "@[exp]"
%token <Language_server.Import.Range.t> RESULTPOSITIONAL "@$"
%token <Language_server.Import.Range.t * int> SEMANTIC "$i"
%token <Language_server.Import.Range.t * string> NAMEDSEMANTIC "$[exp]"
%token <Language_server.Import.Range.t> RESULTSEMANTIC "$$"
%token <Language_server.Import.Range.t> LBRACE "{"
%token <Language_server.Import.Range.t> RBRACE "}"
%token <Language_server.Import.Range.t> LBRACK "["
%token <Language_server.Import.Range.t> RBRACK "]"
%token <Language_server.Import.Range.t> DOT "."
%token <Language_server.Import.Range.t> COLON ":"
%token <Language_server.Import.Range.t> SEMICOLON ";"
%token <Language_server.Import.Range.t> PIPE "|"
%token <Language_server.Import.Range.t * int> DECIMAL (* decimal numbers such as 300 *)
%token <Language_server.Import.Range.t * int> HEXADECIMAL (* hexadecimal numbers such as 0x12d *)
%token <Language_server.Import.Range.t * string> SYMBOL (* terminal/non-terminal symbols (variables for named references, qualifiers) *)
%token <Language_server.Import.Range.t * string> STRING (* "string" or 'char' *)
%token <Language_server.Import.Range.t> EOF "eof"

%start main
%type <unit> main

%%

main:
| sections+ DELIMITER grammar_rules+ DELIMITER? EOF {}
| error {}

%inline sections:
| prologue | bison_decl {}

prologue:
| LPROLOGUE RPROLOGUE {}

bison_decl:
| UNION LBRACE RBRACE {}
| REQUIRE STRING {}
| INITIALACTION LBRACE RBRACE {}
| DESTRUCTOR LBRACE RBRACE symbols {}
| PRINTER LBRACE RBRACE symbols {}
| EXPECT DECIMAL {}
| EXPECTRR DECIMAL {}
| START SYMBOL {}
| CODE SYMBOL? LBRACE RBRACE {} (* check if qualifier is valid *)
| DEBUG {}
| DEFINES STRING? {}
| FILEPREFIX STRING {}
| HEADER STRING? {}
| LANGUAGE STRING {}
| LOCATIONS {}
| NAMEPREFIX STRING {}
| NOLINES {}
| OUTPUT STRING {}
| PUREPARSER {}
| SKELETON STRING {}
| TOKENTABLE {}
| VERBOSE {}
| YACC {}
(* terminal and non-terminal *)
| TOKEN tag? token_name_value+  tokens* {}
| RIGHT tag? prec_name_value+ precs* {}
| LEFT tag? prec_name_value+ precs* {}
| PRECEDENCE tag? prec_name_value+ precs* {}
| NONASSOC tag? prec_name_value+ precs* {}
| TYPE tag? type_name+ types* {}
| NTERM tag? SYMBOL+ nterms* {}
(* %define *)
| define {}

%inline symbols:
| LT GT {}
| LT ASTERISK GT {}
| tag {}
| SYMBOL+ {}

%inline token_name_value:
| SYMBOL number? STRING? {}

%inline tokens:
| tag token_name_value+ {}

%inline prec_name_value:
| SYMBOL number? {}

%inline precs:
| tag prec_name_value+ {}

%inline type_name:
| SYMBOL | STRING {}

%inline types:
| tag type_name+ {}

%inline nterms:
| tag SYMBOL+ {}

tag:
| LT SYMBOL GT {}

number:
| DECIMAL {}
| HEXADECIMAL {}

define: (* check variable and value *)
| DEFINE SYMBOL DOT SYMBOL DOT SYMBOL LBRACE RBRACE {}
| DEFINE SYMBOL DOT SYMBOL DOT SYMBOL STRING {}
| DEFINE SYMBOL DOT SYMBOL DOT SYMBOL SYMBOL {}
| DEFINE SYMBOL DOT SYMBOL DOT SYMBOL {}
| DEFINE SYMBOL DOT SYMBOL LBRACE RBRACE {}
| DEFINE SYMBOL DOT SYMBOL {}
| DEFINE SYMBOL DOT SYMBOL SYMBOL {}
| DEFINE SYMBOL DOT SYMBOL DOT SYMBOL DOT SYMBOL SYMBOL {}
| DEFINE SYMBOL LBRACE RBRACE {}

(* Named references are not checked. If implemented, positional keywords in the action must also be checked. *)
grammar_rules:
| SYMBOL named_result COLON components SEMICOLON {}

%inline named_result:
| LBRACK SYMBOL RBRACK {}

components:
| separated_nonempty_list(PIPE, rule_component) {}

rule:
| SYMBOL | STRING | SYMBOL LBRACK SYMBOL RBRACK {}

rule_component:
| rule+ LBRACE action* RBRACE {}

%inline action:
| POSITIONAL {}
| NAMEDPOSITIONAL {}
| RESULTPOSITIONAL {}
| SEMANTIC {}
| NAMEDSEMANTIC {}
| RESULTSEMANTIC {}
