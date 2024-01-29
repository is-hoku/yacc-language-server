%{
    #include <stdio.h>
    #include <stdlib.h>

    extern int yylex();
    extern char *yytext;
    extern FILE *yyin;

    void yyerror(const char *s) {
        fprintf(stderr, "エラー: %s\n", s);
    }
%}

%token NUMBER
%token PLUS MINUS MULTIPLY DIVIDE
%token LPAREN RPAREN

%%
/* 文法規則の定義 */
expr:
    expr PLUS term   { $$ = $1 + $3; }
    | expr MINUS term  { $$ =
