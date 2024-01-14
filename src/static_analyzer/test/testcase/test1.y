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
%token <int> PLUS MINUS MULTIPLY DIVIDE
%token LPAREN RPAREN

%%
/* 文法規則の定義 */
expr:
    expr PLUS term   { $$ = $1 + $3; }
    | expr MINUS term  { $$ = $1 - $3; }
    | term           { $$ = $1; }
    ;

term:
    term MULTIPLY factor  { $$ = $1 * $3; }
    | term DIVIDE factor    { $$ = $1 / $3; }
    | factor               { $$ = $1; }
    ;

factor:
    NUMBER            { $$ = atoi(yytext); }
    | LPAREN expr RPAREN  { $$ = $2; }
    ;

%%
/* メイン関数 */
int main() {
    printf("式を入力してください: ");
    yyparse();
    return 0;
}
