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
"hoge" 241 = %token GOMA
%token NUMBER
%left <int> HOGE 1 "hoge"
