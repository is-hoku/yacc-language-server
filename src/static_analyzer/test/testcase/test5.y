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

%toke NUMBER
%token PLUS MINUS MULTIPLY DIVIDE
