%{
#include <stdio.h>
#include "parser.tab.h"
int yylex(void);
%}
%s IGNORE
%%
<INITIAL>{
    ^#(.|\\\n)*\n       { BEGIN(IGNORE); yyless(0);}
    [[:blank:]]+        ;
    \\\n                ;
    [+\-*/\^\(\)]|\n    { return *yytext; }
    [0-9]+              { yylval = atoi(yytext); return NUM; }
    .                   { return ERROR; }
}

<IGNORE>{
    \n                  {BEGIN(INITIAL);}
    .                   ;
}

%%