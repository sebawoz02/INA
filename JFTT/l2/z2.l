%{
    #include <stdio.h>
%}

%x TAG
%x COMMENT

%%

\<                  {ECHO; BEGIN(TAG);}

<TAG>\".*\"	        ECHO;
<TAG>.              ECHO;
<TAG>\n             ECHO;
<TAG>.>             {ECHO; BEGIN(INITIAL);}

\<!--               BEGIN(COMMENT);

<COMMENT>.|\n       ;
<COMMENT>--\>       BEGIN(INITIAL);

\<!--.*--.*--\>     {ECHO;}     // Invalid comment

%%

int yywrap(){}
int main() {
    yylex();
    return 0;
}