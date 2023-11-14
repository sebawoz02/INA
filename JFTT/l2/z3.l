%{
#include<stdio.h>
#include<stdbool.h>
#include<string.h>

bool keep_doc = false;

%}

%x STRING
%x SINGLE_COM
%x MULTI_COM
%x SINGLE_DOC
%x MULTI_DOC

n               \\\n

single_doc      \/{n}*\/{n}*[\/\!]
single_com      \/{n}*\/
multi_doc_a     \/{n}*\*{n}*[\*\!]
multi_doc_b     \*{n}*\/
mulit_com_a     \/{n}*\*
mutli_com_b     \*{n}*\/

inc \#include[[:blank:]]["\<].*[">]

%%
BEGIN(INITIAL);

<INITIAL>{
    {inc}           {ECHO;}
    \"              {ECHO; BEGIN(STRING);}
    {single_doc}    {if(keep_doc)ECHO; BEGIN(SINGLE_DOC);}
    {multi_doc_a}   {if(keep_doc)ECHO; BEGIN(MULTI_DOC);}
    {single_com}    BEGIN(SINGLE_COM);
    {mulit_com_a}   BEGIN(MULTI_COM);
}

<STRING>{
    \"              {ECHO; BEGIN(INITIAL);}
    .               {ECHO;}
}

<SINGLE_COM>{
    {n}             ;
    \n              BEGIN(INITIAL);
    .               ;
}

<MULTI_COM>{
    {mutli_com_b}   BEGIN(INITIAL);
    .               ;
}

<SINGLE_DOC>{
    {n}             {if(keep_doc)ECHO;}
    \n              {if(keep_doc)ECHO; BEGIN(INITIAL);}
    .               {if(keep_doc)ECHO;}
}

<MULTI_DOC>{
    {multi_doc_b}   {if(keep_doc)ECHO; BEGIN(INITIAL);}
    .               {if(keep_doc)ECHO;}
}

%%

int yywrap() {}

int main(int argc, char**argv) {
    if (argc > 1 && strcmp("-d", argv[1]) == 0){
        keep_doc = true;
    }
    yylex();
    return 0;
}