%{
#include<stdio.h>
#include<stdbool.h>
#include<string.h>

bool keep_doc = false;

%}

%s string
%s include

%s regular_s
%s doc_s
%s regular_m
%s doc_m

%%
<INITIAL>{
    \"              {printf("%s", yytext); BEGIN(string);}
    "#include <"    {printf("%s", yytext); BEGIN(include);}
    ("///")|("//!") {if (keep_doc) {printf("%s", yytext);} BEGIN(doc_s);}
    ("/**")|("/*!") {if (keep_doc) {printf("%s", yytext);} BEGIN(doc_m);}
    "//"            {BEGIN(regular_s);}
    "/*"            {BEGIN(regular_m);}
}

<string>\"     {printf("%s", yytext); BEGIN(INITIAL);}
<string>.      {printf("%s", yytext);}

<include>>      {printf("%s", yytext); BEGIN(INITIAL);}
<include>.      {printf("%s", yytext);}

<regular_s>\\{2}   ;
<regular_s>.*\\    ;
<regular_s>.*      {BEGIN(INITIAL);}

<doc_s>\\{2}   {if (keep_doc) {printf("%s", yytext);}}
<doc_s>.*\\    {if (keep_doc) {printf("%s", yytext);}}
<doc_s>.*      {if (keep_doc) {printf("%s", yytext);} BEGIN(INITIAL);}

<regular_m>.*"*/" {BEGIN(INITIAL);}
<regular_m>.    ;

<doc_m>.*"*/" {if (keep_doc) {printf("%s", yytext);} BEGIN(INITIAL);}
<doc_m>.      {if (keep_doc) {printf("%s", yytext);}}

%%

int yywrap() {}

int main(int argc, char**argv) {
    if (argc > 1 && strcmp("-d", argv[1]) == 0){
        keep_doc = true;
    }
    yylex();
    return 0;
}