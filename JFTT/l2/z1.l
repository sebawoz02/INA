%{
	#include <stdio.h>
	int no_lines = 0;
	int no_words = 0;
%}

%s LINE
%s WORD
%s SPACE

%%

	BEGIN(LINE);

<LINE>[ \t]+     	;
<LINE>\n            ;
<LINE>[^ \t\n]+     {	
                      	if (no_lines > 0)
                        	printf("\n");
                      	ECHO;
                      	no_lines++;
                      	no_words++;
                      	BEGIN(WORD);
                    }


<WORD>[ \t]+ 			BEGIN(SPACE);
<WORD>\n            	BEGIN(LINE);

<SPACE>[^ \t\n]+       	{
                	  		printf(" ");
                      		ECHO;
                      		no_words++;
                      		BEGIN(WORD);
                    	}
<SPACE>\n           	BEGIN(LINE);
%%

int yywrap(){}
int main(int argc, char **argv) {
	yylex();
	// printf("\nLines: %d, Words: %d", no_lines, no_words);
	return 0;
}