%{
int no_lines = 0;
int no_words = 0;
%}

%%
^[[:blank:]]+\n? 	{}
[[:blank:]]+\n?$ 	{}
[[:blank:]]+ 		{ printf(" "); }
\n 					{ ECHO; no_lines++; }
[^[:blank:]\n]+  	{ ECHO; no_words++; }
%%

int yywrap(){}
int main(int argc, char **argv) {
	yylex();
	// printf("\nLines: %d, Words: %d", no_lines, no_words);
	return 0;
}