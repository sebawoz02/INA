%{
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#define P 1234577

int yylex();
int yyerror(char *s);
int err = 0;
char *buffer = "";
int buf_size = 1;

int add(int a, int b, int p){
	return (a%P + b%P)%p;
}

int sub(int a, int b, int p){
	return add(a, p - b%p, p);
}

int mul(long long int a, long long int b, int p){
	unsigned long long int x = (a%p)*(b%p);
	return x%p;
}

int mult_inv(int a, int b, int *x, int *y){
	if (a == 0){
		*x = 0;
		*y = 1;
		return b;
	}

	int h1, h2;
	int g = mult_inv(b%a, a, &h1, &h2);

	*x = h2 - (b/a)*h1;
	*y = h1;
	return g;
}

int divide(int a, int b, int p){
	int x, y;
	mult_inv(b, p, &x, &y);
	if (x < 0){
		x += p;
	}
	return mul(a, x, p);
}


int power(unsigned long long int a, unsigned long long int b){
	long long result = 1;
    a = a % P;
	b = b % P;
    while (b > 0) {
        if (b % 2 == 1)
            result = mul(result, a, P);

        a = (a * a) % P;
    	b >>= 1;
    }

    return result;
}

void concat(char** buffer, char* a){
	if (strlen(a) + strlen(*buffer) < buf_size){
		strcat(*buffer, a);
	}
	else{
		buf_size += strlen(a);
		buf_size *= 2;
		char *new_buff = (char*) malloc((buf_size*sizeof(char)));
		strcpy(new_buff, *buffer);
		strcat(new_buff, a);
		*buffer = new_buff;
	}
}

%}

%token NUM
%token ERROR
%token '('
%token ')'
%left '-' '+'
%left '*' '/' '%'
%nonassoc '^'
%precedence NEG

%%
input:
	%empty
|	input line
;

line:
	'\n'
|	exp	'\n'	{ 
					if (err == 0) {
						printf("%s\n", buffer);
						printf("Wynik: %d\n", $1);
					} 
					else {
						err = 0;
					}
					buffer = "";
					buf_size = 1;
				}
|	error '\n'	{ buffer = ""; buf_size = 1; }
;

exp:
	ERROR				{ 
							err = 1;
							printf("Błąd składni\n");
						}
|	NUM					{ $$ = $1%P; char num[9]; snprintf(num, 10, "%d ", $$); concat(&buffer, num);}
|	exp '+' exp 		{ $$ = add($1, $3, P); concat(&buffer, "+ ");}
|	exp '-' exp 		{ $$ = sub($1, $3, P); concat(&buffer, "- ");}
|	exp '*'	exp 		{ $$ = mul($1, $3, P); concat(&buffer, "* ");}
|   exp '/' exp			{ 
							if ($3 != 0){
								$$ = divide($1, $3, P);
								concat(&buffer, "/ ");
							}
							else{
								yyerror("Dzielenie przez 0 jest niedozwolone!");
								err = 1;
							}
						}
|	exp '^' exp2		{ $$ = power($1, $3); concat(&buffer, "^ ");}
|	'-' exp	%prec NEG 	{ 
							$$ = P - $2%P ;
							char num[9];
							snprintf(num, 10, "%d ", $2);
							*(buffer+strlen(buffer)-strlen(num)) = '\0';
							snprintf(num, 10, "%d ", $$);
							concat(&buffer, num);
						}
|   '(' exp ')'			{ $$ = $2; }
;

exp2:
	NUM					{ $$ = $1%(P-1); char num[9]; snprintf(num, 10, "%d ", $$); concat(&buffer, num);}
|	exp2 '+' exp2		{ $$ = add($1, $3, P-1); concat(&buffer, "+ ");}
|	exp2 '-' exp2		{ $$ = sub($1, $3, P-1); concat(&buffer, "- ");}
|	exp2 '*' exp2 		{ $$ = mul($1, $3, P-1); concat(&buffer, "* ");}
|   exp2 '/' exp2		{ 
							if ($3 != 0){
								$$ = divide($1, $3, P-1);
								concat(&buffer, "/ ");
							}
							else{
								yyerror("Dzielenie przez 0 jest niedozwolone!");
								err = 1;
							}
						}
|   '-' exp2 %prec NEG 	{ 
							$$ = (P-1) - $2%(P-1) ;
							char num[9];
							snprintf(num, 10, "%d ", $2);
							*(buffer+strlen(buffer)-strlen(num)) = '\0';
							snprintf(num, 10, "%d ", $$);
							concat(&buffer, num);
						}
|	'(' exp2 ')'			{ $$ = $2; }
;
%%

int yyerror(char *s)
{
	fprintf (stderr, "%s\n", s);
}

int main()
{
    return yyparse();
}