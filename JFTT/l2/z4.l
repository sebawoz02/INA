%{
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

typedef struct Stack
{
    int val;
    struct Stack* next;
} Stack;

Stack* top = NULL;
bool err = false;
bool empty_stack_err = false;

void push(int val){
    Stack* new_top = malloc(sizeof(*new_top));
    new_top->val = val;
    new_top->next = top;
    top = new_top;
}

int pop(){
    if(top == NULL)
    {
        empty_stack_err = true;
        return 0;
    }
    int val = top->val;
    Stack* prev = top;
    top = top->next;
    free(prev);

    return val;
}

%}

%%

[-]?[0-9]+  {
    if(!err)
        push(atoi(yytext));
}

"+"     {
    if(!err)
    {
        int a = pop();
        int b = pop();
        if(empty_stack_err)
        {
            printf("Za mala liczba argumentow\n");
            err = true;
        }
        else
        {
            push(b + a);
        }
    }
}

"-"     {
    if(!err)
    {
        int a = pop();
        int b = pop();
        if(empty_stack_err)
        {
            printf("Za mala liczba argumentow\n");
            err = true;
        }
        else
        {
            push(b - a);
        }
    }
}

"*"     {
    if(!err)
    {
        int a = pop();
        int b = pop();
        if(empty_stack_err)
        {
            printf("Za mala liczba argumentow\n");
            err = true;
        }
        else
        {
            push(b * a);
        }
    }
}

"/"     {
    if(!err)
    {
        int a = pop();
        int b = pop();
        if(empty_stack_err)
        {
            printf("Za mala liczba argumentow\n");
            err = true;
        }
        else if( a == 0 )
        {
            printf("Dzielenie przez zero!\n");
            err = true;
        }
        else
        {
            push(b / a);
        }
    }
}

"%"     {
    if(!err)
    {
        int a = pop();
        int b = pop();
        if(empty_stack_err)
        {
            printf("Za mala liczba argumentow\n");
            err = true;
        }
        else if( a == 0 )
        {
            printf("Dzielenie przez zero!\n");
            err = true;
        }
        else
        {
            push(b % a);
        }
    }
}

"^"     {
    if(!err)
    {
        int a = pop();
        int b = pop();
        if(empty_stack_err)
        {
            printf("Za mala liczba argumentow\n");
            err = true;
        }
        else if(a == 0)
        {
            push(1);
        }
        else
        {
            int p = b;
            for(int i = 1; i < a; i++)
            {
                b *= p;
            }
            push(b);
        }
    }
}

\n {
    if (!err){
        int result = pop();
        if (top != NULL){
            printf("Za mala liczba operatorow\n");
        }
        else if(empty_stack_err)
        {
            printf("Za mala liczba operatorow\n");
        }
        else {
            printf("Wynik: %d\n", result);
        }
    }
    err = false;
    empty_stack_err = false;
}

" " ;

[^0-9 \t\n] {
    if (!err){
        printf("Nieprawidłowy symbol: \"%s\"\n", yytext);
        err = true;
    }
}

%%

int yywrap() {}

int main(int argc, char**argv) {
    yylex();
    return 0;
}