import ply.lex as lex

import ply.yacc as yacc


parser_error = 0

tokens = (
    'NAME','NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE', 'POW', 'EQUALS', 
    'LPAREN','RPAREN',
    )

# Tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_POW     = r'\^'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_NAME    = r'[a-zA-Z_][a-zA-Z0-9_]*'

def t_NUMBER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Za duża liczba: %d", t.value)
        t.value = 0
    return t

# Ignored characters
t_ignore = ' \t'
t_ignore_slash = r'\\\n'
t_ignore_comment =  '^\#(.|\\\n)*\n'
    
def t_error(t):
    global parser_error
    parser_error = 1
    print("Syntax error %s" % t.value[0])
    t.lexer.skip(1)
    
# Build the lexer
lexer = lex.lex()

# Parsing rules
precedence = (
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE'),
    ('nonassoc', 'POW'),
    ('right','UMINUS'),
    )

P = 1234577
names = { }
rpn = [] # reverse polish notation
error = 0

def add(a, b, p):
	return (a%p + b%p)%p


def sub(a, b, p):
	return add(a, p-b, p)


def mul(a, b, p):
	x = (a%p)*(b%p)
	return x%p


def mult_inv(a,p):
    p0 = p 
    a0 = a
    y = 0
    x = 1
  
    while a > 1 and p != 0: 
        q = a // p
  
        t = p 

        p = a % p 
        a = t 
        t = y 
   
        y = x - q * y 
        x = t

    if a != 1:
        global error
        error=1
        print(f"Nie istnieje odwrotność {a0} modulo {p0}")
        return -1

    if(x < 0):
        x += p0
    return x



def divide(a, b, p):
	x = mult_inv(b, p)
	if x == -1:
		return 0
	return mul(a, x, p)


def power(a, b, p):
    result = 1
    a = a % p
    if b < 0:
         b = (p-1) + b
    b = b % p
    while b > 0:
        if (b % 2 == 1):
            result = mul(result, a, p)
        a = (a * a) % p
        b = b // 2
    return result


def p_statement_assign(t):
    'statement : NAME EQUALS expression'
    names[t[1]] = t[3]
    rpn.clear()

def p_statement_expr(t):
    'statement : expression'
    global error
    global parser_error
    rpntext = ""
    for element in rpn:
        rpntext += element + " "
    rpn.clear()
    
    if error == 0 and parser_error == 0:
        print("{}\nWynik: {}".format(rpntext, t[1]))
    error = 0
    parser_error = 0

def p_expression_op(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression POW expression2'''
    if t[2] == '+':
        rpn.append("+")
        t[0] = add(t[1], t[3], P)
    elif t[2] == '-':
        rpn.append("-")
        t[0] = sub(t[1], t[3], P)
    elif t[2] == '*':
        rpn.append("*")
        t[0] = mul(t[1], t[3], P)
    elif t[2] == '/':
        rpn.append("/")
        t[0] = divide(t[1], t[3], P)
    elif t[2] == '^':
        rpn.append('^')
        t[0] = power(t[1], t[3], P)

def p_expression_uminus(t):
    'expression : MINUS expression %prec UMINUS'
    t[0] = P - t[2]%P

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

def p_expression_number(t):
    'expression : NUMBER'
    t[0] = t[1]%P
    rpn.append(str(int(t[1])%P))

def p_expression_name(t):
    'expression : NAME'
    global error
    try:
        t[0] = names[t[1]]
        rpn.append(str(t[0]))
    except LookupError:
        rpn.clear()
        error = 1
        print("Niezdefiniowana zmienna: '%s'" % t[1])
        t[0] = 0

def p_expression2_name(t):
    'expression2 : NAME'
    global error
    try:
        t[0] = names[t[1]]
        rpn.append(str(t[0]))
    except LookupError:
        rpn.clear()
        error = 1
        print("Niezdefiniowana zmienna: '%s'" % t[1])
        t[0] = 0        

def p_expression2_op(t):
    '''expression2 : expression2 PLUS expression2
            | expression2 MINUS expression2
            | expression2 TIMES expression2
            | expression2 DIVIDE expression2'''
    if t[2] == '+':
        rpn.append("+")
        t[0] = add(t[1], t[3], P-1)
    elif t[2] == '-':
        rpn.append("-")
        t[0] = sub(t[1], t[3], P-1)
    elif t[2] == '*':
        rpn.append("*")
        t[0] = mul(t[1], t[3], P-1)
    elif t[2] == '/':
        rpn.append("/")
        if t[3] == 0:
            global error
            error = 1
            t[0] = 0
            return 
        t[0] = divide(t[1], t[3], P-1)

def p_expression2_group(t):
    'expression2 : LPAREN expression2 RPAREN'
    t[0] = t[2]

def p_expression2_uminus(t):
    'expression2 : MINUS expression2 %prec UMINUS'
    t[0] = (P-1) - t[2]%(P-1)

def p_expression2_number(t):
    'expression2 : NUMBER'
    t[0] = t[1]%(P-1)
    rpn.append(str(int(t[1])%(P-1)))

def p_error(t):
    global error
    error = 1
    rpn.clear()

parser = yacc.yacc()

while True:
    try:
        s = ""
        while True:
            s += input()
            if s[-1] != '\\':
                break
            s += '\n'
    except EOFError:
        break
    parser.parse(s)