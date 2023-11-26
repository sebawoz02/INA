import ply.lex as lex

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
t_ignore_newlineslash = '\\\n'
t_ignore = ' \t'
t_ignore_comment =  '^\#(.|\\\n)*\n'
    
def t_error(t):
    print("Niepoprawny znak: '%s'" % t.value[0])
    t.lexer.skip(1)
    
# Build the lexer
lexer = lex.lex()