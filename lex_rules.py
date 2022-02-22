import ply.lex as lex

# ======================= LEXER RULES ===================

cmdTokens = {

    'VAR' : 'VAR',
    'BEGIN' : 'BEGIN',
    'END' : 'END',
    'READ' : 'READ',
    'WRITE' : 'WRITE',
    
    'PLUS' : 'PLUS',
    'MINUS' : 'MINUS',
    'TIMES' : 'TIMES',
    'DIV' : 'DIV',
    'MOD' : 'MOD',
    
    'NEQ' : 'NEQ',
    'LEQ' : 'LEQ',
    'GEQ' : 'GEQ',
    'LE' : 'LE',
    'GE' : 'GE',
    'EQ' : 'EQ',
    
    'ASSIGN' : 'ASSIGN',
    'IF' : 'IF',
    'THEN' : 'THEN',
    'ELSE' : 'ELSE',
    'REPEAT' : 'REPEAT',
    'UNTIL' : 'UNTIL',
    'ENDIF' : 'ENDIF',
    'WHILE' : 'WHILE',
    'DO' : 'DO',
    'ENDWHILE' : 'ENDWHILE',

    'FOR' : 'FOR',
    'FROM' : 'FROM',
    'TO' : 'TO',
    'DOWNTO' : 'DOWNTO',
    'ENDFOR' : 'ENDFOR',
}

tokens = [
    'NUM', 'PID', 
     'LEFT', 'RIGHT', 'SEM', 'COL', 'COM'
] + list(cmdTokens.values())

t_LEFT = r'\['
t_RIGHT = r'\]'
t_COL = r'\:'
t_SEM = r'\;'
t_COM = r'\,'

t_ignore = r' '

# When ply sees ( comment ), it ignores it
t_ignore_COMMENT = r'\([^\(]*\)'
# ----------------------------


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_PID(t):
    r'[_a-z]+'
    return t

def t_CTID(t):
    r'ENDIF|ENDFOR|ENDWHILE|ENDDO|THEN|ELSE|DOWNTO|DO|WHILE'
    t.type = cmdTokens.get(t.value,'ID') 
    return t

def t_ID(t):
    r'[A-Z_][A-Z]*'
    if t.value not in cmdTokens:
        print("Wrong syntax at line: ", t.lexer.lineno)
        exit(1)
    t.type = cmdTokens.get(t.value,'ID')    
    return t


def t_NUM(t):
    r'-?\d+'
    t.value = int(t.value)
    return t
    
# When error spotted
def t_error(t):
    t.lexer.skip(1)

# EOF handling rule
def t_eof(t):
    return None

# ----------------------------
# Creating a lexer
lexer = lex.lex()

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV', 'MOD'),
)

