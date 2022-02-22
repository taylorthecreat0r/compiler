from enum import Enum

id_cond = 1
id_iter = 1

class VarType(Enum):
    IDE = 1
    VAL = 2
    NONE = 3
    PTR = 4


class Variable:
    name = ""
    is_table = False
    memory_len = 1
    is_init = False
    is_iter = False
    type_ = VarType.IDE
    x = 0
    memory_offset = 1
    first_index = 1
    
    # FOR pid[pid]
    index_mem = 1
    
    
    
class Condition:
    val1 = None
    val2 = None
    operation = ""    
    
    def __init__(self):
        global id_cond
        self.id = id_cond
        id_cond += 1 
        
class Iterator:
    memory_offset = 1
    name = ""
    
    def __init__(self):
        global id_iter
        self.id = id_iter
        id_iter += 1 
    
negations = {
    'EQ' : 'NEQ',
    'NEQ' : 'EQ',
    'LE' : 'GEQ',
    'GE' : 'LEQ',
    'LEQ' : 'GE',
    'GEQ' : 'LE',
}