import ply.yacc as yacc
from var_structs import *
from utils import *
from  lex_rules import *
import sys
import os

start_if, end_if = 0, 0
start_whiles, end_whiles, start_while_conds = [], [], []
start_fors, end_fors, start_for_conds = [], [], []
fors = []
program_iterators = []
iterartos_names = []
variables = []
variables_names = []
start_mem_index = 0
mem_index = start_mem_index
conditions = []


#====================== PROGRAM ======================

def p_program(p):
    '''
    program : VAR declarations BEGIN commands END
    '''
    addCode("HALT")
    write_to_file(outputFile)

def p_program_other(p):
    '''
    program : BEGIN commands END
    '''
    addCode("HALT")
    write_to_file(outputFile)

#====================== DECLARATIONS ======================

def p_declarations(p):
    '''
    declarations : declarations COM PID
    '''
    if p[3] in variables_names:
        e_error("Repeated var declaration", p.lineno(3))

    #global variables_names
    variables_names.append(p[3])
    declare_simple_variable(p[3])

def p_declarations_pid_arr(p):
    '''
    declarations : declarations COM PID LEFT NUM COL NUM RIGHT
    '''
    pass
    if p[3] in variables_names:
            e_error("Repeated var declaration", p.lineno(3))

    variables_names.append(p[3])
    declare_array(p[3], p[5], p[7], p.lineno(3))

def p_declarations_pid(p):
    '''
    declarations : PID
    '''

    if p[1] in variables_names:
            e_error("Repeated var declaration", p.lineno(1))

    variables_names.append(p[1])
    declare_simple_variable(p[1])

def p_declarations_arr(p):
    '''
    declarations : PID LEFT NUM COL NUM RIGHT
    '''
    if p[1] in variables_names:
            e_error("Repeated var declaration", p.lineno(1))

    variables_names.append(p[1])
    declare_array(p[1], p[3], p[5], p.lineno(1))


#====================== COMMANDS ======================

def p_commands(p):
    '''
    commands : commands command
    '''

def p_commands_com(p):
    '''
    commands : command
    '''

def p_command_assign(p):
    '''
    command : identifier ASSIGN expression SEM
    '''

    val1, val2, operation = p[3]
    if p[1].is_iter:
        e_error("Assigning to iterator is illegal!", p.lineno(1))

    if operation == "PLUS":
        handle_plus(val1, val2, p[1], p.lineno(4))
    elif operation == "MINUS":
        handle_minus(val1, val2, p[1], p.lineno(4))
    elif operation == "-":
        handle_simple(val1, p[1], p.lineno(4))
    elif operation == "TIMES":
        handle_times(val1, val2, p[1], p.lineno(4))
    elif operation == "DIV":
        handle_div(val1, val2, p[1], p.lineno(4))
    elif operation == "MOD":
        handle_mod(val1, val2, p[1], p.lineno(4))

def p_command_if(p):
    '''
    command : IF
    '''


def p_command_then(p):
    '''
    command : condition THEN
    '''
    global start_if
    start_if = get_asm_len()

def p_command_else(p):
    '''
    command : commands ELSE
    '''
    global start_if
    end_if = get_asm_len()
    diff = end_if - start_if + 1
    curr_cond = conditions.pop()
    if curr_cond.operation == 'EQ' or curr_cond.operation == 'LE' or  curr_cond.operation == 'GE':
        fix_jump([diff+1, diff], curr_cond.id)
    else:
        fix_jump([diff], curr_cond.id)
    new_cond = Condition()
    new_cond.operation = negations[curr_cond.operation]
    new_cond.val1 = curr_cond.val1
    new_cond.val2 = curr_cond.val2
    conditions.append(new_cond)
    handle_condition(new_cond.val1, new_cond.val2, new_cond.operation, new_cond.id, p.lineno(2))
    start_if = get_asm_len()

def p_command_endif(p):
    '''
    command : commands ENDIF
    '''
    global start_if, start_while_conds
    end_if = get_asm_len()
    diff = end_if - start_if + 1
    curr_cond = conditions.pop()
    start_while_conds.pop()
    if curr_cond.operation == 'EQ' or curr_cond.operation == 'LE' or  curr_cond.operation == 'GE':
        fix_jump([diff+1, diff], curr_cond.id)
    else:
        fix_jump([diff], curr_cond.id)

def p_command_while(p):
    '''
    command : WHILE condition DO
    '''
    global start_whiles
    start_whiles.append(get_asm_len())

def p_command_endwhile(p):
    '''
    command : commands ENDWHILE
    '''
    curr_cond = conditions.pop()
    global start_whiles, start_while_conds
    end_while = get_asm_len()
    start_while_cond = start_while_conds.pop()
    start_while = start_whiles.pop()
    diff = start_while_cond - end_while
    addCode("JUMP " + str(diff))
    end_while = get_asm_len()
    diff = end_while - start_while + 1
    if curr_cond.operation == 'EQ' or curr_cond.operation == 'LE' or  curr_cond.operation == 'GE':
        fix_jump([diff+1, diff], curr_cond.id)
    else:
        fix_jump([diff], curr_cond.id)

def p_repeat(p):
    '''
    command : REPEAT
    '''
    global start_whiles
    start_while = get_asm_len()
    start_whiles.append(start_while)

def p_until(p):
    '''
    command : commands UNTIL condition SEM
    '''
    global curr_cond, start_whiles
    end_while = get_asm_len()
    start_while = start_whiles.pop()
    diff =  start_while - end_while + 1
    curr_cond = conditions.pop()
    if curr_cond.operation == 'EQ' or curr_cond.operation == 'LE' or  curr_cond.operation == 'GE':
        fix_jump([diff+1, diff], curr_cond.id)
    else:
        fix_jump([diff], curr_cond.id)

def p_for_to(p):
    '''
     command : FOR PID FROM value TO value DO
    '''
    global fors
    if p[2] in iterartos_names:
        e_error("Repeated iterator declaration " + str(p[2]), p.lineno(2))
    fors.append(1)
    handle_for(p[4], p[6], p[2], p.lineno(4))

def p_endfor(p):
    '''
    command : commands ENDFOR
    '''
    global fors
    is_downto = fors.pop()
    id, diff_neg, diff_jump = handle_endfor(is_downto)
    fix_after_for(id, diff_neg, diff_jump)

def p_for_dwonto(p):
    '''
     command : FOR PID FROM value DOWNTO value DO
    '''
    global fors
    fors.append(0)
    if p[2] in iterartos_names:
        e_error("Repeated iterator declaration " + str(p[2]), p.lineno(2))

    handle_for_down(p[4], p[6], p[2], p.lineno(4))

def p_command_read(p):
    '''
    command : READ identifier SEM
    '''
    addCode("GET")
    addCode("SWAP h")


    if p[2].type_ == VarType.IDE:
        construct_value(p[2].memory_offset, 'b')
        addCode("SWAP h")
        p[2].is_init = True
        addCode("STORE b")
    elif p[2].type_ == VarType.PTR:
        load_ptr(p[2], 'b')
        p[2].is_init = True
        addCode("SWAP h")
        addCode("STORE b")

def p_command_write(p):
    '''
    command : WRITE value SEM
    '''

    if p[2].type_ == VarType.VAL:
        construct_value(p[2].x, 'b')
        addCode("SWAP b")
        addCode("PUT")
    elif p[2].type_ == VarType.IDE:
        if not p[2].is_init and not p[2].is_table:
            err = "Variable " + p[2].name + " not initialized"
            e_error(err, p.lineno(3))
        construct_value(p[2].memory_offset, 'b')
        addCode("LOAD b")
        addCode("PUT")
    elif p[2].type_ == VarType.PTR:
        construct_value(p[2].memory_offset, 'b')
        construct_value(p[2].first_index, 'c')
        construct_value(p[2].index_mem, 'd')
        addCode("LOAD d")
        addCode("SWAP d")
        addCode("RESET a")
        addCode("ADD d")
        addCode("SUB c")
        addCode("SWAP c")
        addCode("RESET a")
        addCode("SWAP b")
        addCode("ADD c")
        addCode("SWAP b")
        addCode("LOAD b")
        addCode("PUT")

#====================== EXPRESSION ======================

def p_expression_val(p):
    '''
    expression : value
    '''
    p[0] = (p[1], 0, "-")

def p_expression_plus(p):
    '''
    expression : value PLUS value
    '''
    p[0] = (p[1], p[3], "PLUS")

def p_expression_minus(p):
    '''
    expression : value MINUS value
    '''
    p[0] = (p[1], p[3], "MINUS")

def p_expression_times(p):
    '''
    expression : value TIMES value
    '''
    p[0] = (p[1], p[3], "TIMES")

def p_expression_div(p):
    '''
    expression : value DIV value
    '''
    p[0] = (p[1], p[3], "DIV")

def p_expression_mod(p):
    '''
    expression : value MOD value
    '''
    p[0] = (p[1], p[3], "MOD")

#====================== CONDITIONS ====================

def p_condition(p):
    '''
    condition : value EQ value
              | value NEQ value
              | value LE value
              | value GE value
              | value LEQ value
              | value GEQ value
    '''
    global start_while_conds
    start_while_conds.append(get_asm_len())
    curr_cond = Condition()
    curr_cond.val1 = p[1]
    curr_cond.val2 = p[3]
    curr_cond.operation = p[2]
    conditions.append(curr_cond)
    p[0] = handle_condition(p[1], p[3], curr_cond.operation, curr_cond.id, p.lineno(3))

#====================== VALUE ======================

def p_value_num(p):
    '''
    value : NUM
    '''
    var = Variable()
    var.type_ = VarType.VAL
    var.x = int(p[1])
    p[0] = var

def p_value_iden(p):
    '''
    value : identifier
    '''
    pass
    p[0] = p[1]
#====================== IDENTIFIER ======================

def p_identifier_pid(p):
    '''
    identifier : PID
    '''
    if not p[1] in variables_names:
        if not p[1] in iterartos_names:
            e_error("Undeclared variable " + str(p[1]), p.lineno(1))
        else:
            var = get_iter_from_name(p[1], p.lineno(1))
            p[0] = var
    else:
        var = get_var_from_name(p[1], p.lineno(1))
        p[0] = var

def p_identifier_pid_pid(p):
    '''
    identifier : PID LEFT PID RIGHT
    '''
    if not p[1] in variables_names:
        e_error("Undeclared array " + str(p[1]), p.lineno(1))
    if not p[3] in variables_names:
        if not p[3] in iterartos_names:
            e_error("Undeclared variable " + str(p[3]), p.lineno(3))
        else:
            ind = get_iter_from_name(p[3], p.lineno(3))
            var = get_arr_elem_from_name(p[1], p.lineno(1), ind, True)
            var.is_init = True
            p[0] = var
    else:
        ind = get_var_from_name(p[3], p.lineno(3))
        var = get_arr_elem_from_name(p[1], p.lineno(1), ind, True)
        var.is_init = True
        p[0] = var

def p_identifier_num(p):
    '''
    identifier : PID LEFT NUM RIGHT
    '''

    if not p[1] in variables_names:
        e_error("Undeclared array " + str(p[1]), p.lineno(1))

    var = get_arr_elem_from_name(p[1], p.lineno(1), p[3])
    var.is_init = True
    p[0] = var


def p_error(f):
    exit(0)
    

#====================== FUNCTIONS ======================

def construct_value(num, reg): # USE F, A, reg
    neg = False
    addCode("RESET " + reg)
    if num == 0:
        return
    if abs(num) < 32:
        if num > 0:
            for i in range(num):
                addCode("INC " + reg)
        else:
            for i in range(abs(num)):
                addCode("DEC " + reg)
    else:
        addCode("RESET f")
        addCode("RESET a")
        if num < 0:
            num = abs(num)
            neg = True

        num = bin(num)[2:]
        addCode("INC f")
        for val in num[:-1]:
            if val == '1':
                if neg:
                    addCode("DEC a")
                else:
                    addCode("INC a")
                addCode("SHIFT f")
            else:
                addCode("SHIFT f")

        if num[-1] == '1':
            if neg:
                addCode("DEC a")
            else:
                addCode("INC a")



        addCode("SWAP " + reg)

def declare_simple_variable(name):
    global mem_index
    global variables
    var = Variable()
    var.name = name
    var.type_ = VarType.IDE
    var.memory_offset = mem_index
    variables.append(var)
    mem_index += var.memory_len

def declare_array(name, first, last, line):
    i1, i2 = int(first), int(last)
    if i2 < i1:
        err = "Incorrect array range: (" + str(i1) + ", " + str(i2) + ")"
        e_error(err, line)

    global mem_index
    global variables
    var = Variable()
    var.name = name
    var.type_ = VarType.IDE
    var.is_table = True
    var.memory_len = i2 - i1 + 1
    var.memory_offset = mem_index
    var.first_index = i1
    variables.append(var)
    mem_index += var.memory_len

def get_var_from_name(name, line):
    for var in variables:
        if var.name == name and var.is_table == False:
            return var
        elif var.name == name:
            err = "Variable " + name + " is array!"
            e_error(err, line)
    e_error("Undeclared variable " + str(name), line)

def get_arr_elem_from_name(name, line, index, is_pid=False):
    for var in variables:
        if var.name == name and var.is_table == True and not is_pid:
            element = Variable()
            if index in range(var.first_index, var.first_index + var.memory_len):
                element.memory_offset = var.memory_offset + index - var.first_index
                return element
            else:
                err = "Index " + str(index) + " out of range (" + str(var.first_index) + ", " + str(var.first_index + var.memory_len - 1) + ")"
                e_error(err, line)
        elif var.name == name and var.is_table == False:
            err = "Variable: " + name + " is not array!"
            e_error(err, line)
        elif var.name == name and var.is_table == True and is_pid:
            element = Variable()
            if not index.is_init:
                err = "Variable " + index.name + " not initiazlized"
                e_error(err, line)

            element.type_ = VarType.PTR
            element.memory_offset = var.memory_offset
            element.first_index = var.first_index
            element.index_mem = index.memory_offset
            return element


    e_error("Undeclared array " + str(name), line)

def get_iter_from_name(name, line):
    global program_iterators
    for iter in program_iterators:
        if iter.name == name:
            var = Variable()
            var.name = name
            var.memory_offset = iter.memory_offset + 1
            var.is_init = True
            var.type_ = VarType.IDE
            var.is_iter = True
            return var

    e_error("iterator doesn't exists", line)

def handle_simple(val:Variable, res:Variable, line): # a assign b

    if res.type_ == VarType.IDE:
        if val.type_ == VarType.VAL:
            res.x = val.x
            construct_value(res.memory_offset, 'g')
            construct_value(res.x, 'h')
            addCode("SWAP h")
            addCode("STORE g")
        elif val.type_ == VarType.IDE:
            check_init(val, line)
            construct_value(val.memory_offset, 'b')
            addCode("LOAD b")
            addCode("SWAP c")
            construct_value(res.memory_offset, 'b')
            addCode("SWAP c")
            addCode("STORE b")

        elif val.type_ == VarType.PTR:
            construct_value(val.memory_offset, 'b')
            construct_value(val.first_index, 'c')
            construct_value(val.index_mem, 'd')
            construct_value(res.memory_offset, 'e')
            addCode("LOAD d")
            addCode("SWAP d")
            addCode("RESET a")
            addCode("ADD d")
            addCode("SUB c")
            addCode("SWAP c")
            addCode("RESET a")
            addCode("SWAP b")
            addCode("ADD c")
            addCode("SWAP b")
            addCode("LOAD b")
            addCode("STORE e")
    elif res.type_ == VarType.PTR:
        if val.type_ == VarType.VAL:
            res.x = val.x
            construct_value(res.memory_offset, 'b')
            construct_value(res.first_index, 'c')
            construct_value(res.index_mem, 'd')
            construct_value(val.x, 'h')
            addCode("LOAD d")
            addCode("SWAP d")
            addCode("RESET a")
            addCode("ADD d")
            addCode("SUB c")
            addCode("SWAP c")
            addCode("RESET a")
            addCode("SWAP b")
            addCode("ADD c")
            addCode("SWAP b")
            addCode("SWAP h")
            addCode("STORE b")

        elif val.type_ == VarType.IDE:
            check_init(val, line)
            construct_value(res.memory_offset, 'b')
            construct_value(res.first_index, 'c')
            construct_value(res.index_mem, 'd')
            addCode("LOAD d")
            addCode("SWAP d")
            addCode("RESET a")
            addCode("ADD d")
            addCode("SUB c")
            addCode("SWAP c")
            addCode("RESET a")
            addCode("SWAP b")
            addCode("ADD c")
            addCode("SWAP b") # address of result in b
            construct_value(val.memory_offset, 'c')
            addCode("LOAD c")
            addCode("STORE b")

        elif val.type_ == VarType.PTR:
            construct_value(res.memory_offset, 'b')
            construct_value(res.first_index, 'c')
            construct_value(res.index_mem, 'd')
            addCode("LOAD d")
            addCode("SWAP d")
            addCode("RESET a")
            addCode("ADD d")
            addCode("SUB c")
            addCode("SWAP c")
            addCode("RESET a")
            addCode("SWAP b")
            addCode("ADD c")
            addCode("SWAP e") # address of result in e
            construct_value(val.memory_offset, 'b')
            construct_value(val.first_index, 'c')
            construct_value(val.index_mem, 'd')
            addCode("LOAD d")
            addCode("SWAP d")
            addCode("RESET a")
            addCode("ADD d")
            addCode("SUB c")
            addCode("SWAP c")
            addCode("RESET a")
            addCode("SWAP b")
            addCode("ADD c")
            addCode("SWAP b") # address of result in b
            addCode("LOAD b")
            addCode("STORE e")

    res.is_init = True

def handle_plus(val1: Variable, val2: Variable, res:Variable, line):
    if val1.type_ == VarType.VAL and val2.type_ == VarType.VAL:
        res.x = val1.x + val2.x
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'g')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'g')
        construct_value(res.x, 'h')
        addCode("SWAP h")
        addCode("STORE g")
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.VAL:
        check_init(val1, line)

        construct_value(val1.memory_offset, 'b')
        addCode("LOAD b")
        addCode("SWAP e")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'b')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'b')
        construct_value(val2.x, 'c')
        addCode("SWAP e")
        addCode("ADD c")
        addCode("STORE b")
    elif val2.type_ == VarType.IDE and val1.type_ == VarType.VAL:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'b')
        addCode("LOAD b")
        addCode("SWAP e")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'b')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'b')
        construct_value(val1.x, 'c')
        addCode("SWAP e")
        addCode("ADD c")
        addCode("STORE b")
    elif val2.type_ == VarType.IDE and val1.type_ == VarType.IDE:
        check_init(val1, line)
        check_init(val2, line)
        construct_value(val1.memory_offset, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'b')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'b')
        construct_value(val2.memory_offset, 'c')
        addCode("LOAD c")
        addCode("ADD h")
        addCode("STORE b")
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.PTR:
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.x, 'c')
        addCode("RESET a")
        addCode("ADD h")
        addCode("ADD c")
        addCode("STORE d")
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.VAL:
        val1, val2 = val2, val1
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.x, 'c')
        addCode("RESET a")
        addCode("ADD h")
        addCode("ADD c")
        addCode("STORE d")
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.PTR:
        check_init(val1, line)
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.memory_offset, 'c')
        addCode("LOAD c")
        addCode("SWAP c")
        addCode("RESET a")
        addCode("ADD h")
        addCode("ADD c")
        addCode("STORE d")
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.IDE:
        check_init(val2, line)
        val1, val2 = val2, val1
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.memory_offset, 'c')
        addCode("LOAD c")
        addCode("SWAP c")
        addCode("RESET a")
        addCode("ADD h")
        addCode("ADD c")
        addCode("STORE d")
    else:
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP e")
        load_ptr(val1, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        addCode("RESET a")
        addCode("ADD e")
        addCode("ADD h")
        addCode("STORE d")

    res.is_init = True

def handle_minus(val1: Variable, val2: Variable, res:Variable, line):
    if val1.type_ == VarType.VAL and val2.type_ == VarType.VAL:
        res.x = val1.x - val2.x
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'g')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'g')
        construct_value(res.x, 'h')
        addCode("SWAP h")
        addCode("STORE g")
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.VAL:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'b')
        addCode("LOAD b")
        addCode("SWAP e")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'b')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'b')
        construct_value(val2.x, 'c')
        addCode("SWAP e")
        addCode("SUB c")
        addCode("STORE b")
    elif val2.type_ == VarType.IDE and val1.type_ == VarType.VAL:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'b')
        addCode("LOAD b")
        addCode("SWAP c")
        construct_value(res.x, 'h')
        addCode("SWAP h")
        addCode("SUB c")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'b')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'b')
        addCode("SWAP h")
        addCode("STORE b")
    elif val2.type_ == VarType.IDE and val1.type_ == VarType.IDE:
        check_init(val1, line)
        check_init(val2, line)
        construct_value(val2.memory_offset, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        construct_value(val1.memory_offset, 'c')
        addCode("LOAD c")
        addCode("SUB h")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'b')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'b')
        addCode("SWAP h")
        addCode("STORE b")
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.PTR:
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.x, 'c')
        addCode("RESET a")
        addCode("ADD c")
        addCode("SUB h")
        addCode("STORE d")
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.VAL:
        val1, val2 = val2, val1
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.x, 'c')
        addCode("RESET a")
        addCode("SUB c")
        addCode("ADD h")
        addCode("STORE d")
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.PTR:
        check_init(val1, line)
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.memory_offset, 'c')
        addCode("LOAD c")
        addCode("SWAP c")
        addCode("RESET a")
        addCode("ADD c")
        addCode("SUB h")
        addCode("STORE d")
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.IDE:
        check_init(val2, line)
        val1, val2 = val2, val1
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        construct_value(val1.memory_offset, 'c')
        addCode("LOAD c")
        addCode("SWAP c")
        addCode("RESET a")
        addCode("ADD h")
        addCode("SUB c")
        addCode("STORE d")
    else:
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP e")
        load_ptr(val1, 'b')
        addCode("LOAD b")
        addCode("SWAP h")
        if res.type_ == VarType.IDE:
            construct_value(res.memory_offset, 'd')
        elif res.type_ == VarType.PTR:
            load_ptr(res, 'd')
        addCode("RESET a")
        addCode("SUB e")
        addCode("ADD h")
        addCode("STORE d")
    res.is_init = True

def handle_times(val1: Variable, val2: Variable, res:Variable, line):
    addCode("RESET h")

    if val2.type_ == VarType.IDE:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
    elif val2.type_ == VarType.VAL:
        construct_value(val2.x, 'g')
        addCode("SWAP g")
    else:
        check_init(val2, line)
        load_ptr(val2, 'g')
        addCode("LOAD g")


    addCode("JPOS 5")
    addCode("INC h")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("SUB b")
    addCode("SWAP d")

    if val1.type_ == VarType.IDE:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
    elif val1.type_ == VarType.VAL:
        construct_value(val1.x, 'g')
        addCode("SWAP g")
    else:
        check_init(val1, line)
        load_ptr_no_D(val1, 'g')
        addCode("LOAD g")

    addCode("JPOS 5")
    addCode("INC h")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("SUB b")
    addCode("SWAP c")


    addCode("RESET a")
    addCode("ADD c")
    addCode("SUB d")
    addCode("JPOS 5")
    addCode("RESET a")
    addCode("ADD c")
    addCode("SWAP d")
    addCode("SWAP c")

    addCode("RESET a")
    addCode("ADD d")
    addCode("JPOS 3")
    addCode("RESET a")
    before_zero = get_asm_len()
    addCode("JUMP 0")

    addCode("RESET f")
    addCode("RESET e")
    addCode("RESET a")
    addCode("RESET b")
    addCode("ADD d")
    addCode("DEC b")
    addCode("INC e")
    addCode("SHIFT b")
    addCode("JPOS -2")
    end_smaller = get_asm_len()

    addCode("DEC e")

    addCode("RESET a")
    addCode("INC a")
    addCode("SHIFT e")

    addCode("SUB d")

    addCode("SWAP b")

    addCode("RESET a")
    addCode("SUB b")

    addCode("JNEG " + str(end_smaller - get_asm_len()))
    addCode("JPOS 2")
    addCode("JZERO 8")
    addCode("SWAP d")

    addCode("RESET a")
    addCode("ADD c")
    addCode("SHIFT e")
    addCode("ADD f")
    addCode("SWAP f")
    addCode("JUMP " + str(end_smaller - get_asm_len()))

    addCode("RESET a")
    addCode("ADD c")
    addCode("SHIFT e")
    addCode("ADD f")
    addCode("SWAP f")

    addCode("SWAP h")
    addCode("DEC a")
    addCode("JZERO 3")
    addCode("SWAP f")
    addCode("JUMP 3")
    addCode("RESET a")
    addCode("SUB f")


    asm_code[before_zero] = "JUMP " + str(get_asm_len() - before_zero)

    addCode("SWAP e")

    if res.type_ == VarType.IDE:
        construct_value(res.memory_offset, 'b')
    elif res.type_ == VarType.PTR:
        load_ptr(res, 'b')

    addCode("SWAP e")
    addCode("STORE b")
    res.is_init = True

def handle_div(val1: Variable, val2: Variable, res:Variable, line):

    addCode("RESET h")
    if val2.type_ == VarType.IDE:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
    elif val2.type_ == VarType.VAL:
        construct_value(val2.x, 'g')
        addCode("SWAP g")
    else:
        check_init(val2, line)
        load_ptr(val2, 'g')
        addCode("LOAD g")

    jzero_multiplier = get_asm_len()
    addCode("JZERO x")
    addCode("JPOS 5")
    addCode("INC h")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("SUB b")

    addCode("SWAP d")
    if val1.type_ == VarType.IDE:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
    elif val1.type_ == VarType.VAL:
        construct_value(val1.x, 'g')
        addCode("SWAP g")
    else:
        check_init(val1, line)
        load_ptr_no_D(val1, 'g')
        addCode("LOAD g")
    jzero_multiplicand = get_asm_len()
    addCode("JZERO x")
    addCode("JPOS 5")

    addCode("INC h")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("SUB b")
    addCode("SWAP c")


    addCode("RESET f")
    addCode("RESET e")

    addCode("RESET a")
    addCode("ADD d")
    addCode("INC e")
    addCode("SHIFT e")
    addCode("SUB c")
    addCode("JNEG -5")
    addCode("JZERO 2")
    addCode("DEC e")

    addCode("RESET a")
    addCode("ADD c")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("ADD d")
    addCode("SHIFT e")
    addCode("SWAP b")
    addCode("SUB b")


    addCode("SWAP e")
    addCode("JZERO 13")
    addCode("SWAP e")
    addCode("JNEG -12")
    addCode("SWAP b")

    addCode("RESET a")
    addCode("INC a")
    addCode("SHIFT e")
    addCode("ADD f")
    addCode("SWAP f")

    addCode("SWAP b")
    addCode("JZERO 13")
    addCode("SWAP c")
    addCode("JUMP -23")

    addCode("SWAP e")
    addCode("JNEG 17")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("INC a")
    addCode("SHIFT e")
    addCode("ADD f")
    addCode("SWAP f")
    addCode("SWAP b")
    addCode("JPOS 9")

    addCode("SWAP h")
    addCode("DEC a")
    addCode("JZERO 3")
    addCode("SWAP f")
    addCode("JUMP 3")
    addCode("RESET a")
    addCode("SUB f")
    addCode("JUMP 9")

    addCode("SWAP h")
    addCode("DEC a")
    addCode("JZERO 3")
    addCode("SWAP f")
    addCode("JUMP 4")
    addCode("RESET a")
    addCode("SUB f")
    addCode("DEC a")
    asm_code[jzero_multiplier] = "JZERO " + str(get_asm_len() - jzero_multiplier)
    asm_code[jzero_multiplicand] = "JZERO " + str(get_asm_len() - jzero_multiplicand)
    addCode("SWAP g")

    if res.type_ == VarType.IDE:
        construct_value(res.memory_offset, 'b')
    elif res.type_ == VarType.PTR:
        load_ptr(res, 'b')

    addCode("SWAP g")
    addCode("STORE b")
    res.is_init = True

def handle_mod(val1: Variable, val2: Variable, res:Variable, line):
    addCode("RESET h")
    if val2.type_ == VarType.IDE:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
    elif val2.type_ == VarType.VAL:
        construct_value(val2.x, 'g')
        addCode("SWAP g")
    else:
        check_init(val2, line)
        load_ptr(val2, 'g')
        addCode("LOAD g")
    jzero_factor = get_asm_len()
    addCode("JZERO x")
    addCode("JPOS 5")
    addCode("DEC h")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("SUB b")
    addCode("SWAP d")

    if val1.type_ == VarType.IDE:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
    elif val1.type_ == VarType.VAL:
        construct_value(val1.x, 'g')
        addCode("SWAP g")
    else:
        check_init(val1, line)
        load_ptr_no_D(val1, 'g')
        addCode("LOAD g")
    jzero_dividend = get_asm_len()
    addCode("JZERO x")
    addCode("JPOS 6")
    addCode("INC h")
    addCode("INC h")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("SUB b")
    addCode("SWAP c")


    addCode("RESET e")
    addCode("RESET a")
    addCode("ADD d")
    addCode("INC e")
    addCode("SHIFT e")
    addCode("SUB c")
    addCode("JNEG -5")
    addCode("JZERO 2")
    addCode("DEC e")

    addCode("RESET a")
    addCode("ADD c")
    addCode("SWAP b")
    addCode("RESET a")
    addCode("ADD d")
    addCode("SHIFT e")
    addCode("SWAP b")
    addCode("SUB b")

    no_remainder = get_asm_len()
    addCode("JZERO x")

    addCode("SWAP e")
    addCode("JZERO 5")
    addCode("SWAP e")
    addCode("JNEG -13")
    addCode("SWAP c")
    addCode("JUMP -15")

    addCode("RESET a")
    addCode("ADD h")
    addCode("JZERO 11")
    addCode("DEC a")
    addCode("JZERO 13")
    addCode("JPOS 21")

    addCode("SWAP e")
    addCode("JNEG 30")
    addCode("SWAP e")
    addCode("RESET a")
    addCode("SUB d")
    addCode("ADD e")
    addCode("JUMP 25")

    addCode("SWAP e")
    addCode("JPOS 2")
    addCode("SWAP c")
    addCode("JUMP 21")

    addCode("SWAP e")
    addCode("JPOS 4")
    addCode("RESET a")
    addCode("SUB c")
    addCode("JUMP 16")
    addCode("SWAP e")
    addCode("RESET a")
    addCode("SUB e")
    addCode("JUMP 12")

    addCode("SWAP e")
    addCode("JPOS 5")
    addCode("SWAP e")
    addCode("RESET a")
    addCode("SUB e")
    addCode("JUMP 6")
    addCode("SWAP e")
    addCode("SWAP d")
    addCode("SUB e")
    addCode("JUMP 2")

    addCode("RESET a")
    asm_code[jzero_dividend] = "JZERO " + str(get_asm_len() - jzero_dividend)
    asm_code[jzero_factor] = "JZERO " + str(get_asm_len() - jzero_factor)
    asm_code[no_remainder] = "JZERO " + str(get_asm_len() - no_remainder)

    addCode("SWAP g")

    if res.type_ == VarType.IDE:
        construct_value(res.memory_offset, 'b')
    elif res.type_ == VarType.PTR:
        load_ptr(res, 'b')

    addCode("SWAP g")
    addCode("STORE b")
    res.is_init = True

def handle_condition(val1: Variable, val2: Variable, comparison, id, line):
    if val1.type_ == VarType.VAL and val2.type_ == VarType.VAL:
        construct_value(val1.x, 'g')
        construct_value(val2.x, 'h')
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.IDE:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.x, 'h') # val1 in h
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.VAL:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'h')
        addCode("LOAD h") # val2 in g
        addCode("SWAP h")
        construct_value(val2.x, 'g') # val1 in h
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.IDE:
        check_init(val1, line)
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.memory_offset, 'h')
        addCode("LOAD h") # val1 in h
        addCode("SWAP h")
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.PTR:
        load_ptr(val2, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.x, 'h')
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.VAL:
        load_ptr(val1, 'h')
        addCode("LOAD h") # val2 in g
        addCode("SWAP h")
        construct_value(val2.x, 'g')
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.PTR:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP g")
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.IDE:
        check_init(val2, line)
        load_ptr(val1, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
    else:
        load_ptr(val1, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP g")
    # VAL1 in H, VAL2 in G
        # H, G
    if comparison == 'EQ':
        addCode("SWAP h")
        addCode("SUB g")
        addCode("JPOS x" + str(id))
        addCode("JNEG x" + str(id))
    elif comparison == 'NEQ':
        addCode("SWAP h")
        addCode("SUB g")
        addCode("JZERO x" + str(id))
    elif comparison == 'LE': # H < G
        addCode("SWAP h")
        addCode("SUB g")
        addCode("JPOS x" + str(id))
        addCode("JZERO x" + str(id))
    elif comparison == 'GE':
        addCode("SWAP h")
        addCode("SUB g")
        addCode("JNEG x" + str(id))
        addCode("JZERO x" + str(id))
    elif comparison == 'LEQ':
        addCode("SWAP h")
        addCode("SUB g")
        addCode("JPOS x" + str(id))
    elif comparison == 'GEQ':
        addCode("SWAP h")
        addCode("SUB g")
        addCode("JNEG x" + str(id))

def handle_for(val1: Variable, val2: Variable, name, line):
    global program_iterators, start_fors, start_for_conds, mem_index, iterartos_names

    if val1.type_ == VarType.VAL and val2.type_ == VarType.VAL:
        construct_value(val2.x, 'g')
        construct_value(val1.x, 'h')
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.IDE:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.x, 'h') # val1 in h
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.VAL:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'h')
        addCode("LOAD h") # val1 in g
        addCode("SWAP h")
        construct_value(val2.x, 'g') # val2 in g
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.IDE:
        check_init(val1, line)
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.memory_offset, 'h')
        addCode("LOAD h") # val1 in h
        addCode("SWAP h")
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.PTR:
        load_ptr(val2, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.x, 'h')
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.VAL:
        load_ptr(val1, 'h')
        addCode("LOAD h") # val2 in g
        addCode("SWAP h")
        construct_value(val2.x, 'g')
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.PTR:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP g")
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.IDE:
        check_init(val2, line)
        load_ptr(val1, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
    else:
        load_ptr(val1, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP g")

    iter = Iterator()
    iter.memory_offset = mem_index
    iter.name = name
    program_iterators.append(iter)
    iterartos_names.append(name)
    # VAL1 in H, VAL2 in G, STORE ITERATOR
    construct_value(iter.memory_offset, 'b')
    addCode("RESET a")
    addCode("ADD g")
    addCode("SUB h")
    addCode("STORE b")
    construct_value(iter.memory_offset+1, 'b')
    addCode("RESET a")
    addCode("ADD h")
    addCode("STORE b")
    start_fors.append(get_asm_len())



    # WE WILL BE JUMPING HERE
    construct_value(iter.memory_offset, 'b')
    addCode("LOAD b")
    addCode("JNEG iter"+str(iter.id))
    start_for_conds.append(get_asm_len())
    mem_index += 2

def handle_for_down(val1: Variable, val2: Variable, name, line):
    global program_iterators, start_fors, start_for_conds, mem_index, iterartos_names
    if val1.type_ == VarType.VAL and val2.type_ == VarType.VAL:
        construct_value(val2.x, 'g')
        construct_value(val1.x, 'h')
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.IDE:
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.x, 'h') # val1 in h
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.VAL:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'h')
        addCode("LOAD h") # val1 in g
        addCode("SWAP h")
        construct_value(val2.x, 'g') # val2 in g
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.IDE:
        check_init(val1, line)
        check_init(val2, line)
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.memory_offset, 'h')
        addCode("LOAD h") # val1 in h
        addCode("SWAP h")
    elif val1.type_ == VarType.VAL and val2.type_ == VarType.PTR:
        load_ptr(val2, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
        construct_value(val1.x, 'h')
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.VAL:
        load_ptr(val1, 'h')
        addCode("LOAD h") # val2 in g
        addCode("SWAP h")
        construct_value(val2.x, 'g')
    elif val1.type_ == VarType.IDE and val2.type_ == VarType.PTR:
        check_init(val1, line)
        construct_value(val1.memory_offset, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP g")
    elif val1.type_ == VarType.PTR and val2.type_ == VarType.IDE:
        check_init(val2, line)
        load_ptr(val1, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        construct_value(val2.memory_offset, 'g')
        addCode("LOAD g") # val2 in g
        addCode("SWAP g")
    else:
        load_ptr(val1, 'h') # val1 in h
        addCode("LOAD h")
        addCode("SWAP h")
        load_ptr(val2, 'b')
        addCode("LOAD b")
        addCode("SWAP g")

    iter = Iterator()
    iter.memory_offset = mem_index
    iter.name = name
    program_iterators.append(iter)
    iterartos_names.append(name)
    construct_value(iter.memory_offset, 'b')
    addCode("RESET a")
    addCode("ADD h")
    addCode("SUB g")
    addCode("STORE b")
    construct_value(iter.memory_offset+1, 'b')
    addCode("RESET a")
    addCode("ADD h")
    addCode("STORE b")
    start_fors.append(get_asm_len())

    construct_value(iter.memory_offset, 'b')
    addCode("LOAD b")
    addCode("JNEG iter"+str(iter.id))
    start_for_conds.append(get_asm_len())
    mem_index += 2

def handle_endfor(is_downto):
    global program_iterators, start_fors, start_for_conds
    iter = program_iterators.pop()
    iterartos_names.remove(iter.name)
    start_for = start_fors.pop()
    start_for_cond = start_for_conds.pop()

    if is_downto == 1:
        construct_value(iter.memory_offset, 'b')
        addCode("LOAD b")
        addCode("DEC a")
        addCode("STORE b")

        construct_value(iter.memory_offset+1, 'b')
        addCode("LOAD b")
        addCode("INC a")
        addCode("STORE b")
    elif is_downto == 0:
        construct_value(iter.memory_offset, 'b')
        addCode("LOAD b")
        addCode("DEC a")
        addCode("STORE b")

        construct_value(iter.memory_offset+1, 'b')
        addCode("LOAD b")
        addCode("DEC a")
        addCode("STORE b")

    addCode("JUMP iter" + str(iter.id))
    end_for = get_asm_len()

    diff_jump = start_for - end_for + 1
    diff_jneg = end_for - start_for_cond + 1

    return iter.id, diff_jneg, diff_jump

def load_ptr(val: Variable, reg):
    construct_value(val.memory_offset, 'b')
    construct_value(val.first_index, 'c')
    construct_value(val.index_mem, 'd')
    addCode("LOAD d")
    addCode("SUB c")
    addCode("ADD b")
    addCode("SWAP " + str(reg)) # address of result in b

def load_ptr_no_D(val: Variable, reg):
    construct_value(val.memory_offset, 'b')
    construct_value(val.first_index, 'c')
    construct_value(val.index_mem, 'e')
    addCode("LOAD e")
    addCode("SUB c")
    addCode("ADD b")
    addCode("SWAP " + str(reg)) # address of result in b

def check_init(val: Variable, line):
    if not val.is_init:
        err = "Variable " + val.name + " not initiazlized"
        e_error(err, line)

def e_error(type, line):
    if os.path.exists(outputFile):
      os.remove(outputFile)
    sys.exit(("Error: " + type + ", line: " + str(line)))


parser = yacc.yacc()

print("Starting compilation...")
inputFile = sys.argv[1]
outputFile = sys.argv[2]

if os.path.exists(outputFile):
    os.remove(outputFile)

with open(inputFile, 'r') as fp:
    content = fp.read()
    print("Read file:")
    print(content)
    parser.parse(content)
    print("Compilation finished.")
