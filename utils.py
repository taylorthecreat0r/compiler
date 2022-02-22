def saveFile(asm, outputFile):
    with open(outputFile, 'w+') as fp:
        for a in asm:
            fp.write(a + "\n")

def addComments(asm, cm):
    for idx, c in enumerate(cm):    
        spaces = (16 - len(asm[idx]))
        space = " "
        if ']' in c:
            groupIdx = c.find(']')+1
            tag = c[:groupIdx]
            desc = c[groupIdx:]
            cSpaces = (15 - len(tag))
            asm[idx] = (str(asm[idx] + space*spaces + tag + cSpaces*space + "|" + desc))
        else:
            asm[idx] = (str(asm[idx] + space*spaces + c))


asm_code = [] 

def get_asm_len():
    return len(asm_code)

def addCode(code):
    asm_code.append(code)
        
def write_to_file(f):
    with open(f, 'a') as file:
        for line in asm_code:
            file.write(line + "\n")
        
def fix_jump(diffs, id):
    curr = 0
    suff = 'x' + str(id)
    for index, line in enumerate(asm_code):
        if line.endswith(suff):
            asm_code[index] = line.replace(suff, '') + str(diffs[curr])
            curr +=1
            #print(line)
            
def fix_after_for(id, diff_neg, diff_jump):
    suff = 'iter' + str(id)
    for index, line in enumerate(asm_code):
        if line == 'JNEG ' + suff:
            asm_code[index] = line.replace(suff, '') + str(diff_neg)
        elif line == 'JUMP ' + suff:
            asm_code[index] = line.replace(suff, '') + str(diff_jump)
            