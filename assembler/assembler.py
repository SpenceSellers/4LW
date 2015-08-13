#!/usr/bin/python3
# Warning: This code is pretty bad
# I just wanted to get something working until I could work on an assembler for real.

import sys
import os
import re
import string
import shlex

def log(str):
    print("Assembler: {}".format(str), file=sys.stderr)

class UndefinedLabelException(Exception):
    pass

class Labels:
    def __init__(self):
        self.defs = {}
        self.uses = []

    def define(self,label, pos):
        log("defining {} as {}".format(label, pos))
        self.defs[label] = pos

    def is_defined(self, label):
        return label in self.defs

    def add_use(self,label, pos):
        self.uses.append((label, pos))

    def get_loc(self,label):
        return expandWord(toBase27(self.defs[label]))

    def replaceWord(self, s, word, pos):
        assert len(word) == 4
        newstring = s[0:pos] + word + s[pos+4:]
        assert len(s) == len(newstring)
        return newstring

    def replaceAll(self, s):
        news = s
        for label, pos in self.uses:
            if not self.is_defined(label):
                raise UndefinedLabelException("Label {} not defined (required at binary pos {})".format(label, pos))

            word = self.get_loc(label)
            #print("Replacing {} with {} ({})".format(pos, label, word))
            news = self.replaceWord(news, word, pos)
        assert len(news) == len(s)
        return news

def main():
    filename = sys.argv[1]
    log("Assembling {}".format(filename))
    f = open(filename, 'r')
    whole = f.read()
    f.close()
    labels = Labels()

    log(os.path.dirname(__file__))

    assembled = assembleSection(whole, 0, labels)

    labels.define('PROG_END', len(assembled))
    assembled = labels.replaceAll(assembled)
    print(assembled)

def assembleSection(content, index, labels):
    assembled = ""
    for l in content.split('\n'):
        assembled += assembleLine(l, len(assembled) + index, labels)
    return assembled

def assembleLine(line, index, labels):

    m = re.match('^\s*(.*?)(#.*)?$', line)
    real_line = m.group(1)
    splitted = shlex.split(real_line)
    if len(splitted) == 0:
        return ""

    if splitted[0] == 'label':
        labels.define(splitted[1],index)
        return ""

    if splitted[0] == 'constant':
        labels.define(splitted[1],splitted[2])
        return ""

    if splitted[0] == 'data':
        labels.define(splitted[1], index)
        data = ''.join(splitted[2:])
        labels.define('$' + splitted[1], len(data))
        return data

    if splitted[0] == 'string':
        labels.define(splitted[1], index)
        s = toInternalString(' '.join(splitted[2:]))
        labels.define('$' + splitted[1], len(s))
        return s

    if splitted[0] == 'term_string':
        labels.define(splitted[1], index)
        s = toInternalString(' '.join(splitted[2:])) + 'ZZZZ'
        labels.define('$' + splitted[1], len(s)-4)
        return s

    if splitted[0] == 'import':
        f = open(splitted[1], 'r')
        contents = f.read()
        f.close()
        return assembleSection(contents, index, labels)

    if splitted[0] == 'preserve':
        # lines = '\n'.join(["MV [reg {}] [stack P]".format(reg) for reg in splitted[1:]])
        lines = 'PU [stack P]' + ''.join(['[reg {}]'.format(reg) for reg in splitted[1:]])
        return assembleSection(lines, index, labels)

    if splitted[0] == 'restore':
        lines = 'PL [stack P]' + ''.join(['[reg {}]'.format(reg) for reg in splitted[1:]])
        return assembleSection(lines, index, labels)

    if splitted[0] == 'call':
        lines = "FN [const :{}]".format(splitted[1]) + ' '.join(splitted[2:])
        return assembleSection(lines, index, labels)

    return assembleInstruction(real_line, index, labels)

def assembleInstruction(line, index, labels):
    splitted = re.findall("\s*(\[.*?\]|\S+)", line)
    if len(splitted) == 0:
        return ""
    opcode = splitted[0].upper()
    args = splitted[1:]
    assembled_args = ""
    argdex = index + 4 # index of this particular operand
    for arg in args:
        assembled_args += assembleOperand(arg, argdex, labels)
        argdex += 8 # A full operand is 8 letters long.

    assert len(assembled_args) % 8 == 0

    # The length recorded in the instruction head, in words
    length = toBase27(len(assembled_args)/4 + 1)

    if len(length) > 1:
        raise Exception("Length of operands is too long!")

    return opcode + length + '_' + assembled_args

def assembleOperand(arg_str, index, labels):
    match = re.match("\[\s*(\S*)\s*(.*?)\]", arg_str)
    try:
        loctype = match.group(1)
    except AttributeError:
        log("Bad operand parse at " + arg_str + " (index {})".format(index))
        raise
    data_descrips = re.split('\s+', match.group(2).strip())

    flags = data_descrips[:-1]
    dat = data_descrips[-1]
    opflags = []
    for flag in flags:
        if flag == "neg":
            opflags.append('N')
        elif flag == "inc":
            opflags.append('I')
        elif flag == 'dec':
            opflags.append('D')
        elif flag == 'mem':
            opflags.append('M')
        else:
            raise Exception("Unrecognized dataloc flag: {}".format(flag))

    if len(opflags) > 2:
        raise Exception("There cannot be more than two flags on an operand")

    flagstr = "{s:_>2}".format(s = ''.join(opflags))

    if loctype == 'reg':
        loctypestr = 'R'
    elif loctype == 'const':
        loctypestr = 'C'
    elif loctype == 'io':
        loctypestr = 'I'
    elif loctype == 'stack':
        loctypestr = 'S'
    else:
        raise Exception("Unrecognized data type: " + loctype)

    return '_' + flagstr + loctypestr + getDat(dat, index + 4, labels)

def getDat(datstr, index, labels):
    if datstr in [None, '']:
        return '____'
    if re.match("[A-Z_]+", datstr):
        return expandWord(datstr)
    if re.match("[0-9]+", datstr):
        return expandWord(toBase27(int(datstr)))
    m = re.match(":(\S+)", datstr)
    if m:
        labels.add_use(m.group(1), index)
        return "@@@@"

    raise Exception("Invalid dat type")

def expandWord(s):
    word =  "{:>4s}".format(s).replace(" ", "_")
    if len(word) > 4:
        raise Exception("Word is too long!: " + word)
    assert len(word) == 4
    return word

def to_base(n, base):
    digits = []
    while n > 0:
        digits.insert(0, int(n % base))
        n = int(n // base)
    return digits

def toBase27(n):
    alpha = '_' + string.ascii_uppercase
    s = ''
    digits = to_base(n, 27)
    for converted in digits:
        s += alpha[converted]
    return s

def toInternalChar(c):
    if len(c) != 1:
        raise Exception("Invalid character length")

    if c == ' ': return "__A_"

    if c in string.ascii_uppercase:
        return "___" + c

    if c in string.ascii_lowercase:
        return "__A" + c.upper()

    if c in string.digits:
        return "__N" + chr(ord('A') + int(c))

    if c == '\n': return "__C_"

    raise Exception("Unknown character for internal char: {}".format(c))

def toInternalString(s):
    return ''.join([toInternalChar(c) for c in s])


if __name__ == '__main__':
    main()
