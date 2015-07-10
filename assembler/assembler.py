#!/usr/bin/python3
# Warning: This code is pretty bad
# I just wanted to get something working until I could work on an assembler for real.

import sys
import os
import re
import string
class Labels:
    def __init__(self):
        self.defs = {}
        self.uses = []

    def define(self,label, pos):
        self.defs[label] = pos

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
            word = self.get_loc(label)
            #print("Replacing {} with {} ({})".format(pos, label, word))
            news = self.replaceWord(news, word, pos)
        assert len(news) == len(s)
        return news

def main():
    filename = sys.argv[1]
    f = open(filename, 'r')
    whole = f.read()
    f.close()
    labels = Labels()
    assembled = ""
    for line in whole.split("\n"):
        assembled += assembleLine(line, len(assembled), labels)
    assembled = labels.replaceAll(assembled)
    print(assembled)

def assembleLine(line, index, labels):

    m = re.match('^(.*?)(#.*)?$', line)
    real_line = m.group(1)
    splitted = re.split('\s+', real_line)
    if len(splitted) == 0:
        return ""
    if splitted[0] == 'label':
        labels.define(splitted[1],index)
        return ""

    if splitted[0] == 'data':
        return ''.join(splitted[1:])
    return assembleInstruction(real_line, index, labels)

def assembleInstruction(line, index, labels):
    splitted = re.findall("\s*(\[.*?\]|\S+)", line)
    if len(splitted) == 0:
        return ""
    opcode = splitted[0]
    args = splitted[1:]
    assembled_args = ""
    argdex = index + 4
    for arg in args:
        assembled_args += assembleOperand(arg, argdex, labels)
        argdex += 8
    assert len(assembled_args) % 4 == 0
    length = toBase27(len(assembled_args)/4 + 1)
    if len(length) > 1:
        raise Exception("Length of operands is too long!")
    return opcode + length + '_' + assembled_args

def assembleOperand(arg_str, index, labels):
    match = re.match("\[\s*(\S+)\s+(.*?)\]", arg_str)
    loctype = match.group(1)
    data_descrips = re.split('\s+', match.group(2))

    flags = data_descrips[:-1]
    dat = data_descrips[-1]
    opflag = '_'
    for flag in flags:
        if flag == "neg":
            opflag = 'N'
        elif flag == "inc":
            opflat = 'I'
        elif flag == 'dec':
            opflag = 'D'

    if loctype == 'reg':
        return '__' + opflag + 'R' + getDat(dat, index + 4, labels)
    elif loctype == 'mem':
        return '__' + opflag + 'M' + getDat(dat, index + 4, labels)
    elif loctype == 'const':
        return '__' + opflag + 'C' + getDat(dat, index + 4, labels)
    elif loctype == 'io':
        return '__' + opflag + 'I' + getDat(dat, index + 4, labels)
    else:
        raise Exception("Unrecognized data type: " + loctype)

def getDat(datstr, index, labels):
    if re.match("[A-Z_]+", datstr):
        return expandWord(datstr)
    if re.match("[0-9]+", datstr):
        return expandWord(toBase27(int(datstr)))
    m = re.match(":(\S+)", datstr)
    if m:
        labels.add_use(m.group(1), index)
        return "@@@@"

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

if __name__ == '__main__':
    main()
