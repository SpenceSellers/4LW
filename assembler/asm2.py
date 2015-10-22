#!/usr/bin/env python

import string
import bakers
import asmparse
import sys
import os
import uuid
import copy

def log(s):
    print(s, file=sys.stderr)

def trace(obj, desc = 'Traced'):
    log("{}: {}".format(desc, obj))
    return obj

charTable = {
    '\n': '__C_',
    ':': '__PC',
    '!': '__PX',
    '%': '__PP',
}
class Bakeable:
    def bake(self):
        raise NotImplementedError()

    def __repr__(self):
        return "<{}>".format(self.__class__.__name__)

    def render_all(self):
        return self.bake().render_total()

    def debug_labels(self):
        return self.bake().debug_labels()

class Program(Bakeable):
    def __init__(self, bakeables):
        self.pieces = bakeables

    def bake(self):
        return bakers.BakerSequence([b.bake() for b in self.pieces])

    def __repr__(self):
        return "Program {}".format(self.pieces)

class Function(Bakeable):
    def __init__(self, name, bakeables, preserve = []):
        self.name = name
        self.preserves = preserve
        self.pieces = bakeables

    def bake(self):
        elems = []
        label = Label(self.name).bake() # Function label
        preserves = Instruction('PU', [Operand('S', [], ConstWord('P'))] + [Operand('R', [], ConstWord(reg)) for reg in self.preserves]).bake()
        elems.append(preserves)
        elems.extend([b.bake() for b in self.pieces]) # Actual body
        elems.append(Label("@return").bake())
        restores = Instruction('PL', [Operand('S', [], ConstWord('P'))] + [Operand('R', [], ConstWord(reg)) for reg in reversed(self.preserves)]).bake()
        elems.append(restores)
        elems.append(Instruction('RT',[]).bake())
        return bakers.BakerSequence([label, bakers.CaptureScopeBaker(bakers.BakerSequence(elems))])
        #return bakers.BakerSequence([label] + elems)

    def __repr__(self):
        return "Function {}: {}".format(self.name, self.pieces)

class Loop(Bakeable):
    def __init__(self, bakeables):
        self.pieces = bakeables

    def bake(self):
        name = uuid.uuid4()
        elems = []
        elems.append(Label(name).bake())
        elems.append(Label('@continue').bake())
        elems.extend([b.bake() for b in self.pieces])
        jumpback = Instruction('JP', [Operand('C', [], RefWord(name))])
        elems.append(jumpback.bake())
        elems.append(Label('@break').bake())
        return bakers.CaptureScopeBaker(bakers.BakerSequence(elems))

class If(Bakeable):
    def __init__(self, condition, then, otherwise = None):
        self.cond = condition
        self.then = then
        self.otherwise = otherwise
        print("Otherwise is " + otherwise)

    def bake(self):
        elems = []
        cond = copy.copy(self.cond)
        cond.append(Instruction('JP', [Operand('C', [], RefWord('@endif'))]))
        then = copy.copy(self.then)
        then.insert(0, Label('@true'))
        then.append(Label('@endif'))
        then.append(Label('@false'))
        elems += cond
        elems += then

        if self.otherwise:
            log(type(self.otherwise))
            elems += self.otherwise

        log("Elems is {} ".format(elems))
        return bakers.CaptureScopeBaker(bakers.BakerSequence([b.bake() for b in elems]))

class FunctionCall(Bakeable):
    def __init__(self, fname, args = [], to = None):
        self.fname = fname
        assert type(args) == list, "Type of fcall args is {}".format(type(args))
        self.args = args
        self.to = to

    def bake(self):
        elems = []
        ins = Instruction('FN', [Operand('C', [], RefWord(self.fname))] + self.args)
        elems.append(ins)
        if self.to:
            elems.append(Instruction('MV', [Operand('S', [], ConstWord('V')), self.to]))
        return bakers.BakerSequence([e.bake() for e in elems])

    def __repr__(self):
        return "<FunctionCall {} args {}>".format(self.fname, self.args)

class Instruction(Bakeable):
    def __init__(self, opcode, operands):
        assert len(opcode) == 2
        self.opcode = opcode
        assert type(operands) == list, "Operands are {}".format(operands)
        self.operands = operands

    def bake(self):
        oplen = toBase27(len(self.operands) * 2 + 1)
        assert len(oplen) == 1
        head = bakers.Baked("{opcode}{len}_".format(opcode=self.opcode, len = oplen))

        return bakers.BakerSequence([head] + [opr.bake() for opr in self.operands])

    def __repr__(self):
        return "<{} {}>".format(self.opcode, self.operands)

class Operand(Bakeable):
    def __init__(self, type, flags, payload):
        self.type = type
        assert len(self.type) == 1

        self.flags = flags

        if len(self.flags) > 2:
            raise Exception("There cannot be more than two flags on an operand")

        self.payload = payload

    def bake(self):
        flagstr = "{s:_>2}".format(s = ''.join(self.flags))
        head = "_{flags}{type}".format(type = self.type, flags = flagstr)
        return bakers.BakerSequence([bakers.Baked(head), self.payload.bake()])

    def __repr__(self):
        return "[Opr {} {} {}]".format(self.type, self.flags if self.flags else '', self.payload)

class Label(Bakeable):
    def __init__(self, label):
        self.label = label

    def __repr__(self):
        return "<@label {}>".format(self.label)

    def bake(self):
        return bakers.LabelBaker(self.label)

class Reserved(Bakeable):
    def __init__(self, label, length):
        self.label = label
        self.length = length

    def bake(self):
        return bakers.Baked('____' * self.length, self.label)
    
class ConstWord(Bakeable):
    def __init__(self, word):
        word = expandWord(word)
        assert len(word) == 4
        self.word = word

    def bake(self):
        return bakers.Baked(self.word)

    def __repr__(self):
        return "(Word {})".format(self.word)

class RefWord(Bakeable):
    def __init__(self, label):
        self.label = label

    def bake(self):
        return bakers.Pointing(self.label)

    def __repr__(self):
        return "(RefWord {})".format(self.label)

class String(Bakeable):
    def __init__(self, label, s):
        self.s = s
        self.label = label

    def bake(self):
        internal_s = toInternalString(self.s)
        return bakers.Baked(internal_s, label = self.label)

class TerminatedString(Bakeable):
    def __init__(self, label, s):
        self.s = s
        self.label = label

    def bake(self):
        internal_s = toInternalString(self.s) + 'ZZZZ'
        return bakers.Baked(internal_s, label = self.label)

class Import(Bakeable):
    def __init__(self, filename):
        self.filename = filename

    def bake(self):
        text = open(self.filename, 'r').read()
        return asmparse.parse_program(text).bake()

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
    if c == '0':
        return "__N_"
    if c in string.digits:
        return "__N" + chr(ord('A') + int(c) - 1)

    if c in charTable:
        return charTable[c]


    raise Exception("Unknown character for internal char: {}".format(c))

def toInternalString(s):
    return ''.join([toInternalChar(c) for c in s])

def main():
    filename = sys.argv[1]
    f = open(filename, 'r')
    raw_prog = f.read()

    prog = asmparse.parse_program(raw_prog)

    #print(prog.debug_labels())
    
    print(prog.render_all())

if __name__ == '__main__':
    main()
