import string
import bakers
import asmparse
import sys
import os

class Bakeable:
    def bake(self):
        raise NotImplementedError()

    def __repr__(self):
        return "<{}>".format(self.__class__.__name__)

    def render_all(self):
        return self.bake().render_total()

class Program(Bakeable):
    def __init__(self, bakeables):
        self.pieces = bakeables

    def bake(self):
        return bakers.BakerSequence([b.bake() for b in self.pieces])

    def __repr__(self):
        return "Program {}".format(self.pieces)

class Instruction(Bakeable):
    def __init__(self, opcode, operands):
        assert len(opcode) == 2
        self.opcode = opcode
        assert type(operands) == list
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
        return "[Operand {} {} {}]".format(self.type, self.flags, self.payload)

class Label(Bakeable):
    def __init__(self, label):
        self.label = label

    def __repr__(self):
        return "<@label {}>".format(self.label)

    def bake(self):
        return bakers.LabelBaker(self.label)


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

    if c == '\n': return "__C_"

    raise Exception("Unknown character for internal char: {}".format(c))

def toInternalString(s):
    return ''.join([toInternalChar(c) for c in s])

def main():
    filename = sys.argv[1]
    f = open(filename, 'r')
    raw_prog = f.read()

    print(asmparse.parse_program(raw_prog).render_all())

if __name__ == '__main__':
    main()
