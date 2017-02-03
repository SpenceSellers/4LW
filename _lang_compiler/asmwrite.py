''' This module makes it easier to write 4LW assembly code programatically. '''


import uuid
import string
import copy

from enum import Enum

def expandWord(s):
    word = "{:>4s}".format(s).replace(" ", "_")
    if len(word) > 4:
        raise Exception("Word is too long!: " + word)
    assert len(word) == 4
    return word

def toBase27(n):
    alpha = '_' + string.ascii_uppercase
    s = ''
    digits = to_base(n, 27)
    for converted in digits:
        s += alpha[converted]
    return s

def to_base(n, base):
    digits = []
    while n > 0:
        digits.insert(0, int(n % base))
        n = int(n // base)
    return digits

def toWord(n):
    return expandWord(toBase27(n))

def comment(str):
    return "# {}\n".format(str)

class DataFlag(Enum):
    INC = 'inc'
    DEC = 'dec'
    MEM = 'mem'
    NEG = 'neg'
    TIMESFOUR = 'timesfour'
    PLUSFOUR = 'plusfour'

class LocType(Enum):
    REG = 'reg'
    CONST = 'const'
    IO = 'io'
    STACK = 'stack'
    TAPE = 'tape'

class Opcode(Enum):
    MOVE = "MV"
    ADD = "AD"
    SUB = "SB"
    MUL = "ML"
    DIV = "DV"
    MOD = "MD"
    JUMP = "JP"
    JUMPEQUAL = "JE"
    JUMPNOTEQUAL = "JN"
    JUMPZERO = "JZ"
    JUMPGREATER = "JG"
    JUMPLESSER = "JL"
    FUNCCALL = "FN"
    READ = 'RD'
    RETURN = "RT"
    HALT = "HL"
    AND = "AN"
    NOP = "__"

class Stacks(Enum):
    RESULT = 'V'
    ARGS = 'S'

class ConstWord:
    def __init__(self, val):
        if type(val) is int:
            self.word = toWord(val)
        elif type(val) is str:
            self.word = expandWord(val)
        else:
            raise Exception("Uh oh")

    def emit(self):
        return self.word

    def is_zero(self):
        return self.word == '____'

    def __eq__(self, other):
        if type(self) == type(other):
            return self.word == other.word
        else:
            return False

class RefWord:
    def __init__(self, ident):
        self.ident = ident

    def emit(self):
        return ':' + self.ident

class DataLoc:
    def __init__(self, loctype, payload=ConstWord(0), flags = []):
        assert type(payload) != str
        self.loctype = loctype
        self.payload = payload
        self.flags = flags

    def emit(self):
        flags = ' '.join(f.value for f in self.flags)
        return "[{} {} {}]".format(self.loctype.value, flags, self.payload.emit())

    def can_add_flag(self):
        return len(self.flags) <= 1

    def add_flag(self, flag):
        assert len(self.flags) <= 1
        self.flags.insert(0, flag)

    def with_flag(self, flag):
        assert self.can_add_flag(), "Cannot add new flag to dataloc!"
        new = copy.deepcopy(self)
        new.add_flag(flag)
        return new

    def __eq__(self, other):
        return self.loctype == other.loctype and self.flags == other.flags and self.payload == other.payload


class Instruction:
    def __init__(self, opcode, args=[]):
        self.opcode = opcode
        self.args = args

        for arg in args:
            assert isinstance(arg, DataLoc)

    def emit(self):
        args = ' '.join(a.emit() for a in self.args)
        return "{} {}\n".format(self.opcode.value, args)

class Function:
    def __init__(self, name, raw, preserving=[]):
        self.name = name
        self.raw = raw
        self.preserving = preserving

    def emit(self):
        preserves = ''
        if self.preserving:
            preserves = 'preserving ' + ' '.join(self.preserving) + ' '

        return "function {} {} {{\n{}}}\n".format(self.name, preserves, indent_text(self.raw))

class Label:
    def __init__(self, label):
        self.label = label

    def emit(self):
        return "label {}\n".format(self.label)

    def as_dataloc(self):
        return DataLoc(LocType.CONST, RefWord(self.label))


class AnonLabel(Label):
    def __init__(self, hint=""):
        super().__init__(ident(hint=hint))

class Jump:
    def __init__(self, to):
        assert isinstance(to, DataLoc)
        self.to = to

    def emit(self):
        return Instruction(Opcode.JUMP, [self.to]).emit()

class TermString:
    def __init__(self, name, str):
        self.name = name
        self.str = str

    def emit(self):
        return "term_string {} \"{}\"\n".format(self.name, self.str)

class Reserved:
    def __init__(self, name, len=1):
        self.name = name
        self.len = len

    def emit(self):
        return "reserve {} {}".format(self.name, self.len)

def ident(hint=''):
    return str(hint) + '_' +  str(uuid.uuid4())[:8]


def indent_text(text, amount=4, ch=' '):
    padding = amount * ch
    lines = text.split('\n')
    padded = [line if len(line) == 0 else padding + line for line in lines]
    return "\n".join(padded)

#print(Function('myfunc', [Instruction(Opcode.MOVE, [DataLoc(LocType.REG, ConstWord(0))])]).emit())
