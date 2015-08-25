import string

class Instruction:
    def __init__(self, opcode, operands):
        assert len(opcode) == 2
        self.opcode = opcode
        assert type(operands) == list
        self.operands = operands

    def bake(self):
        oplen = toBase27(len(self.operands) * 2 + 1)
        assert len(oplen) == 1
        head = Baked("{opcode}{len}_".format(opcode=self.opcode, len = oplen))

        return BakerSequence([head] + [opr.bake() for opr in self.operands])

    def __repr__(self):
        return "<{} {}>".format(self.opcode, self.operands)

class Operand:
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
        return BakerSequence([Baked(head), self.payload.bake()])

    def __repr__(self):
        return "[Operand {} {} {}]".format(self.type, self.flags, self.payload)

class Label:
    def __init__(self, label):
        self.label = label

    def __repr__(self):
        return "<@label {}>".format(self.label)

    def bake(self):
        return LabelBaker(self.label)


class ConstWord:
    def __init__(self, word):
        word = expandWord(word)
        assert len(word) == 4
        self.word = word

    def bake(self):
        return Baked(self.word)

    def __repr__(self):
        return "(Word {})".format(self.word)

class RefWord:
    def __init__(self, label):
        self.label = label

    def bake(self):
        return Pointing(self.label)

    def __repr__(self):
        return "(RefWord {})".format(self.label)

class Baker:
    def render(self, table):
        return ''

    def report(self):
        return {}

    def length(self):
        return 0

    def render_total(self):
        locs = self.report()
        return self.render(locs)

class BakerSequence(Baker):
    def __init__(self, seq):
        self.seq = seq

    def render(self, table):
        result = ''
        for baker in self.seq:
            chunk = baker.render(table)
            assert len(chunk) == baker.length()
            result += chunk
        return result

    def report(self):
        locs = {}
        idx = 0
        for baker in self.seq:
            new_locs = baker.report()
            for label, sub_pos in new_locs.items():
                locs[label] = idx + sub_pos
            idx += baker.length()
        return locs

    def length(self):
        c = 0
        for baker in self.seq:
            c += baker.length()
        return c

class Baked(Baker):
    def __init__(self, s):
        self.s = s

    def render(self, table):
        return self.s

    def length(self):
        return len(self.s)

class LabelBaker(Baker):
    def __init__(self, label):
        self.label = label

    def render(self, table):
        return ''

    def report(self):
        return {self.label: 0}

    def length(self):
        return 0

class Pointing(Baker):
    def __init__(self, label):
        self.label = label

    def render(self, table):
        pointer = table[self.label]
        result = expandWord(toBase27(pointer))
        return result

    def length(self):
        return 4

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
