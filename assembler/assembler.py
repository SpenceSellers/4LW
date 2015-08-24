#!/usr/bin/python3
# Warning: This code is pretty bad
# I just wanted to get something working until I could work on an assembler for real.

import sys
import os
import re
import string
import shlex
import uuid

FLAG_MAP = {'neg': 'N',
            'inc': 'I',
            'dec': 'J',
            'mem': 'M',
            'timesfour': 'F',
            'first': 'A',
            'second': 'B',
            'third': 'C',
            'fourth': 'D'
            }

LOCTYPE_MAP = {
    'const': 'C',
    'reg': 'R',
    'stack': 'S',
    'io': 'I'}

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

class Assembler:
    def __init__(self, filename):
        self.filename = os.path.abspath(filename)
        self.labels = Labels()
        self.literals = []

    def add_literal(self, data, index):
        self.literals.append((data, index))

    def insert_literals(self, index):
        idx = index
        s = ""
        for data, mention_index in self.literals:
            literal_id = uuid.uuid4()
            self.labels.define(literal_id, idx)
            self.labels.add_use(literal_id, mention_index)
            idx += len(data)
            s += data
        self.literals = []
        return s

    def assemble(self):
        log("Assembling {}".format(self.filename))
        f = open(self.filename, 'r')
        whole = f.read()
        f.close()


        log(os.path.dirname(__file__))

        assembled = self.assembleSection(whole, 0, self.labels)

        self.labels.define('PROG_END', len(assembled))
        assembled = self.labels.replaceAll(assembled)

        if len(self.literals) > 0:
            log("== WARNING == Literals not inserted")
        return assembled

    def assembleSection(self, content, index, labels):
        assembled = ""
        for l in content.split('\n'):
            assembled += self.assembleLine(l, len(assembled) + index, labels)
        return assembled

    def assembleLine(self, line, index, labels):

        m = re.match('^\s*(.*?)(#.*)?$', line)
        real_line = m.group(1)
        splitted = shlex.split(real_line)
        try:
            first, rest = real_line.split(' ', 1)
        except:
            first, rest = None, None

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
            return self.import_file(splitted[1], index, labels)

        if splitted[0] == 'preserve':
            # lines = '\n'.join(["MV [reg {}] [stack P]".format(reg) for reg in splitted[1:]])
            lines = 'PU [stack P]' + ''.join(['[reg {}]'.format(reg) for reg in splitted[1:]])
            return self.assembleSection(lines, index, labels)

        if splitted[0] == 'restore':
            lines = 'PL [stack P]' + ''.join(['[reg {}]'.format(reg) for reg in splitted[1:]])
            return self.assembleSection(lines, index, labels)

        if splitted[0] == 'call':
            args = rest.split(' ', 1)
            lines = "FN [const :{}]".format(args[0]) + ' '.join(args[1:])
            return self.assembleSection(lines, index, labels)

        if splitted[0] == 'literals':
            return self.insert_literals(index)

        return self.assembleInstruction(real_line, index, labels)

    def import_file(self, filename, index, labels):
        if not os.path.isabs(filename):
            filename = os.path.join(os.path.dirname(self.filename), filename)
        f = open(filename, 'r')
        contents = f.read()
        f.close()
        return self.assembleSection(contents, index, labels)

    def assembleInstruction(self, line, index, labels):
        splitted = re.findall("\s*(\[.*?\]|\S+)", line)
        if len(splitted) == 0:
            return ""
        opcode = splitted[0].upper()
        args = splitted[1:]
        assembled_args = ""
        argdex = index + 4 # index of this particular operand
        for arg in args:
            assembled_args += self.assembleOperand(arg, argdex, labels)
            argdex += 8 # A full operand is 8 letters long.

        assert len(assembled_args) % 8 == 0

        # The length recorded in the instruction head, in words
        length = toBase27(len(assembled_args)/4 + 1)

        if len(length) > 1:
            raise Exception("Length of operands is too long!")

        return opcode + length + '_' + assembled_args

    def assembleOperand(self, arg_str, index, labels):
        insides_regex = re.compile(r'\S*".*?"|\S+')
        reg1 = re.compile(r'\[(.*)\]', re.VERBOSE)
        match = re.match(reg1, arg_str)
        try:
            insides_raw = match.group(1)
        except AttributeError:
            log("Bad operand parse at " + arg_str + " (index {}). Missing brackets?".format(index))
            raise
        insides = re.findall(insides_regex, insides_raw)
        loctype = insides[0]

        flags = insides[1:-1]
        if len(insides) > 1:
            dat = insides[-1]
        else:
            dat = None
        opflags = []
        for flag in flags:
            try:
                opflags.append(FLAG_MAP[flag.lower()])
            except:
                raise Exception("Unrecognized dataloc flag: {}".format(flag))

        if len(opflags) > 2:
            raise Exception("There cannot be more than two flags on an operand")

        flagstr = "{s:_>2}".format(s = ''.join(opflags))

        try:
            loctypestr = LOCTYPE_MAP[loctype.lower()]
        except:
            raise Exception("Unrecognized data type: " + loctype)

        return '_' + flagstr + loctypestr + self.getDat(dat, index + 4, labels)

    def getDat(self, datstr, index, labels):
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

        m = re.match("""s(.?)\"(.*)\"""", datstr)
        if m:
            s = toInternalString(m.group(2))
            if m.group(1) == 't': # Z terminated
                s += 'ZZZZ'
            self.add_literal(s, index)
            return "++++"

        raise Exception("Invalid dat type: {}".format(datstr))

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
    asm = Assembler(sys.argv[1])
    print(asm.assemble())

if __name__ == '__main__':
    main()
