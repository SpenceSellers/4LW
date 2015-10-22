from pyparsing import *
import asm2 as asm

def trace(x):
    print("aa {}".format(x))
    return x

# This will oddly set the default whitespace for all pyparsing.
# We just want to avoid newlines.
Word.setDefaultWhitespaceChars(" \t")

DAT_TYPE_MAP = {
    'const': 'C',
    'reg': 'R',
    'io': 'I',
    'stack': 'S'
}

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

single_letter = Word(srange('[A-Z_]'), exact = 1)

identifier = Word(alphas + '_' + '@', alphas + '_' + nums)

filename = Word(alphas + nums + '_' + '.')

base27 = Word(alphas + '_')

opcode = Word(srange('[A-Z]'), exact = 2)

flags = ZeroOrMore(oneOf("inc dec mem neg timesfour first second third fourth"))\
    .setParseAction(lambda s,l,t: [[FLAG_MAP[a] for a in t]])

dattype = oneOf("const reg io stack")\
    .setParseAction(lambda s,l,t: DAT_TYPE_MAP[t[0]])

base10dat = Word(nums)\
    .setParseAction(lambda s,l,t: asm.ConstWord(asm.expandWord(asm.toBase27(int(t[0])))))

base27dat = base27\
    .setParseAction(lambda s,l,t: asm.ConstWord(asm.expandWord(t[0])))

referenceDat = (":" + identifier)\
    .setParseAction(lambda s,l,t: asm.RefWord(t[1]))

emptyDat = Empty().setParseAction(lambda s,l,t: asm.ConstWord('____'))

dat = MatchFirst([base10dat, base27dat, referenceDat, emptyDat])

operand = (Literal('[').suppress() + dattype + flags + dat + Literal(']').suppress())\
    .setParseAction(lambda s,l,t: asm.Operand(t[0], t[1], t[2]))

instruction = (opcode + Group(ZeroOrMore(operand)))\
    .setParseAction(lambda s,l,t: asm.Instruction(t[0], t[1].asList()))

label = (Keyword("label") + identifier)\
    .setParseAction(lambda s,l,t: asm.Label(t[1]))

fcall = (Keyword("call") + identifier + Group(ZeroOrMore(operand)) + Optional(Keyword('to').suppress() + operand, default = None))\
    .setParseAction(lambda s,l,t: asm.FunctionCall(t[1], args = t[2].asList(), to = t[3]))
        #asm.Instruction('FN', [asm.Operand('C', [], asm.RefWord(t[1]))] + t[2].asList()))


preserve = (Keyword("preserve") + Group(ZeroOrMore(single_letter)))\
    .setParseAction(lambda s,l,t:
        asm.Instruction('PU',
        [asm.Operand('S', [], asm.ConstWord('P'))] + [asm.Operand('R', [], asm.ConstWord(reg)) for reg in t[1].asList()]))

restore = (Keyword("restore") + Group(ZeroOrMore(single_letter)))\
    .setParseAction(lambda s,l,t:
        asm.Instruction('PL',
        [asm.Operand('S', [], asm.ConstWord('P'))] + [asm.Operand('R', [], asm.ConstWord(reg)) for reg in t[1].asList()]))

import_ = (Keyword('import') + filename)\
    .setParseAction(lambda s,l,t: asm.Import(t[1]))

string = (Keyword('string') + identifier + quotedString)\
    .setParseAction(lambda s,l,t: asm.String(t[1], t[2][1:-1]))

term_string = (Keyword('term_string') + identifier + quotedString)\
    .setParseAction(lambda s,l,t: asm.TerminatedString(t[1], t[2][1:-1]))

words = (Keyword('reserve') + identifier + Word(nums))\
        .setParseAction(lambda s,l,t: asm.Reserved(t[1], int(t[2])))

block = Forward()
function = Forward()
loop = Forward()
if_ = Forward()

emptyLine = Empty()

line = MatchFirst([instruction, label, fcall, preserve, restore, string, term_string, words,  import_, function, loop, if_, emptyLine]) + Optional(Literal('#') + restOfLine).suppress() + LineEnd().suppress()

preservables = Group(OneOrMore(single_letter))

block << ('{' + Group(ZeroOrMore(line)) + '}')\
    .setParseAction(lambda s,l,t: [t[1].asList()])

function << (Keyword('function') + identifier + Optional(Keyword('preserving').suppress() + preservables, default=[]) + block)\
    .setParseAction(lambda s,l,t: asm.Function(t[1], t[3], preserve = t[2]))

loop << (Keyword('loop') + block)\
    .setParseAction(lambda s,l,t: asm.Loop(t[1]))

if_ << (Keyword('if') + block + Keyword('then') + block + \
    Optional(Keyword('else') + block, default=None)\
        .setParseAction(lambda s,l,t: t[1]))\
    .setParseAction(lambda s,l,t: asm.If(t[1], t[3], otherwise = t[4]))

program = ZeroOrMore(line) + StringEnd()

def parse_program(str):
    proglist = program.parseString(str)
    return asm.Program(proglist)

#print('\n'.join([repr(p) for p in proglist]))
