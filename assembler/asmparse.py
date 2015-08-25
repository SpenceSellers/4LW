from pyparsing import *
import asm2 as asm

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

single_letter = Word(srange('[A-Z]'), exact = 1)

identifier = Word(alphas)

base27 = Word(alphas + '_')

opcode = Word(srange('[A-Z]'), exact = 2)

flags = ZeroOrMore(oneOf("inc dec mem timesfour first second third fourth"))\
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

fcall = (Keyword("call") + identifier + Group(ZeroOrMore(operand)))\
    .setParseAction(lambda s,l,t:
        asm.Instruction('FN', [asm.Operand('C', [], asm.RefWord(t[1]))] + t[2].asList()))

preserve = (Keyword("preserve") + Group(ZeroOrMore(single_letter)))\
    .setParseAction(lambda s,l,t:
        asm.Instruction('PU',
        [asm.Operand('S', [], asm.ConstWord('P'))] + [asm.Operand('R', [], asm.ConstWord(reg)) for reg in t[1].asList()]))

line = MatchFirst([instruction, label, fcall, preserve]) - Optional(Literal('#') + restOfLine).suppress() + LineEnd().suppress()

program = ZeroOrMore(line) + StringEnd()

proglist = program.parseString("""call here
HL
label here
preserve A B C D
MV [const 20] [io]
RT
HL
""").asList()

print('\n'.join([repr(p) for p in proglist]))

print(asm.BakerSequence([l.bake() for l in proglist]).render_total())

#print([p.render() for p in proglist])
