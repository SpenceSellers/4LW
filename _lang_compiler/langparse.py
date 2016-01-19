from pyparsing import *
import ast

ParserElement.enablePackrat()

def addify(t): return ast.AddExpr(t[0][0], t[0][2])
def mulify(t): return ast.MulExpr(t[0][0], t[0][2])
def subify(t): return ast.SubExpr(t[0][0], t[0][2])
def divify(t): return ast.DivExpr(t[0][0], t[0][2])
def modify(t): return ast.ModExpr(t[0][0], t[0][2])
def equalify(t): return ast.Equality(t[0][0], t[0][2])
def notequalify(t): return ast.Inequality(t[0][0], t[0][2])

def greaterify(t): return ast.Greater(t[0][0], t[0][2])
def lesserify(t): return ast.Lesser(t[0][0], t[0][2])

expr = Forward()
bottom_expr = Forward()
function = Forward()
block_sequence = Forward()
special_loc = Forward()
num = Word(nums)


identifier = Word(srange('[a-z]_'), alphas + nums)
#identifier = Word(alphas)

single_letter = Word(srange('[A-Z_]'), exact = 1)

base27 = Word(alphas + '_')

num_expr = num.copy().setParseAction(lambda t: ast.ConstExpr(int(t[0])))

base27_expr = base27.copy().setParseAction(lambda t: ast.ConstExpr(t[0]))

fcall_args = Group(Optional(expr + ZeroOrMore(Suppress(',') + expr)))

fcall_expr = (identifier + '(' + fcall_args + ')').setParseAction(lambda t: ast.FCall(t[0], t[2]))

parens_expr = (Literal('(') + expr + Literal(')')).setParseAction(lambda t: t[1])

var_expr = identifier.copy().setParseAction(lambda t: ast.VarExpr(t[0]))

deref_expr = ("*" + expr).setParseAction(lambda t: ast.DerefExpr(t[1]))

string_expr = quotedString.copy().setParseAction(lambda t: ast.StringExpr(t[0][1:-1]))

bottom_expr << MatchFirst([num_expr, fcall_expr, var_expr, base27_expr, parens_expr, deref_expr, string_expr, special_loc])

expr << infixNotation(bottom_expr,
    [
        ('+', 2, opAssoc.LEFT, addify),
        ('-', 2, opAssoc.LEFT, subify),
        ('*', 2, opAssoc.LEFT, mulify),
        ('/', 2, opAssoc.LEFT, divify),
        ('%', 2 ,opAssoc.LEFT, modify),
        ('==', 2, opAssoc.LEFT, equalify),
        ('!=', 2, opAssoc.LEFT, notequalify),
        ('>', 2, opAssoc.LEFT, greaterify),
        ('<', 2, opAssoc.LEFT, lesserify),

        ('!', 1, opAssoc.RIGHT, lambda t: ast.Negate(t[0][1]))
    ])

mem_lvalue = ("*" + expr).setParseAction(lambda t: ast.RefLoc(t[1]))

io_lvalue = Keyword("IO").setParseAction(lambda t: ast.Io())

stack = (Keyword("stack") + single_letter).setParseAction(lambda t: ast.Stack(t[1]))

special_loc << ('[' + MatchFirst([io_lvalue, stack]) + ']').setParseAction(lambda t: t[1])

lvariable = identifier.copy().setParseAction(lambda t: ast.Variable(t[0]))
lvalue = MatchFirst([lvariable, mem_lvalue, special_loc])

declarevar = (Keyword('var') + identifier).setParseAction(lambda t: ast.DeclareVar(t[1]))

declarefn = (Keyword('declare') + identifier + Optional(Keyword('returns').setParseAction(lambda t: True), default = False))\
    .setParseAction(lambda t: ast.DeclareFunction(t[1], t[2]))

assignment = (lvalue + Literal(':=') + expr).setParseAction(lambda t: ast.Assignment(t[0], t[2]))

return_ = (Keyword('return') + expr).setParseAction(lambda t: ast.Return(t[1]))

halt = Keyword('halt').setParseAction(lambda t: ast.Halt())

expr_statement = expr.copy().setParseAction(lambda t: ast.ExprStatement(t[0]))

if_ = (Keyword('if') + '(' + expr + ')' + block_sequence + Optional(Keyword('else').suppress() + block_sequence, default=None))\
      .setParseAction(lambda t: ast.If(t[2], t[4], t[5]))

while_ = (Keyword('while') + '(' + expr + ')' + block_sequence)\
         .setParseAction(lambda t: ast.While(t[2], t[4]))

nop = Keyword('nop').setParseAction(lambda t: ast.Nop())

include = (Keyword('include') + QuotedString(quoteChar = '<', endQuoteChar = '>'))\
    .setParseAction(lambda t: ast.Include(t[1]))

asm = (Keyword('asm') + QuotedString(quoteChar = '\"')).setParseAction(lambda t: ast.Asm(t[1]))

goto = (Keyword('goto') + identifier).setParseAction(lambda t: ast.Goto(t[1]))

label = (Keyword('label') + identifier).setParseAction(lambda t: ast.Label(t[1]))

comment = ('#' + SkipTo(lineEnd))

line = comment.suppress() | MatchFirst([declarevar, declarefn, assignment, function, return_, halt, if_, while_, include, asm, goto, label, expr_statement, nop]) + Literal(';').suppress() + Optional(comment).suppress()

sequence = ZeroOrMore(line).setParseAction(lambda t: ast.Sequence(t.asList()))

block_sequence << ('{' + sequence + '}').setParseAction(lambda t: t[1])

function_args = Group(Optional(identifier + ZeroOrMore(Suppress(',') + identifier)))

function << (Keyword('function') + identifier + '(' + function_args + ')' + block_sequence)\
    .setParseAction(lambda t: ast.Function(t[1], t[3], t[5] ))

# lambda q: ast.Function(t[1], t[3], + t[5] )
program = sequence + StringEnd()
