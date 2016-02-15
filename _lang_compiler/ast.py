import asmwrite as asm
import abc
import os
import sys
import var_types as types

TEMPSTACK = asm.DataLoc(asm.LocType.STACK, asm.ConstWord('_'))
ARGSTACK = asm.DataLoc(asm.LocType.STACK, asm.ConstWord(asm.Stacks.ARGS.value))
RESULT_STACK = asm.DataLoc(asm.LocType.STACK, asm.ConstWord(asm.Stacks.RESULT.value))
MAX_WORD = asm.DataLoc(asm.LocType.CONST, asm.ConstWord("ZZZZ"))
ZERO_WORD = asm.DataLoc(asm.LocType.CONST, asm.ConstWord(0))
RETURN_LOC = asm.DataLoc(asm.LocType.CONST, asm.RefWord('@return'))

def log(s):
    print(s, file=sys.stderr)

class Context:
    def __init__(self, parent = None):
        self.available_registers = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K']
        self.available_registers.reverse()
        self.vars = {}
        self.strings = []
        self.parent = parent
        self.functions = {}
        self.reserved_mem = {}
        self.types = {}

        self.types['Node'] = types.Struct(['data', 'next'])

        # TODO function return values.

    def reserve_register(self):
        try:
            return self.available_registers.pop()
        except IndexError:
            return None

    def make_var(self, name):
        reg = self.reserve_register()
        self.vars[name] = reg

    def get_reg_for_var(self, name):
        return self.vars[name]

    def var_exists(self, name):
        return name in self.vars

    def get_loc_for_name(self, name):
        if self.var_exists(name):
            return asm.DataLoc(asm.LocType.REG, asm.ConstWord(self.get_reg_for_var(name)))
        else:
            return asm.DataLoc(asm.LocType.CONST, asm.RefWord(name))

    def get_used_regs(self):
        regs = set()
        for var, reg in self.vars.items():
            regs.add(reg)

        return regs

    def register_fn(self, name, returns):
        self.functions[name] = returns


    def does_fn_return_value(self, name):
        try:
            return self.functions[name]
        except:
            # Recurse up parent tree until we maybe find someone who knows about this fn.
            if self.parent:
                return self.parent.does_fn_return_value(name)
            else:
                raise SymbolNotRegisteredException("Unknown function: {}".format(name))

    def register_string(self, string):
        maybename = self.maybe_get_string_name(string)
        if maybename:
            return asm.DataLoc(asm.LocType.CONST, asm.RefWord(maybename))

        name = asm.ident('str')
        self.strings.append((string, name))
        return asm.DataLoc(asm.LocType.CONST, asm.RefWord(name))

    def maybe_get_string_name(self, string):
        for s, name in self.strings:
            if s == string:
                return name
        return None

    def reserve_mem(self, name, len=1):
        self.reserved_mem[name] = len

    def emit_reserved_mem(self):
        out = ''
        for name, len in self.reserved_mem:
            out += asm.Reserve(name, len).emit()

        return out

    def inherit(self, other):
        for entry in other.strings:
            self.strings.append(entry)

    def emit_strings(self):
        out = ''
        for string, name in self.strings:
            out += asm.TermString(name, string).emit()

        return out

class SymbolNotRegisteredException(Exception):
    pass

class Statement:
    pass

class LValue:
    @abc.abstractmethod
    def emit_assign_to(self, src, context):
        pass

class Variable(LValue):
    def __init__(self, name):
        self.name = name

    def emit_assign_to(self, src, context):
        assert type(src) is asm.DataLoc
        var_reg = context.get_reg_for_var(self.name)
        return asm.Instruction(asm.Opcode.MOVE, [src,context.get_loc_for_name(self.name)]).emit()

    def __repr__(self):
        return "LValue {}".format(self.name)

class RefLoc(LValue):
    def __init__(self, loc):
        assert isinstance(loc, Expr)
        self.loc = loc

    def emit_assign_to(self, src, context):
        assert type(src) is asm.DataLoc
        loccalc, locto = self.loc.emit_with_dest(context)
        refloc = locto.with_flag(asm.DataFlag.MEM)
        return loccalc + asm.Instruction(asm.Opcode.MOVE, [src,refloc]).emit()


class Expr:
    @abc.abstractmethod
    def emit_with_dest(self, context):
        pass

    def emit_jump_true(self, jump_dest, context):
        out = ''
        calc, calc_dest = self.emit_with_dest(context)

        out += calc
        out += asm.Instruction(asm.Opcode.JUMPGREATER, [calc_dest, ZERO_WORD, jump_dest]).emit()

        return out

    def emit_jump_false(self, jump_dest, context):
        out = ''
        calc, calc_dest = self.emit_with_dest(context)

        out += calc
        out += asm.Instruction(asm.Opcode.JUMPZERO, [calc_dest, jump_dest]).emit()

        return out

    def must_read_result(self, context):
        return False

    # It doesn't matter if this is true and claims to be false,
    # But the opposite WILL cause incorrect code to be generated.
    def is_always_true(self):
        return False;

    def optimized(self):
        return self

class ConstExpr(Expr):
    def __init__(self, val):
        self.val = asm.ConstWord(val)

    def emit_with_dest(self, context):
        return ('', asm.DataLoc(asm.LocType.CONST, self.val))

    def is_always_true(self):
        return not self.val.is_zero()




class VarExpr(Expr):
    def __init__(self, varname):
        self.varname = varname

    def emit_with_dest(self, context):
        reg = context.get_reg_for_var(self.varname)
        return ('', asm.DataLoc(asm.LocType.REG, asm.ConstWord(reg)))

class StringExpr(Expr):
    def __init__(self, string):
        self.string = string

    def emit_with_dest(self, context):
        loc = context.register_string(self.string)
        return ('', loc)

class DerefExpr(Expr):
    def __init__(self, expr):
        self.expr = expr

    def emit_with_dest(self, context):
        calc, dest = self.expr.emit_with_dest(context)
        derefed_dest = dest.with_flag(asm.DataFlag.MEM)
        return (calc, derefed_dest)

    def must_read_result(self, context):
        return self.expr.must_read_result(context)

class IncExpr(Expr):
    def __init__(self, inner):
        self.expr = inner

    def emit_with_dest(self, context):
        calc, dest = self.expr.emit_with_dest(context)
        inced_dest = dest.with_flag(asm.DataFlag.INC)
        return (calc, inced_dest)

class BiExpr(Expr):
    def __init__(self, a, b):
        assert isinstance(a, Expr)
        assert isinstance(b, Expr)
        self.a = a
        self.b = b

    @abc.abstractmethod
    def opcode(self):
        pass

    def emit_with_dest(self, context):
        acalc, adest = self.a.emit_with_dest(context)
        bcalc, bdest = self.b.emit_with_dest(context)
        ins = asm.Instruction(self.opcode(), [adest, bdest, TEMPSTACK])
        return ("{}{}{}".format(acalc, bcalc, ins.emit()), TEMPSTACK)

    def must_read_result(self, context):
        return True;

class AddExpr(BiExpr):
    def opcode(self):
        return asm.Opcode.ADD

    def optimized(self):
        if isinstance(self.b, ConstExpr) and self.b.val == '___A':
            return IncExpr(self.a)
        else:
            return self

class MulExpr(BiExpr):
    def opcode(self):
        return asm.Opcode.MUL

class SubExpr(BiExpr):
    def opcode(self):
        return asm.Opcode.SUB

class DivExpr(BiExpr):
    def opcode(self):
        return asm.Opcode.DIV

class ModExpr(BiExpr):
    def opcode(self):
        return asm.Opcode.MOD

class Negate(Expr):
    def __init__(self, inner):
        self.inner = inner

    def emit_with_dest(self, context):
        innercalc, dest = self.inner.emit_with_dest(context)
        return (innercalc, dest.with_flag(asm.DataFlag.NEG))

    def must_read_result(self, context):
        return inner.must_read_result(context)


class Biconditional(Expr):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    @abc.abstractmethod
    def emit_conditional(self, aloc, bloc, successloc):
        pass

    def emit_with_dest(self, context):
        out = ''
        equal_label = asm.AnonLabel('equal')
        equaldone_label = asm.AnonLabel('equal_done')

        acalc, adest = self.a.emit_with_dest(context)
        bcalc, bdest = self.b.emit_with_dest(context)
        out += asm.comment("Begin equality check")

        out += bcalc # Eval B
        out += acalc # Eval A

        out += self.emit_conditional(adest, bdest, equal_label.as_dataloc())

        out += asm.Instruction(asm.Opcode.MOVE, [ZERO_WORD, TEMPSTACK]).emit()
        out += asm.Jump(equaldone_label.as_dataloc()).emit()

        out += equal_label.emit()
        out += asm.Instruction(asm.Opcode.MOVE, [MAX_WORD, TEMPSTACK]).emit()
        out += equaldone_label.emit()
        out += asm.comment("End equality check")

        return (out, TEMPSTACK)

    def emit_jump_true(self, dest, context):

        out = ''
        acalc, adest = self.a.emit_with_dest(context)
        bcalc, bdest = self.b.emit_with_dest(context)
        out += bcalc
        out += acalc
        out += self.emit_conditional(adest, bdest, dest)

        return out

    def emit_jump_false(self, dest, context):
        label = asm.AnonLabel('ontrue_jumpfalse')
        out = ''
        out += self.emit_jump_true(label.as_dataloc(), context)
        out += asm.Jump(dest).emit()
        out += label.emit()
        return out

    def must_read_result(self, context):
        return True

class Equality(Biconditional):
    def emit_conditional(self, aloc, bloc, successloc):
        return asm.Instruction(asm.Opcode.JUMPEQUAL, [aloc, bloc, successloc]).emit()

class Inequality(Biconditional):
    def emit_conditional(self, aloc, bloc, successloc):
        return asm.Instruction(asm.Opcode.JUMPNOTEQUAL, [aloc, bloc, successloc]).emit()

class Greater(Biconditional):
    def emit_conditional(self, aloc, bloc, successloc):
        return asm.Instruction(asm.Opcode.JUMPGREATER, [aloc, bloc, successloc]).emit()

class Lesser(Biconditional):
    def emit_conditional(self, aloc, bloc, successloc):
        return asm.Instruction(asm.Opcode.JUMPLESSER, [aloc, bloc, successloc]).emit()

class And(Biconditional):
    def emit_conditional(self, aloc, bloc, successloc):
        return asm.Instruction(asm.Opcode.AND, [aloc, bloc, successloc]).emit()


class FCall(Expr):
    def __init__(self, fname, args=[]):
        self.fname = fname
        self.args = args
        for arg in args:
            assert isinstance(arg, Expr)

    def emit_with_dest(self, context):
        argcalcs = []
        for arg in self.args:
            calc, dest = arg.emit_with_dest(context)
            argcalcs.append(calc)
            move_to_arg_stack = asm.Instruction(asm.Opcode.MOVE, [dest, ARGSTACK])
            argcalcs.append(move_to_arg_stack.emit())

        # If the function returns a value, it's going to have to be cleared off of the
        # function result stack one way or another.
        try:
            has_result = context.does_fn_return_value(self.fname)
        except SymbolNotRegisteredException:
            # We don't know about this function, we'll just have to assume the user knows best.
            # The safe default is to not cause a stack underflow.
            has_result = False

        if has_result:
            result_dest = RESULT_STACK
        else:
            result_dest = ZERO_WORD

        call = asm.Instruction(asm.Opcode.FUNCCALL, [asm.DataLoc(asm.LocType.CONST, asm.RefWord(self.fname))])
        return (''.join(argcalcs) + call.emit(), result_dest)

    def must_read_result(self, context):
        try:
            return context.does_fn_return_value(self.fname)
        except SymbolNotRegisteredException:
            log("Unknown fn {}. Return values may not be cleared.".format(self.fname))
            return False

class Io(LValue, Expr):
    def __init__(self):
        pass

    def emit_assign_to(self, src, context):
        return asm.Instruction(asm.Opcode.MOVE, [src,asm.DataLoc(asm.LocType.IO)]).emit()

    def emit_with_dest(self, context):
        return ('', asm.DataLoc(asm.LocType.IO))

class Stack(LValue, Expr):
    def __init__(self, letter):
        assert(len(letter) == 1)
        self.letter = letter

    def emit_assign_to(self, src, context):
        return asm.Instruction(asm.Opcode.MOVE, [src,asm.DataLoc(asm.LocType.STACK, asm.ConstWord(self.letter))]).emit()

    def emit_with_dest(self, context):
        return ('', asm.DataLoc(asm.LocType.STACK, asm.ConstWord(self.letter)))

class Tape(LValue, Expr):
    def __init__(self, letter):
        assert(len(letter) == 1)
        self.letter = letter

    def emit_assign_to(self, src, context):
        return asm.Instruction(asm.Opcode.MOVE, [src,asm.DataLoc(asm.LocType.TAPE, asm.ConstWord(self.letter))]).emit()

    def emit_with_dest(self, context):
        return ('', asm.DataLoc(asm.LocType.TAPE, asm.ConstWord(self.letter)))

class StructAccess(LValue, Expr):
    def __init__(self, type_name, base, field_name):
        self.type_name = type_name

        assert isinstance(base, Expr)
        self.base = base

        assert isinstance(field_name, str)
        self.field_name = field_name

    def emit_assign_to(self, src, context):
        out = ''
        type = context.types[self.type_name]
        offset = type.get_field_offset(self.field_name)

        basecalc, base_dest = self.base.emit_with_dest(context)
        out += basecalc

        out += asm.Instruction(asm.Opcode.ADD, [base_dest, asm.DataLoc(asm.LocType.CONST, asm.ConstWord(offset)), TEMPSTACK]).emit()
        out += asm.Instruction(asm.Opcode.MOVE, [src, TEMPSTACK.with_flag(asm.DataFlag.MEM)]).emit()

        return out

    def emit_with_dest(self, context):

        type = context.types[self.type_name]
        offset = type.get_field_offset(self.field_name)

        out = ''
        basecalc, base_dest = self.base.emit_with_dest(context)
        out += basecalc

        out += asm.Instruction(asm.Opcode.ADD, [base_dest, asm.DataLoc(asm.LocType.CONST, asm.ConstWord(offset)), TEMPSTACK]).emit()

        return (out, TEMPSTACK.with_flag(asm.DataFlag.MEM))




class Assignment(Statement):
    def __init__(self, lvalue, rvalue):
        assert isinstance(lvalue, LValue)
        assert isinstance(rvalue, Expr)
        self.lvalue = lvalue
        self.rvalue = rvalue

    def emit(self, context):
        calc, dest = self.rvalue.optimized().emit_with_dest(context)
        return calc + self.lvalue.emit_assign_to(dest, context)

    def __repr__(self):
        return "<assign {} := {}>".format(self.lvalue, self.rvalue)

class ReserveMem(Statement):
    def __init__(self, name, len=1):
        self.name = name
        self.len = len

    def emit(self, context):
        context.reserve_mem(self.name, self.len)
        return ''

class DeclareVar(Statement):
    def __init__(self, varname):
        assert type(varname) is str
        self.varname = varname

    def emit(self, context):
        context.make_var(self.varname)
        return ''

class DeclareFunction(Statement):
    def __init__(self, name, returns):
        self.name = name
        self.returns = returns

    def emit(self, context):
        context.register_fn(self.name, self.returns)
        return ''

class Sequence(Statement):
    def __init__(self, statements):
        self.statements = statements

    def emit(self, context, indent = 0):
        asm = ''
        for statement in self.statements:
            assert isinstance(statement, Statement)
            asm += indent_text(statement.emit(context), indent)

        return asm

    def emit_top_level(self, context):
        out = ''
        out += self.emit(context)
        out += context.emit_strings()
        out += context.emit_reserved_mem()
        return out

    def __repr__(self):
        return "Sequence: {}".format(self.statements)

class Function(Statement):
    def __init__(self, name, args, sequence):
        self.name = name
        self.args = args
        self.sequence = sequence

    def emit(self, context):
        c = Context(parent=context)
        move_args = ''
        # Args are pushed backwards
        for arg in reversed(self.args):
            c.make_var(arg)
            reg = c.get_loc_for_name(arg)
            move_args += asm.Instruction(asm.Opcode.MOVE, [ARGSTACK, reg]).emit()

        inner = move_args + self.sequence.emit(c)
        to_preserve = c.get_used_regs()
        context.inherit(c)
        return asm.Function(self.name, inner, preserving = to_preserve).emit()

class ExprStatement(Statement):
    def __init__(self, expr):
        assert isinstance(expr, Expr)
        self.expr = expr

    def emit(self, context):
        out = ''
        calc, dest = self.expr.emit_with_dest(context)
        out += calc
        if self.expr.must_read_result(context):
            # out += asm.Instruction(asm.Opcode.MOVE, [dest, asm.DataLoc(asm.LocType.CONST, asm.ConstWord(0))]).emit()
            out += asm.Instruction(asm.Opcode.READ, [dest]).emit()
        return out

class Return(Statement):
    def __init__(self, expr = None):
        self.expr = expr

    def emit(self, context):
        if self.expr:
            calc, dest = self.expr.emit_with_dest(context)
            saveval = asm.Instruction(asm.Opcode.MOVE, [dest, RESULT_STACK])
            retjump = asm.Jump(RETURN_LOC)
            return calc + saveval.emit() + retjump.emit()
        else:
            return asm.Jump(RETURN_LOC).emit()

class Halt(Statement):
    def emit(self, context):
        return asm.Instruction(asm.Opcode.HALT).emit()

class Nop(Statement):
    def emit(self, context):
        return asm.Instruction(asm.Opcode.NOP).emit()

class If(Statement):
    def __init__(self, condition, then, otherwise = None):
        self.condition = condition
        self.then = then
        self.otherwise = otherwise

    def emit(self, context):
        then_label = asm.AnonLabel("then")
        # else_label = asm.AnonLabel("else")
        endif_label = asm.AnonLabel("endif")

        out = ''
        out += asm.comment("Begin If")
        if self.condition.is_always_true():
            out += asm.Jump(then_label.as_dataloc()).emit()
        else:
            out += self.condition.emit_jump_true(then_label.as_dataloc(), context)
        out += asm.comment("else")
        if self.otherwise:
            out += self.otherwise.emit(context)
        out += asm.Jump(endif_label.as_dataloc()).emit()
        out += asm.comment("then")
        out += then_label.emit()
        out += self.then.emit(context)
        out += endif_label.emit()
        out += asm.comment("End if")
        return out

class While(Statement):
    def __init__(self, condition, inner):
        self.cond = condition
        self.inner = inner

    def emit(self, context):
        startlabel = asm.AnonLabel('while')
        endwhile = asm.AnonLabel('endwhile')

        if self.cond.is_always_true():
            # This while is an infinite loop
            test = ''
        else:
            test = self.cond.emit_jump_false(endwhile.as_dataloc(), context)

        innercode = self.inner.emit(context)

        jumptop = asm.Jump(startlabel.as_dataloc())

        return startlabel.emit() + test + innercode + jumptop.emit() + endwhile.emit()

class Include(Statement):
    def __init__(self, path):
        self.path = path

    def compile(self):
        import compiler
        extension = os.path.splitext(self.path)[1]
        if extension.lower() in ('.4lwa', '.asm'):
            f = open(self.path, 'r')
            text = f.read()
            f.close()
            return text + '\n'
        else:
            # We'll hope that this is the right language.
            return compiler.compile_file(self.path)


    def emit(self, context):
        return self.compile()

class Asm(Statement):
    def __init__(self, text):
        self.text = text

    def emit(self, context):
        return self.text + '\n'

class Goto(Statement):
    def __init__(self, labelname):
        self.labelname = labelname

    def emit(self, context):
        return asm.Jump(asm.DataLoc(asm.LocType.CONST, asm.RefWord(self.labelname))).emit()

class Label(Statement):
    def __init__(self, name):
        self.name = name

    def emit(self, context):
        return asm.Label(self.name).emit()

def indent_text(text, amount=4, ch=' '):
    padding = amount * ch
    lines = text.split('\n')
    padded = [line if len(line) == 0 else padding + line for line in lines]
    return "\n".join(padded)
    #return padding + ('\n'+padding).join(lines)
