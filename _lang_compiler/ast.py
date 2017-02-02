import asmwrite as asm
import abc
import os
import sys
import var_types as types
import traceback
# The DataLoc of a stack that _lang will use for temporary values.
TEMPSTACK = asm.DataLoc(asm.LocType.STACK, asm.ConstWord('_'))
# The DataLoc of the stack that _lang will use to pass function arguments
ARGSTACK = asm.DataLoc(asm.LocType.STACK, asm.ConstWord(asm.Stacks.ARGS.value))
# The DataLoc of the stack that will be used to return function results
RESULT_STACK = asm.DataLoc(asm.LocType.STACK, asm.ConstWord(asm.Stacks.RESULT.value))
# The biggest word that 4LW can represent.
MAX_WORD = asm.DataLoc(asm.LocType.CONST, asm.ConstWord("ZZZZ"))
ZERO_WORD = asm.DataLoc(asm.LocType.CONST, asm.ConstWord(0))
# This dataloc points to the local function return label.
RETURN_LOC = asm.DataLoc(asm.LocType.CONST, asm.RefWord('@return'))

def log(s):
    print(s, file=sys.stderr)

class Context:
    ''' Contexts store information about scopes/functions.

    Contexts can have children contexts, so they form a kind of tree. 
    Contexts manage register allocation and mapping variable names to registers.
    '''
    def __init__(self, parent = None, name = None):
        # These are the registers available for local use.
        # 4LW has more registers than this, some of them are used by the hardware,
        # and others might be reserved for explicit program use.
        self.available_registers = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K']

        # We'll just reverse the list so that .pop() uses them in a prettier order.
        self.available_registers.reverse()

        # Maps variable names to registers.
        self.vars = {}

        # Local strings
        self.strings = []

        # The parent scope. Will be None for a root context.
        self.parent = parent

        # Stores function names. This has to be different from variables,
        # because variables are stored in registers. Better to use constant 
        # memory addresses for functions.
        # The keys are the function names, the values are booleans that 
        # represent whether or not the function returns a word.
        self.functions = {}

        # Stores uninitialized reserved memory. 
        # Key is the name, value is the length of stored memory in words.
        self.reserved_mem = {}

        self.types = {}

        # The context's name. This is for debugging mostly.
        self.name = name


    def reserve_register(self):
        ''' Asks to be reserved a register. Returns the register you were assigned. '''        
        try:
            return self.available_registers.pop()
        except IndexError:
            return None

    def make_var(self, name):
        ''' Creates a named variable. Internally this reserves a register, but the caller
        doesn't need to know which one yet. '''
        reg = self.reserve_register()
        self.vars[name] = reg

    def get_reg_for_var(self, name):
        ''' Returns the register that a named variable is assigned to. '''
        return self.vars[name]

    def var_exists(self, name):
        ''' Does this variable actually exist? '''
        return name in self.vars

    def get_loc_for_name(self, name):
        ''' Gets the dataloc for an identifier. 

        If it's a plain named variable, you'll get the dataloc to that register.
        If not, you'll get a reference to that assembly label.

        That's kind of scary. If the programmer references a variable that doesn't exist,
        the program will compile into assembly. The assembler will blow up though, since
        the label probably doesn't exist. The idea is that the outputted assembly could be
        linked with external raw assembly code and the function calls would work.
        '''
        if self.var_exists(name):
            return asm.DataLoc(asm.LocType.REG, asm.ConstWord(self.get_reg_for_var(name)))
        else:
            return asm.DataLoc(asm.LocType.CONST, asm.RefWord(name))

    def get_used_regs(self):
        ''' Returns the set of used registers '''
        regs = set()
        for var, reg in self.vars.items():
            regs.add(reg)

        return regs

    def register_fn(self, name, returns):
        ''' Registers a function. 'returns' should be true if the function returns a word,
        and otherwise false. '''
        self.functions[name] = returns

    def does_fn_return_value(self, name):
        ''' Does a given function return a word, or not return anything at all?'''
        try:
            return self.functions[name]
        except:
            # Recurse up parent tree until we maybe find someone who knows about this fn.
            if self.parent:
                return self.parent.does_fn_return_value(name)
            else:
                raise SymbolNotRegisteredException("Unknown function: {}".format(name))

    def register_string(self, string):
        ''' Stores a string so that it'll be available later. '''
        maybename = self.maybe_get_string_name(string)
        if maybename:
            # This string has already been registered, we don't have to do it again.
            return asm.DataLoc(asm.LocType.CONST, asm.RefWord(maybename))

        name = asm.ident('str')
        self.strings.append((string, name))
        return asm.DataLoc(asm.LocType.CONST, asm.RefWord(name))


    def maybe_get_string_name(self, string):
        ''' Does this string already have a name? If so, get the name. '''
        for s, name in self.strings:
            if s == string:
                return name
        return None

    def reserve_mem(self, name, len=1):
        ''' Reserve raw uninitialized memory '''
        self.reserved_mem[name] = len

    def emit_reserved_mem(self):
        out = ''
        for name, len in self.reserved_mem:
            out += asm.Reserve(name, len).emit()

        return out

    def inherit(self, other):
        ''' Contexts can inherit from their children. This is done to bubble
        strings, etc up to the top level. '''
        for entry in other.strings:
            self.strings.append(entry)

        for key, value in other.types.items():
            log("{} is inheriting {} from {}".format(self, key, other))
            self.register_type(key, value)

    def emit_strings(self):
        out = ''
        for string, name in self.strings:
            out += asm.TermString(name, string).emit()

        return out

    def register_type(self, typename, type):
        self.types[typename] = type

    def get_type(self, type_name):
        if type_name in self.types:
            return self.types[type_name]
        else:
            if self.parent:
                return self.parent.get_type(type_name)
            else:
                log("  It doesn't exist, our types are {}".format(self.types.keys()))
                return None

    def __str__(self):
        if self.name:
            return "[Context {}, parent {}]".format(self.name, self.parent)
        else :
            return "[Unknown context, parent {}]".format(self.parent)

class SymbolNotRegisteredException(Exception):
    pass

class Statement:
    pass

class LValue:
    ''' LValues can be assigned to.'''
    @abc.abstractmethod
    def emit_assign_to(self, src, context):
        pass

class Variable(LValue):
    ''' The most common type of LValue. Internally it's backed by a register. '''
    def __init__(self, name):
        self.name = name

    def emit_assign_to(self, src, context):
        assert type(src) is asm.DataLoc
        var_reg = context.get_reg_for_var(self.name)
        return asm.Instruction(asm.Opcode.MOVE, [src,context.get_loc_for_name(self.name)]).emit()

    def __repr__(self):
        return "LValue {}".format(self.name)

class RefLoc(LValue):
    ''' Assigns to an assembly label to a memory location.
    Strings etc will use this. '''
    def __init__(self, loc):
        assert isinstance(loc, Expr)
        self.loc = loc

    def emit_assign_to(self, src, context):
        assert type(src) is asm.DataLoc
        loccalc, locto = self.loc.emit_with_dest(context)
        refloc = locto.with_flag(asm.DataFlag.MEM)
        return loccalc + asm.Instruction(asm.Opcode.MOVE, [src,refloc]).emit()


class Expr:
    ''' Exprs calculate values. '''
    @abc.abstractmethod
    def emit_with_dest(self, context):
        pass

    def emit_jump_true(self, jump_dest, context):
        ''' Emits a jump to the destination if this expr turns out to be non-zero 
        This is a generic impl, a lot of Exprs will want to override it.
        '''
        out = ''
        calc, calc_dest = self.emit_with_dest(context)

        out += calc
        out += asm.Instruction(asm.Opcode.JUMPGREATER, [calc_dest, ZERO_WORD, jump_dest]).emit()

        return out

    def emit_jump_false(self, jump_dest, context):
        ''' The inverse of emit_jump_true. It's a different function because they often
        can be optimized very differently'''
        out = ''
        calc, calc_dest = self.emit_with_dest(context)

        out += calc
        out += asm.Instruction(asm.Opcode.JUMPZERO, [calc_dest, jump_dest]).emit()

        return out

    def must_read_result(self, context):
        ''' Does the caller HAVE to run the emit_with_dest() of this Expr?
        Many Exprs will put results on the stack, and you don't want to let those
        stick around. '''
        return False

    # It doesn't matter if this is true and claims to be false,
    # But the opposite WILL cause incorrect code to be generated.
    def is_always_true(self):
        return False;

    def optimized(self):
        return self

class ConstExpr(Expr):
    ''' A constant value '''
    def __init__(self, val):
        self.val = asm.ConstWord(val)

    def emit_with_dest(self, context):
        return ('', asm.DataLoc(asm.LocType.CONST, self.val))

    def is_always_true(self):
        return not self.val.is_zero()

    def __eq__(self, other):
        return type(self) == type(other) and self.val == other.val

    def has_value(self, val):
        return self.val == asm.ConstWord(val)

class ConstRefExpr(Expr):
    ''' An expr that is a constant memory address of some identifier '''
    def __init__(self, name):
        self.name = name

    def emit_with_dest(self, context):
        return ('', asm.DataLoc(asm.LocType.CONST, asm.RefWord(self.name)))

class VarExpr(Expr):
    ''' Evaluates to the value of a variable '''
    def __init__(self, varname):
        self.varname = varname

    def emit_with_dest(self, context):
        reg = context.get_reg_for_var(self.varname)
        return ('', asm.DataLoc(asm.LocType.REG, asm.ConstWord(reg)))

class StringExpr(Expr):
    ''' Evaluates to the memory address of a string '''
    def __init__(self, string):
        self.string = string

    def emit_with_dest(self, context):
        loc = context.register_string(self.string)
        return ('', loc)

class DerefExpr(Expr):
    ''' Derefs the value of another expression, treating it as a memory adddress '''
    def __init__(self, expr):
        self.expr = expr

    def emit_with_dest(self, context):
        calc, dest = self.expr.emit_with_dest(context)
        derefed_dest = dest.with_flag(asm.DataFlag.MEM)
        return (calc, derefed_dest)

    def must_read_result(self, context):
        return self.expr.must_read_result(context)

class IncExpr(Expr):
    ''' Increments the value of an expression '''
    def __init__(self, inner):
        self.expr = inner

    def emit_with_dest(self, context):
        calc, dest = self.expr.emit_with_dest(context)
        inced_dest = dest.with_flag(asm.DataFlag.INC)
        return (calc, inced_dest)

class Inc4Expr(Expr):
    ''' Increments the value of an expression by four. 
    4LW can do this in hardware, so it's nice to have in _lang.'''
    def __init__(self, inner):
        self.expr = inner
    def emit_with_dest(self, context):
        calc, dest = self.expr.emit_with_dest(context)
        inced_dest = dest.with_flag(asm.DataFlag.PLUSFOUR)
        return (calc, inced_dest)

class DecExpr(Expr):
    ''' Decrements the value of an expression '''
    def __init__(self, inner):
        self.expr = inner

    def emit_with_dest(self, context):
        calc, dest = self.expr.emit_with_dest(context)
        deced_dest = dest.with_flag(asm.DataFlag.DEC)
        return (calc, deced_dest)

class BiExpr(Expr):
    ''' Abstract method for exprs that take in two 'arguments' '''
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

    def emit_with_dest(self, context):
        if isinstance(self.b, ConstExpr) and self.b.val.word == '___A':
            return IncExpr(self.a).emit_with_dest(context)
        elif isinstance(self.b, ConstExpr) and self.b.val.word == '___D':
            return Inc4Expr(self.a).emit_with_dest(context)
        else:
            return super().emit_with_dest(context)



class MulExpr(BiExpr):
    def opcode(self):
        return asm.Opcode.MUL

class SubExpr(BiExpr):
    def opcode(self):
        return asm.Opcode.SUB

    def emit_with_dest(self, context):
        if isinstance(self.b, ConstExpr) and self.b.val.word == '___A':
            return DecExpr(self.a).emit_with_dest(context)
        else:
            return super().emit_with_dest(context)

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
    ''' Abstract class that is used by Exprs that take two values and compare them somehow '''
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

    def emit_jump_true(self, dest, context):
        out = ''
        if type(self.b) is ConstExpr and self.b.has_value(0):
            acalc, adest = self.a.emit_with_dest(context)
            out += acalc
            out += asm.Instruction(asm.Opcode.JUMPZERO, [adest, dest]).emit()
            return out

        elif type(self.b) is ConstExpr and self.b.has_value('ZZZZ'):
            acalc, adest = self.a.emit_with_dest(context)
            out += acalc
            out += asm.Instruction(asm.Opcode.JUMPZERO, [adest.with_flag(asm.DataFlag.NEG), dest]).emit()
            return out
        else:
            return super().emit_jump_true(dest, context)

    def emit_jump_false(self, dest, context):
        # This would be more efficient as an inequality
        return Inequality(self.a, self.b).emit_jump_true(dest, context)

class Inequality(Biconditional):
    def emit_conditional(self, aloc, bloc, successloc):
        return asm.Instruction(asm.Opcode.JUMPNOTEQUAL, [aloc, bloc, successloc]).emit()

    def emit_jump_false(self, dest, context):
        # This would be more efficient as an equality.
        return Equality(self.a, self.b).emit_jump_true(dest, context)


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
    ''' Expr that calls a function'''
    def __init__(self, fname, args=[]):
        self.fname = fname
        self.args = args
        for arg in args:
            assert isinstance(arg, Expr)

    def emit_with_dest(self, context):
        argcalcs = []
        dests = []
        for arg in self.args:
            calc, dest = arg.emit_with_dest(context)
            argcalcs.append(calc)

            dests.append(dest)

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

        func_loc = context.get_loc_for_name(self.fname)

        call = asm.Instruction(asm.Opcode.FUNCCALL, [func_loc] + dests)
        return (''.join(argcalcs) + call.emit(), result_dest)

    def must_read_result(self, context):
        try:
            return context.does_fn_return_value(self.fname)
        except SymbolNotRegisteredException:
            log("Unknown fn {}. Return values may not be cleared.".format(self.fname))
            return False

class SizeOf(Expr):
    def __init__(self, type_name):
        self.type_name = type_name

    def emit_with_dest(self, context):
        size = context.get_type(self.type_name).len_words()
        return ('', asm.DataLoc(asm.LocType.CONST, asm.ConstWord(size)))

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
        type = context.get_type(self.type_name)
        offset = type.get_field_offset(self.field_name)

        basecalc, base_dest = self.base.emit_with_dest(context)
        out += basecalc

        loc = None
        if offset == 0:
            loc = base_dest
        elif offset == 4:
            loc = base_dest.with_flag(asm.DataFlag.PLUSFOUR)
        else:
            out += asm.Instruction(asm.Opcode.ADD, [base_dest, asm.DataLoc(asm.LocType.CONST, asm.ConstWord(offset)), TEMPSTACK]).emit()
            loc = TEMPSTACK

        out += asm.Instruction(asm.Opcode.MOVE, [src, loc.with_flag(asm.DataFlag.MEM)]).emit()

        return out

    def emit_with_dest(self, context):

        type = context.get_type(self.type_name)
        if type is None:
            raise Exception("Type {} does not exist in context {}".format(self.type_name, context))
        offset = type.get_field_offset(self.field_name)
        out = ''
        basecalc, base_dest = self.base.emit_with_dest(context)
        out += basecalc

        loc = None
        if offset == 0:
            loc = base_dest
        elif offset == 4:
            loc = base_dest.with_flag(asm.DataFlag.PLUSFOUR)
        else:
            out += asm.Instruction(asm.Opcode.ADD, [base_dest, asm.DataLoc(asm.LocType.CONST, asm.ConstWord(offset)), TEMPSTACK]).emit()
            loc = TEMPSTACK

        #out += asm.Instruction(asm.Opcode.ADD, [base_dest, asm.DataLoc(asm.LocType.CONST, asm.ConstWord(offset)), TEMPSTACK]).emit()

        return (out, loc.with_flag(asm.DataFlag.MEM))

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
    def __init__(self, varname, initial_val = None):
        assert type(varname) is str
        self.varname = varname
        assert initial_val == None or isinstance(initial_val, Expr)
        self.initial_val = initial_val;

    def emit(self, context):

        context.make_var(self.varname)
        if self.initial_val:
            return Assignment(Variable(self.varname), self.initial_val).emit(context)
        else:
            return ''

class DeclareFunction(Statement):
    def __init__(self, name, returns):
        self.name = name
        self.returns = returns

    def emit(self, context):
        context.register_fn(self.name, self.returns)
        return ''

class DeclareStruct(Statement):
    def __init__(self, name, fields):
        self.name = name
        self.fields = fields

    def emit(self, context):
        context.register_type(self.name, types.Struct(self.fields))
        return ''

class DeclareMacro(Statement):
    def __init__(self, name, internal):
        self.name = name
        self.internal = internal

    def emit(self, context):
        context.register_macro(self.name, self.internal)

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
        #out += context.emit_reserved_mem()
        return out

    def __repr__(self):
        return "Sequence: {}".format(self.statements)

class Function(Statement):
    def __init__(self, name, args, sequence, options = []):
        self.name = name
        self.args = args
        self.sequence = sequence
        self.options = set(options)

    def emit(self, context):
        if 'returns' in self.options:
            context.register_fn(self.name, True)
        else:
            context.register_fn(self.name, False)
        c = Context(parent=context, name = "Function {}".format(self.name))
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
            out = ''
            calc, dest = self.expr.emit_with_dest(context)
            out += calc
            # If the calc dest is already the return stack,
            # it's really just a waste of time to move it.
            if dest == RESULT_STACK:
                out += asm.comment("Omitting return move")
            else:
                out += asm.Instruction(asm.Opcode.MOVE, [dest, RESULT_STACK]).emit()
            out += asm.Jump(RETURN_LOC).emit()
            return out
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

    def compile(self, context):
        import compiler
        extension = os.path.splitext(self.path)[1]
        if extension.lower() in ('.4lwa', '.asm'):
            f = open(self.path, 'r')
            text = f.read()
            f.close()
            return text + '\n'
        else:
            # We'll hope that this is the right language.
            result = compiler.compile_file_with_parent_context(self.path, context)
            return result


    def emit(self, context):
        result = self.compile(context)
        #context.inherit(subcontext)
        return result

class Asm(Statement):
    '''A raw line of assembly'''
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
