import ast
import ctypes

from contextlib import contextmanager
from . import llvm


BOOL_INST = {
    ast.And: llvm.BuildAnd,
    ast.Or: llvm.BuildOr
}


class Function(object):

    @classmethod
    def wrap(class_, func, cfunc):
        # Arrange type dictionary into their respective positions.
        argtypes = [func.__n2o_argtypes__[arg] for arg in func.__n2o_args__]

        cfunc.argtypes = [t.c_type for t in argtypes]
        cfunc.restype = (func.__n2o_restype__.c_type
                         if func.__n2o_restype__ is not None
                         else None)

        return class_(cfunc, argtypes)

    def __init__(self, cfunc, argtypes):
        self.cfunc = cfunc
        self.converters = [
            t.convert if hasattr(t, "convert") else lambda a: a
            for t in argtypes
        ]

    def __call__(self, *args):
        return self.cfunc(*(c(a) for c, a in zip(self.converters, args)))


class ExternalFunction(object):
    """Stores information about externally defined function included in the module."""

    def __init__(self, name, func, restype, argtypes):
        self.func_name = name
        self.__n2o_func__ = func
        self.__n2o_restype__ = restype
        self.__n2o_argtypes__ = argtypes


class ScopedVars(object):
    """Stack of dictionaries of scoped variables.

    Top scope is the function, then any nested according to
    indented blocks (eg. if, for) as they occur. On block exit,
    the associated scope is popped off.

    """

    def __init__(self):
        self.__vars = [{}]

    def __getitem__(self, name):
        """Finds and returns local variable with a given *name*.

        Traverses the scope stack outwards until *name* is found; throws
        KeyError if unsuccessful.

        """
        for scope in self.__vars[::-1]:
            v = scope.get(name)
            if v is not None:
                return v

        raise KeyError(name)

    def __setitem__(self, name, value):
        """Sets the *name* to *value* in currently active scope."""
        self.__vars[-1][name] = value

    @contextmanager
    def scope(self):
        """Creates scope for the duration of the context manager."""
        self.__vars.append({})
        yield
        self.__vars.pop()


class Visitor(ast.NodeVisitor):

    def __init__(self, module, builder, globals_):
        self.module = module
        self.builder = builder

        # Map of value names to their nitrous types. Currently,
        # this is important only for arrays of aggregate values
        # to facilitate correct reference mechanics.
        self.types = {}

        # Global immutable symbols
        self.globals = globals_
        # Scoped local symbols (including parameters)
        self.locals = ScopedVars()

        # Stack of information for current loop and its parent ones.
        self.loop_info = []

        # Push nodes on the stack as we're traversing down the tree,
        # pop them back when we're done. This is useful for error reporting
        # since not all nodes (eg. Slice) have the lineno attribute. In this
        # case, climb up until there's a node which does have it.
        self.node_stack = []

        # Value stack used to assemble LLVM IR as the syntax tree is traversed.
        self.stack = []

    def store(self, addr, value, type_=None):
        """Stores *value* on the stack under *addr*.

        *addr* can be either a variable name or a GEP value.

        Allocates stack space if *addr* is not an existing variable. Returns
        the pointer to allocated space.

        """
        if isinstance(addr, basestring):
            name = addr
            try:
                addr = self.locals[name]
            except KeyError:
                # First time storing the variable; allocate stack space
                # and register with most nested scope.
                func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
                addr = entry_alloca(func, llvm.TypeOf(value), "v")
                self.locals[name] = addr
                # Register value type, if supplied
                if type_ is not None:
                    self.types[name] = type_

        llvm.BuildStore(self.builder, value, addr)
        return addr

    def load(self, addr):
        """Loads/returns contents of a symbol.

        *addr* can either be a variable name or GEP value.

        In case of local variable, a load instruction is generated and result is
        returned. Global constants and functions/emitters are returned directly.

        """
        if isinstance(addr, llvm.ValueRef):
            return llvm.BuildLoad(self.builder, addr, "v")
        else:
            try:
                # Try variables; they are all LLVM values and stack pointers.
                return llvm.BuildLoad(self.builder, self.locals[addr], "v")
            except KeyError:
                try:
                    return self.globals[addr]
                except KeyError:
                    raise NameError("{0} is undefined or unavailable in current scope".format(addr))

    def push(self, value, type_=None):
        """Pushes a value on expression value stack.

        Optionally, associates nitrous *type_* with the value, which
        can later be requested with ``typeof()``.

        """
        self.stack.append(value)
        if type_ is not None:
            name = llvm.GetValueName(value)
            assert isinstance(value, llvm.ValueRef)
            assert name not in self.types
            self.types[name] = type_

    def pop(self):
        """Pops top value from expression value stack."""
        return self.stack.pop()

    def typeof(self, v):
        """Returns nitrous type for value *v*.

        Returns None if *v* has no known type association. *v* can be
        either an LLVM value or value name.

        """
        name = llvm.GetValueName(v) if isinstance(v, llvm.ValueRef) else v
        return self.types.get(name, None)

    def visit(self, node):
        from .exceptions import TranslationError

        try:
            self.node_stack.append(node)
            super(Visitor, self).visit(node)
        except (NameError, ValueError, TypeError, NotImplementedError), e:
            # Use translation error tag the exception with line number and
            # carry it upwards to translation routine where the traceback is reported.
            lineno = next(n for n in self.node_stack[::-1] if hasattr(n, "lineno")).lineno
            raise TranslationError(type(e), lineno, e.args[0])
        finally:
            self.node_stack.pop()

    def visit_Num(self, node):
        self.push(emit_constant(node.n))

    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):
            self.push(self.load(node.id), self.typeof(node.id))
        elif isinstance(node.ctx, ast.Store):
            self.push(node.id)
        else:
            raise NotImplementedError("Unknown Name context {0!s}".format(type(node.ctx)))

    def visit_Attribute(self, node):
        from .types import Reference

        self.generic_visit(node)

        v = self.pop()
        vt = self.typeof(v)

        if isinstance(vt, Reference):
            # References to structures
            vt = vt.value_type
            if isinstance(node.ctx, ast.Load):
                a, at = vt.emit_getattr(self.builder, v, node.attr)
            elif isinstance(node.ctx, ast.Store):
                a, at = vt.emit_setattr(self.builder, v, node.attr)
            else:
                raise NotImplementedError("Unsupported attribute context {0}".format(node.ctx))
        else:
            # Assume regular python object
            if isinstance(node.ctx, ast.Load):
                a, at = getattr(v, node.attr), None
            else:
                raise NotImplementedError("Unsupported attribute context {0}".format(node.ctx))

        self.push(a, at)

    def visit_Tuple(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        v = tuple([self.pop() for _ in node.elts][::-1])
        self.push(v)

    def visit_Index(self, node):
        if isinstance(node.value, ast.Tuple):
            # Tuple visitor will push the value onto the stack itself
            ast.NodeVisitor.generic_visit(self, node)
        else:
            # Assume everything else will produce a single index value.
            ast.NodeVisitor.generic_visit(self, node)
            self.push((self.pop(),))

    def visit_Subscript(self, node):
        """Label subscript of form `var_expr[index_expr]`.

        If in Load() context, return the value; if in Store()
        context, return the prepared GEP; parent node which knows
        about the source data will complete the instruction.

        """
        ast.NodeVisitor.generic_visit(self, node)
        i = self.pop()
        v = self.pop()

        # Index is a nd tuple
        vt = self.typeof(v)

        if isinstance(node.ctx, ast.Load):
            e, et = vt.emit_getitem(self.builder, v, i)
        elif isinstance(node.ctx, ast.Store):
            e, et = vt.emit_setitem(self.builder, v, i)
        else:
            raise NotImplementedError("Unsupported subscript context {0}".format(node.ctx))

        self.push(e, et)

    def visit_Slice(self, node):
        raise NotImplementedError("Slices are not supported")

    def visit_Delete(self, node):
        raise NotImplementedError("`del`etions are not supported")

    def visit_Assign(self, node):
        target = node.targets[0]
        if len(node.targets) > 1:
            raise NotImplementedError("Chained assignment is not supported")

        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.pop()

        if isinstance(target, (ast.Name, ast.Subscript, ast.Attribute)):
            # Handled cases: lhs = rhs, *lhs_gep = rhs
            self.store(self.pop(), rhs, self.typeof(rhs))
        else:
            raise NotImplementedError("Unsupported assignment target {0}"
                                      .format(node.targets[0]))

    def visit_AugAssign(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.pop()

        if isinstance(node.target, (ast.Name, ast.Subscript, ast.Attribute)):
            # Handled cases: lhs = rhs, *lhs_gep = rhs
            lhs_addr = self.pop()
            rhs = emit_binary_op(self.builder, node.op, self.load(lhs_addr), rhs)
            # No need to store type, since the target already exists
            self.store(lhs_addr, rhs)
        else:
            raise NotImplementedError("Unsupported augmented assignment target {0}"
                                      .format(node.target))

    def visit_Return(self, node):
        from .types import Bool, types_equal

        ast.NodeVisitor.generic_visit(self, node)

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        return_type = llvm.function_return_type(func)

        if llvm.GetTypeKind(return_type) == llvm.VoidTypeKind:
            if len(self.stack) > 0:
                raise ValueError("No return value expected")

            llvm.BuildRetVoid(self.builder)

        else:
            v = self.pop()
            t = llvm.TypeOf(v)
            # Special case; if we're returning boolean, cast to i8
            # FIXME Move this to Bool.emit_cast_to or similar?
            if (llvm.GetTypeKind(t) == llvm.IntegerTypeKind and llvm.GetIntTypeWidth(t) == 1):
                v = llvm.BuildCast(self.builder, llvm.ZExt, v, Bool.llvm_type, "tmp")

            if not types_equal(llvm.TypeOf(v), return_type):
                raise TypeError("Unexpected return value type")

            llvm.BuildRet(self.builder, v)

    def visit_BinOp(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.pop()
        lhs = self.pop()

        v = emit_binary_op(self.builder, node.op, lhs, rhs)
        self.push(v)

    def visit_BoolOp(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.pop()
        lhs = self.pop()

        v = BOOL_INST[type(node.op)](self.builder, lhs, rhs, "tmp")
        self.push(v)

    def visit_Compare(self, node):
        from .types import COMPARE_INST, type_key, types_equal

        if len(node.ops) > 1 or len(node.comparators) > 1:
            raise NotImplementedError("Only simple `if` expressions are supported")

        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.pop()
        lhs = self.pop()
        op = node.ops[0]

        ty = llvm.TypeOf(lhs)
        if not types_equal(ty, llvm.TypeOf(rhs)):
            raise TypeError("Conflicting operand types for {0}: {1} and {2}"
                            .format(op, lhs, rhs))

        inst, ops = COMPARE_INST[type_key(ty)]
        v = inst(self.builder, ops[type(op)], lhs, rhs, "tmp")

        self.push(v)

    def visit_IfExp(self, node):
        from .types import types_equal

        self.visit(node.test)
        test_expr = truncate_bool(self.builder, self.pop())

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        if_branch_bb = llvm.AppendBasicBlock(func, "if")
        else_branch_bb = llvm.AppendBasicBlock(func, "else")
        merge_bb = llvm.AppendBasicBlock(func, "merge")

        llvm.BuildCondBr(self.builder, test_expr, if_branch_bb, else_branch_bb)

        llvm.PositionBuilderAtEnd(self.builder, if_branch_bb)
        self.visit(node.body)
        if_expr = self.pop()
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        if_branch_bb = llvm.GetInsertBlock(self.builder)

        llvm.PositionBuilderAtEnd(self.builder, else_branch_bb)
        self.visit(node.orelse)
        else_expr = self.pop()
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        else_branch_bb = llvm.GetInsertBlock(self.builder)

        expr_type = llvm.TypeOf(if_expr)
        if not types_equal(expr_type, llvm.TypeOf(else_expr)):
            raise TypeError("`if` expression clause return types don't match")

        llvm.PositionBuilderAtEnd(self.builder, merge_bb)
        phi = llvm.BuildPhi(self.builder, expr_type, "phi")
        llvm.AddIncoming(phi, ctypes.byref(if_expr), ctypes.byref(if_branch_bb), 1)
        llvm.AddIncoming(phi, ctypes.byref(else_expr), ctypes.byref(else_branch_bb), 1)

        self.push(phi)

    def visit_If(self, node):
        self.visit(node.test)
        test_expr = truncate_bool(self.builder, self.pop())

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        if_branch_bb = llvm.AppendBasicBlock(func, "if")
        else_branch_bb = llvm.AppendBasicBlock(func, "else")
        merge_bb = llvm.AppendBasicBlock(func, "merge")

        merged_if = False
        merged_else = False

        llvm.BuildCondBr(self.builder, test_expr, if_branch_bb, else_branch_bb)

        llvm.PositionBuilderAtEnd(self.builder, if_branch_bb)

        with self.locals.scope():
            for b in node.body:
                self.visit(b)

        # Branching to merge bock only if the clause block hasn't terminated yet.
        if not llvm.IsATerminatorInst(llvm.GetLastInstruction(if_branch_bb)):
            llvm.BuildBr(self.builder, merge_bb)
            merged_if = True

        llvm.PositionBuilderAtEnd(self.builder, else_branch_bb)

        with self.locals.scope():
            for b in node.orelse:
                self.visit(b)

        if not llvm.IsATerminatorInst(llvm.GetLastInstruction(else_branch_bb)):
            llvm.BuildBr(self.builder, merge_bb)
            merged_else = True

        # If neither of if/else merged, it means they both returned;
        # At this point there shouldn't be any more instructions afterwards
        # in the current indent block, and we don't have to reposition the builder.
        if merged_if or merged_else:
            llvm.PositionBuilderAtEnd(self.builder, merge_bb)
        else:
            llvm.DeleteBasicBlock(merge_bb)

    def visit_For(self, node):
        """for/else loop block.

        IR is structured as following:

            start_bb:
                - get range start/end/step
                - set up loop counter
            test_bb:
                - if loop counter < end, goto body_bb; else goto exit_bb
            body_bb:
                - with new local scope
                    - traverse nested AST
            step_bb:
                - increment loop counter
                - goto test_bb

            exit_bb:
                - end loop IR

        Every loop pushes test and step blocks onto `loop_info` stack
        so that nested blocks can resolve break/continue statements.

        """
        # FIXME hardcoded Long type for loop variable
        from .types import Long

        if len(node.orelse) != 0:
            raise NotImplementedError("`else` in a `for` statement is not supported")

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))

        top_bb = llvm.GetInsertBlock(self.builder)

        start_bb = llvm.AppendBasicBlock(func, "loop_start")
        test_bb = llvm.AppendBasicBlock(func, "loop_test")
        body_bb = llvm.AppendBasicBlock(func, "loop_body")
        step_bb = llvm.AppendBasicBlock(func, "loop_step")
        exit_bb = llvm.AppendBasicBlock(func, "loop_exit")

        # Reorder blocks for better flowing IR listing; not strictly necessary
        llvm.MoveBasicBlockAfter(start_bb, top_bb)
        llvm.MoveBasicBlockAfter(test_bb, start_bb)
        llvm.MoveBasicBlockAfter(body_bb, test_bb)
        llvm.MoveBasicBlockAfter(step_bb, body_bb)
        llvm.MoveBasicBlockAfter(exit_bb, step_bb)

        llvm.BuildBr(self.builder, start_bb)

        # Loop header; get loop variable, iteration limits.
        llvm.PositionBuilderAtEnd(self.builder, start_bb)

        self.visit(node.target)
        target = self.pop()

        self.visit(node.iter)
        start, stop, step = self.pop()

        # Loop counter
        i_ptr = self.store(target, start)
        llvm.BuildBr(self.builder, test_bb)

        # Loop test
        llvm.PositionBuilderAtEnd(self.builder, test_bb)
        i = llvm.BuildLoad(self.builder, i_ptr, "loop_i")
        t = llvm.BuildICmp(self.builder, llvm.IntSLT, i, stop, "loop_{0}_cmp".format(target))
        llvm.BuildCondBr(self.builder, t, body_bb, exit_bb)

        llvm.PositionBuilderAtEnd(self.builder, body_bb)

        # Posting entrance and exit blocks (for continue/break respectively)
        self.loop_info.append((step_bb, exit_bb))

        with self.locals.scope():
            for b in node.body:
                self.visit(b)

        self.loop_info.pop()
        llvm.BuildBr(self.builder, step_bb)

        # Loop step; incrementing counter and going back to test_bb
        llvm.PositionBuilderAtEnd(self.builder, step_bb)
        i_next = llvm.BuildAdd(self.builder, i, step, "loop_{0}_next".format(target))
        llvm.BuildStore(self.builder, i_next, i_ptr)
        llvm.BuildBr(self.builder, test_bb)

        # Loop exit; decrement the counter for consistency with python interpreter.
        llvm.PositionBuilderAtEnd(self.builder, exit_bb)
        one = llvm.ConstInt(Long.llvm_type, 1, True)
        i_final = llvm.BuildSub(self.builder, i, one, "loop_{0}_final".format(target))
        llvm.BuildStore(self.builder, i_final, i_ptr)

    def visit_Continue(self, node):
        step_bb, _ = self.loop_info[-1]
        llvm.BuildBr(self.builder, step_bb)

    def visit_Break(self, node):
        _, exit_bb = self.loop_info[-1]
        llvm.BuildBr(self.builder, exit_bb)

    def visit_Call(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        args = [self.pop() for _ in range(len(node.args))][::-1]
        func = self.pop()

        if hasattr(func, "__n2o_func__"):
            # Function is compiled; check arguments for validity (unless
            # it's a definition for an external function) and make a direct call
            if not isinstance(func, ExternalFunction):
                _validate_function_args(func, args)
            result = llvm.BuildCall(self.builder, func.__n2o_func__,
                                    (llvm.ValueRef * len(args))(*args),
                                    len(args), "")
            if func.__n2o_restype__ is not None:
                llvm.SetValueName(result, "v")
        else:
            # Function is either CPython one or an LLVM emitter.
            result = func(*args)
            if getattr(result, "__n2o_emitter__", False):
                result = result(self.module, self.builder)

        self.push(result)


def emit_body(module, builder, func):
    """Emits function body IR.

    Expects function already is declared and referenced as func.__n2o_func__.

    """
    from .exceptions import TranslationError
    from .util import remove_indent
    from inspect import getsourcelines

    # ast.parse returns us a module, first function there is what we're parsing.
    func_source = remove_indent(getsourcelines(func))
    func_body = ast.parse(func_source).body[0].body

    # Emit function body IR
    v = Visitor(module, builder, dict(resolve_constants(func.__n2o_globals__)))
    llvm.PositionBuilderAtEnd(builder, llvm.AppendBasicBlock(func.__n2o_func__, "entry"))

    # Store function parameters as locals
    for i, name in enumerate(func.__n2o_args__):
        param = llvm.GetParam(func.__n2o_func__, i)
        llvm.SetValueName(param, name)
        v.store(name, param, func.__n2o_argtypes__[name])

    try:
        for node in func_body:
            v.visit(node)

    except TranslationError, e:
        raise _unpack_translation_error(func.func_name, func_source, e.args)

    last_block = llvm.GetInsertBlock(builder)
    if not llvm.IsATerminatorInst(llvm.GetLastInstruction(last_block)):
        # Last return out of a void function can be implicit.
        restype = llvm.function_return_type(func.__n2o_func__)
        if llvm.GetTypeKind(restype) == llvm.VoidTypeKind:
            llvm.BuildRetVoid(builder)
        else:
            # Point to the last function line where the return statement should be.
            e_args = (TypeError, func_body[-1].lineno, "Function must return a value")
            raise _unpack_translation_error(func.func_name, func_source, e_args)


def entry_alloca(func, type_, name):
    """Reserves stack space for a variable at function entry point."""
    entry = llvm.GetEntryBasicBlock(func)
    builder = llvm.CreateBuilder()
    llvm.PositionBuilder(builder, entry, llvm.GetFirstInstruction(entry))
    a = llvm.BuildAlloca(builder, type_, name)
    llvm.DisposeBuilder(builder)
    return a


def emit_constant(value):
    """Emit constant IR for known value types."""
    from .types import Bool, Long, Double

    if isinstance(value, float):
        return llvm.ConstReal(Double.llvm_type, value)
    elif isinstance(value, bool):
        # Check bool before integer since bool is also an int
        return llvm.ConstInt(Bool.llvm_type, value, True)
    elif isinstance(value, int):
        return llvm.ConstInt(Long.llvm_type, value, True)
    else:
        raise TypeError("Unknown Number type {0!s}".format(type(value)))


def resolve_constants(symbols):
    """Converts eligible values in a dictionary to LLVM constant objects."""
    for k, v in symbols.iteritems():
        try:
            yield k, emit_constant(v)
        except TypeError:
            # Not a constant, something else will handle this.
            yield k, v


def emit_binary_op(builder, op, lhs, rhs):
    from .types import BINARY_INST, type_key, types_equal

    ty = llvm.TypeOf(lhs)
    if not types_equal(ty, llvm.TypeOf(rhs)):
        raise TypeError("Conflicting operand types for {0}: {1} and {2}"
                        .format(op, lhs, rhs))

    return BINARY_INST[type_key(ty)][type(op)](
        builder, lhs, rhs, type(op).__name__.lower()
    )

def truncate_bool(builder, v):
    """Truncate Bool value for use in conditional comparison"""
    t = llvm.TypeOf(v)
    if llvm.GetTypeKind(t) == llvm.IntegerTypeKind:
        width = llvm.GetIntTypeWidth(t)
        if width == 8:
            return llvm.BuildCast(builder, llvm.Trunc, v, llvm.IntType(1), "v")
        elif width == 1:
            return v

    raise TypeError("Not a boolean variable")


def _validate_function_args(func, args):
    """Raises TypeError if if *args* do not match annotated function signature."""
    from .types import types_equal
    import inspect

    if len(args) != len(func.__n2o_argtypes__):
        raise TypeError("{0}() takes exactly {1} argument(s) ({2} given)"
                        .format(func.func_name, len(func.__n2o_argtypes__), len(args)))

    spec = inspect.getargspec(func)
    mask = map(types_equal,
               (func.__n2o_argtypes__[name].llvm_type for name in spec.args),
               (llvm.TypeOf(val) for val in args))

    if not all(mask):
        wrong_args = ", ".join((a for a, ok in zip(spec.args, mask) if not ok))
        raise TypeError("{0}() called with wrong argument type(s) for {1}"
                        .format(func.func_name, wrong_args))


def _unpack_translation_error(func_name, func_lines, args, before=2, after=5):
    """Unpacks TranslationError *args* data and reconstructs the contained exception.

    Translation errors are used to attach source localtion (line number / column offset)
    and shuttle them out of AST traversal where they can be formatted and rethrown.

    :param args: TranslationError.args with (error type, line number and error message)
    :param before: number of lines to show before the offending one.
    :param after: number of lines to show after the offending one.

    """
    from itertools import chain

    def draw_arrow(snippet, line_index):
        for i, line in enumerate(snippet):
            prefix = "  >>> " if i == line_index else "      "
            yield prefix + line

    error_type, line_number, message = args
    line_i = line_number - 1

    snippet = func_lines.split("\n")[min(0, line_i - before): line_i + after]
    tb = draw_arrow(snippet, line_i)

    return error_type("\n".join(chain((message, "  Traceback:"), tb)))
