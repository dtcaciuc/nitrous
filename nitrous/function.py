import ast
import ctypes

from contextlib import contextmanager
from . import llvm


_BOOL_INST = {
    ast.And: llvm.BuildAnd,
    ast.Or: llvm.BuildOr
}

_INTEGER_COMPARE_INST = llvm.BuildICmp, {
    ast.Eq: llvm.IntEQ,
    ast.Gt: llvm.IntSGT,
    ast.GtE: llvm.IntSGE,
    ast.Lt: llvm.IntSLT,
    ast.LtE: llvm.IntSLE,
    ast.NotEq: llvm.IntNE
}

_FLOATING_COMPARE_INST = llvm.BuildFCmp, {
    ast.Eq: llvm.RealUEQ,
    ast.Gt: llvm.RealUGT,
    ast.GtE: llvm.RealUGE,
    ast.Lt: llvm.RealULT,
    ast.LtE: llvm.RealULE,
    ast.NotEq: llvm.RealUNE,
}

_INTEGER_BINARY_INST = {
    ast.Add: llvm.BuildAdd,
    ast.Sub: llvm.BuildSub,
    ast.Mult: llvm.BuildMul,
    ast.BitAnd: llvm.BuildAnd,
    ast.BitOr: llvm.BuildOr,
    ast.BitXor: llvm.BuildXor,
    ast.Mod: llvm.build_smod,
    ast.Pow: llvm.build_pow,
    # Integer division is consciously left out and
    # handled in function.py/emit_binary_op
}

_FLOATING_BINARY_INST = {
    ast.Add: llvm.BuildFAdd,
    ast.Sub: llvm.BuildFSub,
    ast.Mult: llvm.BuildFMul,
    ast.Div: llvm.BuildFDiv,
    ast.Mod: llvm.build_fmod,
    ast.Pow: llvm.build_pow,
}

_FLOATING_UNARY_INST = {
    ast.USub: llvm.BuildFNeg
}

_INTEGRAL_UNARY_INST = {
    ast.USub: llvm.BuildNeg,
    ast.Invert: llvm.BuildNot
}


class FunctionDecl(object):
    """Result of annotating a function with ``function()`` decorator.

    Used to create a :class:`Function` instance first time declaration is used
    in a module.

    """

    def __init__(self, restype, argtypes, args, pyfunc):
        self.restype = restype
        self.argtypes = argtypes
        # TODO Replace this with argtypes ordered dictionary?
        # For consistency, same in ExternalFunction as well.
        self.args = args
        self.pyfunc = pyfunc

        # Options
        self.options = {'cdiv': False, 'inline': False}
        self.globals = {}

        # Gets populated by functools.wraps
        self.__name__ = None


class Function(object):
    """Used for representing a function instance in a module and (optionally)
    to interface the function to Python code through ctypes.

    Before each function is returned as a :class:`Module` instance attribute,
    its ``cfunc`` attribute is populated through either ``wrap_so`` or ``wrap_engine``
    methods (depending on the type of module backend used).

    """

    def __init__(self, decl, llvm_func):
        self.decl = decl
        self.llvm_func = llvm_func
        # Copy globals, since these depend on individual function environment.
        self.globals = decl.globals.copy()
        self.__name__ = decl.__name__

        self.converters = [
            t.convert if hasattr(t, "convert") else lambda a: a
            for t in [self.decl.argtypes[arg] for arg in self.decl.args]
        ]

        self.cfunc = None

    @property
    def _c_restype(self):
        return self.decl.restype.c_type if self.decl.restype is not None else None

    @property
    def _c_argtypes(self):
        return [self.decl.argtypes[arg].c_type for arg in self.decl.args]

    def wrap_so(self, so):
        """Populates ctypes function object from a loaded SO file."""
        self.cfunc = getattr(so, llvm.GetValueName(self.llvm_func))
        self.cfunc.argtypes = self._c_argtypes
        self.cfunc.restype = self._c_restype

    def wrap_engine(self, engine):
        """Populates ctypes function object from an execution engine."""
        proto = ctypes.CFUNCTYPE(self._c_restype, *self._c_argtypes)
        self.cfunc = proto(llvm.GetPointerToGlobal(engine, self.llvm_func))

    def __call__(self, *args):
        return self.cfunc(*(c(a) for c, a in zip(self.converters, args)))


def function(restype=None, **kwargs):
    """Decorate an existing function with signature type annotations.

    *restype* is the function return type. *kwargs* key/value pairs map argument
    names to their respective types.

    """
    def wrapper(pyfunc):
        from .exceptions import AnnotationError
        from .types import Reference, is_aggregate
        from .lib import range_
        import functools
        import inspect

        # Types can provide a susbtitution if they're used directly
        # as an argument type (eg. Structure needs to be implicitly
        # passed as Reference() to said structure.
        argtypes = dict(
            (k, Reference(t) if is_aggregate(t) else t)
            for k, t in kwargs.items()
        )

        # - Ordered argument name sequence
        spec = inspect.getargspec(pyfunc)
        if spec.varargs or spec.keywords:
            raise AnnotationError("Variable and/or keyword arguments are not allowed")
        if set(spec.args) != set(argtypes):
            raise AnnotationError("Argument type annotations don't match function arguments.")

        decl = functools.wraps(pyfunc)(FunctionDecl(restype, argtypes, spec.args, pyfunc))

        # Immutable global symbols.
        # - Built-ins
        decl.globals["range"] = range_
        decl.globals["True"] = True
        decl.globals["False"] = False
        # - Other symbols available at the point of function
        #   definition; try to resolve as many constants as possible.
        parent_frame = inspect.currentframe().f_back
        decl.globals.update(parent_frame.f_globals)
        decl.globals.update(parent_frame.f_locals)
        del parent_frame

        return decl

    return wrapper


def options(cdiv=False, inline=False):
    """Set behavioural options which affect the generated code.

    :param cdiv: Set ``True`` to match C behaviour when performing integer division.
    :param inline: Set ``True`` to always inline the function.

    """
    def wrapper(decl):
        decl.options.update(cdiv=cdiv, inline=inline)
        return decl
    return wrapper


def c_function(name, restype, argtypes):
    """Declares a external C function.

    :param name: function name as it's listed in library symbols.
    :param restype: function return value type
    :param argtypes: sequence of function argument types

    """
    # Since argtypes is a list, `args` will be integers to act as keys
    decl = FunctionDecl(restype, argtypes, range(len(argtypes)), pyfunc=None)
    decl.__name__ = name
    return decl


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


class FunctionBuilder(ast.NodeVisitor):

    def __init__(self, builder, opts):
        self.builder = builder
        self.opts = opts

        # Map of value names to their nitrous types. Currently,
        # this is important only for arrays of aggregate values
        # to facilitate correct reference mechanics.
        self.types = {}

        # Global immutable symbols
        self.globals = {}
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

        # List of compiled functions that has been used
        # but were not yet declared.
        self.new_funcs = []

    def store(self, name, value, type_=None):
        """Stores *value* on the stack under *name*.

        Allocates stack space if *name* is not an existing variable. Returns
        the pointer to allocated space.

        """
        try:
            addr = self.locals[name]
        except KeyError:
            # First time storing the variable; allocate stack space
            # and register with most nested scope.
            addr = entry_alloca(self.builder, llvm.TypeOf(value), "v")
            self.locals[name] = addr
            # Register explicitly stated type or propagate existing value type.
            self.types[name] = type_ or self.typeof(value)

        # Make sure storage and value LLVM types match.
        addr_ty = llvm.GetElementType(llvm.TypeOf(addr))
        if not llvm.types_equal(addr_ty, llvm.TypeOf(value)):
            raise TypeError("Cannot assign {0} to a {1}".format(
                self.typeof(value), self.typeof(name)
            ))

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
            k = llvm.address_of(value)
            if k in self.types and type_.tag != self.types[k].tag:
                raise RuntimeError("Value is already registered with a different type.")
            self.types[k] = type_

    def pop(self):
        """Pops top value from expression value stack."""
        return self.stack.pop()

    def typeof(self, v):
        """Returns nitrous type for value *v*.

        Returns None if *v* has no known type association. *v* can be
        either an LLVM value or value name.

        """
        # Global constants and values pushed on stack and
        # keyed by their address; everything else (eg. stores
        # and python objects) is keyed by their name.
        name = llvm.address_of(v) if isinstance(v, llvm.ValueRef) else v
        return self.types.get(name, None)

    def visit(self, node):
        from .exceptions import TranslationError

        try:
            self.node_stack.append(node)
            super(FunctionBuilder, self).visit(node)
        except (NameError, ValueError, TypeError, NotImplementedError), e:
            # Use translation error tag the exception with line number and
            # carry it upwards to translation routine where the traceback is reported.
            lineno = next(n for n in self.node_stack[::-1] if hasattr(n, "lineno")).lineno
            raise TranslationError(type(e), lineno, e.args[0])
        finally:
            self.node_stack.pop()

    def r_visit(self, node):
        """Visits given node; pops and returns the last value."""
        self.visit(node)
        v = self.pop()
        return v

    def visit_Num(self, node):
        self.push(*emit_constant(self.builder, node.n))

    def visit_Str(self, node):
        from .types import String
        self.push(emit_constant_string(self.builder, node.s), String)

    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):
            self.push(self.load(node.id), self.typeof(node.id))
        elif isinstance(node.ctx, ast.Store):
            self.store(node.id, self.pop())
        else:
            raise NotImplementedError("Unknown Name context {0!s}".format(type(node.ctx)))

    def visit_Attribute(self, node):
        from .types import Reference

        v = self.r_visit(node.value)
        vt = self.typeof(v)

        if isinstance(vt, Reference):
            # References to structures
            vt = vt.value_type

        if hasattr(vt, "emit_getattr"):
            if isinstance(node.ctx, ast.Load):
                self.push(*vt.emit_getattr(self.builder, v, node.attr))
            elif isinstance(node.ctx, ast.Store):
                vt.emit_setattr(self.builder, v, node.attr, self.pop())
            else:
                raise NotImplementedError("Unsupported attribute context {0}".format(node.ctx))
        else:
            # Assume regular python object
            if isinstance(node.ctx, ast.Load):
                self.push(*try_emit_constant(self.builder, getattr(v, node.attr)))
            else:
                raise NotImplementedError("Unsupported attribute context {0}".format(node.ctx))

    def visit_Tuple(self, node):

        if isinstance(node.ctx, ast.Load):
            self.push(tuple(self.r_visit(e) for e in node.elts))

        elif isinstance(node.ctx, ast.Store):
            rhs = self.pop()
            try:
                rhs_len = len(rhs)
            except TypeError:
                raise TypeError("Value of type '{0}' is not an iterable".format(self.typeof(rhs)))

            if len(node.elts) != rhs_len:
                raise ValueError("Cannot unpack {0} values into {1}".format(rhs_len, len(node.elts)))

            for e_node, v in zip(node.elts, rhs):
                self.push(v)
                # Node in store context will expect value on stack.
                self.visit(e_node)

        else:
            raise NotImplementedError("Unsupported tuple context {0}".format(node.ctx))

    def visit_Index(self, node):
        if isinstance(node.value, ast.Tuple):
            # Tuple visitor will push the value onto the stack itself
            self.visit(node.value)
        else:
            # Assume everything else will produce a single index value.
            self.push((self.r_visit(node.value),))

    def visit_Subscript(self, node):
        """Label subscript of form `var_expr[index_expr]`.

        If in Load() context, return the value; if in Store()
        context, return the prepared GEP; parent node which knows
        about the source data will complete the instruction.

        """
        from .types import Reference

        v = self.r_visit(node.value)
        vt = self.typeof(v)

        if isinstance(vt, Reference):
            vt = vt.value_type

        # Index is a nd tuple
        i = self.r_visit(node.slice)

        # TODO catch attribute error; raise type error
        # indicating that this type does not support indexing.
        if isinstance(node.ctx, ast.Load):
            self.push(*vt.emit_getitem(self.builder, v, i))
        elif isinstance(node.ctx, ast.Store):
            vt.emit_setitem(self.builder, v, i, self.pop())
        else:
            raise NotImplementedError("Unsupported subscript context {0}".format(node.ctx))

    def visit_Slice(self, node):
        raise NotImplementedError("Slices are not supported")

    def visit_Delete(self, node):
        raise NotImplementedError("`del`etions are not supported")

    def visit_Assign(self, node):
        if len(node.targets) > 1:
            raise NotImplementedError("Chained assignment is not supported")

        # Get value; keep it on the stack and visit target.
        # visit_{Name, Subscript, Attribute, Tuple} in store context will expect it there.
        self.visit(node.value)
        self.visit(node.targets[0])

    def visit_AugAssign(self, node):
        raise RuntimeError("Encountered unexpected augmented assignment")

    def visit_Return(self, node):
        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        return_type = llvm.function_return_type(func)

        if llvm.GetTypeKind(return_type) == llvm.VoidTypeKind:
            if node.value is not None:
                raise ValueError("No return value expected")

            llvm.BuildRetVoid(self.builder)

        else:
            v = self.r_visit(node.value)
            if not llvm.types_equal(llvm.TypeOf(v), return_type):
                raise TypeError("Unexpected return value type")

            llvm.BuildRet(self.builder, v)

    def visit_UnaryOp(self, node):
        rhs = self.r_visit(node.operand)

        kind = llvm.GetTypeKind(llvm.TypeOf(rhs))
        op_type = type(node.op)

        if op_type == ast.Not:
            inst = self._emit_bool_not
        elif kind == llvm.IntegerTypeKind:
            inst = _INTEGRAL_UNARY_INST[op_type]
        elif kind in (llvm.FloatTypeKind, llvm.DoubleTypeKind):
            inst = _FLOATING_UNARY_INST[op_type]
        else:
            raise TypeError("Unsupported operand type for {0}: {1}"
                            .format(node.op, self.typeof(rhs)))

        v = inst(self.builder, rhs, op_type.__name__.lower())
        self.push(v, self.typeof(rhs))

    def visit_BinOp(self, node):
        lhs = self.r_visit(node.left)
        rhs = self.r_visit(node.right)
        op = node.op

        ty = llvm.TypeOf(lhs)
        if not llvm.types_equal(ty, llvm.TypeOf(rhs)):
            raise TypeError("Conflicting operand types for {0}: {1} and {2}"
                            .format(op, self.typeof(lhs), self.typeof(rhs)))

        # Vectors use same ops as their element types.
        if llvm.GetTypeKind(ty) == llvm.VectorTypeKind:
            ty = llvm.GetElementType(ty)

        kind = llvm.GetTypeKind(ty)

        if kind == llvm.IntegerTypeKind:
            div = llvm.BuildSDiv if self.opts["cdiv"] else llvm.build_py_idiv
            inst = div if isinstance(op, ast.Div) else _INTEGER_BINARY_INST[type(op)]
        elif kind in (llvm.FloatTypeKind, llvm.DoubleTypeKind):
            inst = _FLOATING_BINARY_INST[type(op)]
        else:
            raise TypeError("Unsupported operand types for {0}: {1} and {2}"
                            .format(op, self.typeof(lhs), self.typeof(rhs)))

        v = inst(self.builder, lhs, rhs, type(op).__name__.lower())
        # Assuming binary operations return values of the same type as operands.
        self.push(v, self.typeof(lhs))

    def visit_BoolOp(self, node):
        from .types import Bool

        rhs = self._truncate_bool(self.r_visit(node.values[0]))
        # Expressions like `a > 1 or b > 1 or c > 1` collapse into one `or` with 3 .values
        for v in node.values[1:]:
            lhs = self._truncate_bool(self.r_visit(v))
            rhs = _BOOL_INST[type(node.op)](self.builder, lhs, rhs, "cmp")

        self.push(_extend_bool(self.builder, rhs), Bool)

    def visit_Compare(self, node):
        from .types import Bool

        if len(node.ops) > 1 or len(node.comparators) > 1:
            raise NotImplementedError("Only simple `if` expressions are supported")

        lhs = self.r_visit(node.left)
        rhs = self.r_visit(node.comparators[0])
        op = node.ops[0]

        ty = llvm.TypeOf(lhs)
        if not llvm.types_equal(ty, llvm.TypeOf(rhs)):
            raise TypeError("Conflicting operand types for {0}: {1} and {2}"
                            .format(op, self.typeof(lhs), self.typeof(rhs)))

        # Vectors use same ops as their element types.
        if llvm.GetTypeKind(ty) == llvm.VectorTypeKind:
            ty = llvm.GetElementType(ty)

        kind = llvm.GetTypeKind(ty)
        if kind in (llvm.IntegerTypeKind, llvm.PointerTypeKind):
            inst, ops = _INTEGER_COMPARE_INST
        elif kind in (llvm.FloatTypeKind, llvm.DoubleTypeKind):
            inst, ops = _FLOATING_COMPARE_INST
        else:
            raise TypeError("Cannot compare {0} with {1}"
                            .format(self.typeof(lhs), self.typeof(rhs)))

        v = inst(self.builder, ops[type(op)], lhs, rhs, "cmp")
        self.push(_extend_bool(self.builder, v), Bool)

    def visit_IfExp(self, node):
        test_expr = self._truncate_bool(self.r_visit(node.test))

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        if_branch_bb = llvm.AppendBasicBlock(func, "if")
        else_branch_bb = llvm.AppendBasicBlock(func, "else")
        merge_bb = llvm.AppendBasicBlock(func, "merge")

        llvm.BuildCondBr(self.builder, test_expr, if_branch_bb, else_branch_bb)

        llvm.PositionBuilderAtEnd(self.builder, if_branch_bb)
        if_expr = self.r_visit(node.body)
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        if_branch_bb = llvm.GetInsertBlock(self.builder)

        llvm.PositionBuilderAtEnd(self.builder, else_branch_bb)
        else_expr = self.r_visit(node.orelse)
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        else_branch_bb = llvm.GetInsertBlock(self.builder)

        expr_type = llvm.TypeOf(if_expr)
        if not llvm.types_equal(expr_type, llvm.TypeOf(else_expr)):
            raise TypeError("`if` expression clause return types don't match")

        llvm.PositionBuilderAtEnd(self.builder, merge_bb)
        phi = llvm.BuildPhi(self.builder, expr_type, "phi")
        llvm.AddIncoming(phi, ctypes.byref(if_expr), ctypes.byref(if_branch_bb), 1)
        llvm.AddIncoming(phi, ctypes.byref(else_expr), ctypes.byref(else_branch_bb), 1)

        self.push(phi)

    def visit_If(self, node):
        test_expr = self._truncate_bool(self.r_visit(node.test))

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
        if not llvm.IsATerminatorInst(llvm.GetLastInstruction(llvm.GetInsertBlock(self.builder))):
            llvm.BuildBr(self.builder, merge_bb)
            merged_if = True

        llvm.PositionBuilderAtEnd(self.builder, else_branch_bb)

        with self.locals.scope():
            for b in node.orelse:
                self.visit(b)

        if not llvm.IsATerminatorInst(llvm.GetLastInstruction(llvm.GetInsertBlock(self.builder))):
            llvm.BuildBr(self.builder, merge_bb)
            merged_else = True

        # If neither of if/else merged, it means they both returned;
        # At this point there shouldn't be any more instructions afterwards
        # in the current indent block, and we don't have to reposition the builder.
        if merged_if or merged_else:
            llvm.PositionBuilderAtEnd(self.builder, merge_bb)
        else:
            llvm.DeleteBasicBlock(merge_bb)

    def visit_While(self, node):
        """while loop block.

        Component blocks:

            test_bb:
                - if loop counter < end, goto body_bb; else goto exit_bb
            body_bb:
                - with new local scope
                    - traverse nested AST
            exit_bb:
                - end loop IR

        """

        if len(node.orelse) != 0:
            raise NotImplementedError("`else` in a `while` statement is not supported")

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))

        top_bb = llvm.GetInsertBlock(self.builder)

        test_bb = llvm.AppendBasicBlock(func, "while_test")
        body_bb = llvm.AppendBasicBlock(func, "while_body")
        exit_bb = llvm.AppendBasicBlock(func, "while_exit")

        llvm.MoveBasicBlockAfter(test_bb, top_bb)
        llvm.MoveBasicBlockAfter(body_bb, test_bb)
        llvm.MoveBasicBlockAfter(exit_bb, body_bb)

        llvm.BuildBr(self.builder, test_bb)
        llvm.PositionBuilderAtEnd(self.builder, test_bb)

        test = self._truncate_bool(self.r_visit(node.test))
        llvm.BuildCondBr(self.builder, test, body_bb, exit_bb)

        llvm.PositionBuilderAtEnd(self.builder, body_bb)

        # Posting entrance and exit blocks (for continue/break respectively)
        self.loop_info.append((test_bb, exit_bb))

        with self.locals.scope():
            for b in node.body:
                self.visit(b)

        self.loop_info.pop()
        llvm.BuildBr(self.builder, test_bb)

        llvm.PositionBuilderAtEnd(self.builder, exit_bb)

    def visit_For(self, node):
        """for/else loop block.

        Component blocks:

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
        from .types import const_index

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

        start, stop, step = self.r_visit(node.iter)

        # Loop counter
        if not isinstance(node.target, ast.Name):
            raise TypeError("Unsupported loop variable type: {0}".format(type(node.target)))

        target = node.target.id

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
        i_final = llvm.BuildSub(self.builder, i, const_index(1), "loop_{0}_final".format(target))
        llvm.BuildStore(self.builder, i_final, i_ptr)

    def visit_Continue(self, node):
        step_bb, _ = self.loop_info[-1]
        llvm.BuildBr(self.builder, step_bb)

    def visit_Break(self, node):
        _, exit_bb = self.loop_info[-1]
        llvm.BuildBr(self.builder, exit_bb)

    def visit_Call(self, node):
        func = self.r_visit(node.func)
        args = [self.r_visit(a) for a in node.args]
        result_type = None

        if isinstance(func, FunctionDecl):
            _validate_function_args(func, args)
            llvm_func = self._get_or_create_function(func)
            result = llvm.BuildCall(self.builder, llvm_func,
                                    (llvm.ValueRef * len(args))(*args),
                                    len(args), "")
            if func.restype is not None:
                result_type = func.restype
                llvm.SetValueName(result, "v")
        else:
            # Function is either CPython one or an LLVM emitter.
            result = func(*args)
            if getattr(result, "__n2o_emitter__", False):
                result, result_type = result(self.builder)

        self.push(result, result_type)

    def visit_Print(self, node):
        from .lib import print_
        dest = self.r_visit(node.dest)
        end = emit_constant_string(self.builder, "\n" if node.nl else "")
        print_(*map(self.r_visit, node.values), end=end, file=dest)(self.builder)

    def _get_or_create_function(self, decl):
        """Returns LLVM function value for given declaration;
        creates one if it doesn't exist yet.

        """
        module = llvm.GetParentModule__(self.builder)
        llvm_func, exists = _get_or_create_function(module, decl)

        if not exists:
            self.new_funcs.append(Function(decl, llvm_func))

        return llvm_func

    def _truncate_bool(self, v):
        """Truncates boolean carrying byte to 1-bit integer for use in conditionals."""
        from .types import Bool

        if self.typeof(v).tag != Bool.tag:
            raise TypeError("Must be a boolean expression")

        return llvm.BuildCast(self.builder, llvm.Trunc, v, llvm.IntType(1), "b")

    def _emit_bool_not(self, _, v, name):
        """Emits "not" operation for 8-bit boolean value."""
        # FIXME empty argument is for builder, but we have that through `self` already.
        nv = llvm.BuildNot(self.builder, self._truncate_bool(v), name)
        return _extend_bool(self.builder, nv)


class UnpackAugAssign(ast.NodeTransformer):
    """Replaces augmented assignments with non-augmented ones.

    Eg., ``x[i] += y`` to ``x[i] = x[i] + y``.

    """

    def visit_AugAssign(self, node):
        from ast import BinOp, copy_location
        from copy import copy

        load_target = copy(node.target)
        load_target.ctx = ast.Load()

        op_node = BinOp(left=load_target, right=node.value, op=node.op)
        assign = ast.Assign(targets=[node.target], value=copy_location(op_node, node))

        return copy_location(assign, node)


def emit_body(builder, func):
    """Emits function body IR.

    Expects function already is declared and referenced as func.llvm_func.

    """
    from .exceptions import TranslationError
    from inspect import getsourcelines
    from textwrap import dedent

    # ast.parse returns us a module, first function there is what we're parsing.
    lines, _ = getsourcelines(func.decl.pyfunc)
    func_source = dedent("".join(lines))
    func_body = ast.parse(func_source).body[0].body

    # Emit function body IR
    b = FunctionBuilder(builder, func.decl.options)

    entry_bb = llvm.AppendBasicBlock(func.llvm_func, "entry")
    llvm.PositionBuilderAtEnd(builder, entry_bb)

    # Populate global symbols
    for k, v in func.globals.iteritems():
        v, t = try_emit_constant(b.builder, v)
        b.globals[k] = v
        b.types[k] = t

    # Store function parameters as locals
    for i, name in enumerate(func.decl.args):
        param = llvm.GetParam(func.llvm_func, i)
        llvm.SetValueName(param, name)
        b.store(name, param, func.decl.argtypes[name])

    try:
        for node in func_body:
            node = UnpackAugAssign().visit(node)
            b.visit(node)

    except TranslationError, e:
        raise _unpack_translation_error(func_source, e.args)

    last_block = llvm.GetInsertBlock(builder)
    if not llvm.IsATerminatorInst(llvm.GetLastInstruction(last_block)):
        # Last return out of a void function can be implicit.
        restype = llvm.function_return_type(func.llvm_func)
        if llvm.GetTypeKind(restype) == llvm.VoidTypeKind:
            llvm.BuildRetVoid(builder)
        else:
            # Point to the last function line where the return statement should be.
            e_args = (TypeError, func_body[-1].lineno, "Function must return a value")
            raise _unpack_translation_error(func_source, e_args)

    return b.new_funcs


def entry_alloca(builder, type_, name):
    """Reserves stack space for a variable at function entry point.

    Existing *builder* must point somewhere within the function.

    """
    b = _entry_builder(builder)
    a = llvm.BuildAlloca(b, type_, name)
    llvm.DisposeBuilder(b)
    return a


def entry_array_alloca(builder, element_type, n, name):
    """Reserves stack space for an array of size *n* at function entry point.

    Existing *builder* must point somewhere within the function.

    """
    b = _entry_builder(builder)
    a = llvm.BuildArrayAlloca(b, element_type, n, name)
    llvm.DisposeBuilder(b)
    return a


def _entry_builder(builder):
    """Returns new entry point builder based on another builder currently working on the function."""
    b = llvm.CreateBuilder()
    entry = llvm.GetEntryBasicBlock(llvm.GetBasicBlockParent(llvm.GetInsertBlock(builder)))
    llvm.PositionBuilder(b, entry, llvm.GetFirstInstruction(entry))
    return b


def _extend_bool(builder, v):
    """Extends 1-bit boolean to conventional one."""
    from .types import Bool

    return llvm.BuildCast(builder, llvm.ZExt, v, Bool.llvm_type, "b")


def try_emit_constant(builder, value):
    """Same as emit_constant but does not raise TypeError."""
    try:
        return emit_constant(builder, value)
    except TypeError:
        return value, None


def emit_constant(builder, value):
    """Emit constant IR for known value types."""
    from .types import Bool, Long, Double, String

    if isinstance(value, float):
        return llvm.ConstReal(Double.llvm_type, value), Double
    elif isinstance(value, bool):
        # Check bool before integer since bool is also an int
        return llvm.ConstInt(Bool.llvm_type, value, True), Bool
    elif isinstance(value, int):
        return llvm.ConstInt(Long.llvm_type, value, True), Long
    elif isinstance(value, basestring):
        return emit_constant_string(builder, value), String
    else:
        raise TypeError("Unknown Number type {0!s}".format(type(value)))


def emit_constant_string(builder, value):
    from .types import String

    module = llvm.GetParentModule__(builder)
    init = llvm.ConstString(value, len(value), False)

    s = llvm.AddGlobal(module, llvm.TypeOf(init), "")
    llvm.SetInitializer(s, init)
    llvm.SetGlobalConstant(s, llvm.TRUE)
    llvm.SetLinkage(s, llvm.PrivateLinkage)

    return llvm.BuildPointerCast(builder, s, String.llvm_type, "")


def _qualified_name(module, decl):
    """Returns qualified declaration name.

    The scheme used is:

        qualified-name: function-module-name "_" function-name "_" mangled-argtypes

    *mangled-argtypes* is a string which uniquely encodes the sequence of
    function argument types, similar to mangled C++ names. The name is unchanged
    if declaration has no associated Python function.

    """
    if not decl.pyfunc:
        # XXX hack; have a better way to determine whether
        # function name should be qualified or not.
        return decl.__name__

    suffix = "".join(decl.argtypes[arg].tag for arg in decl.args)
    return "_".join((llvm.GetModuleName(module),
                     decl.__module__,
                     decl.__name__,
                     suffix))


def _get_or_create_function(module, decl, vargs=False):
    """Gets or declares an an LLVM function based on its declaration.

    Set *vargs* to True of the function accepts variadic arguments.

    """
    name = _qualified_name(module, decl)
    llvm_func = llvm.GetNamedFunction(module, name)
    exists = bool(llvm_func)

    if not exists:

        argtypes = [decl.argtypes[arg] for arg in decl.args]
        restype = decl.restype

        llvm_restype = restype.llvm_type if restype is not None else llvm.VoidType()

        llvm_argtypes = (llvm.TypeRef * len(argtypes))()
        for i, ty in enumerate(argtypes):
            llvm_argtypes[i] = ty.llvm_type

        llvm_func_type = llvm.FunctionType(llvm_restype, llvm_argtypes, len(llvm_argtypes), vargs)
        llvm_func = llvm.AddFunction(module, name, llvm_func_type)
        llvm.SetLinkage(llvm_func, llvm.ExternalLinkage)

        for i, ty in enumerate(argtypes):
            if llvm.GetTypeKind(ty.llvm_type) == llvm.PointerTypeKind:
                llvm.AddAttribute(llvm.GetParam(llvm_func, i), llvm.NoAliasAttribute)

        if decl.options["inline"]:
            llvm.AddFunctionAttr(llvm_func, llvm.AlwaysInlineAttribute)

    return llvm_func, exists


def _validate_function_args(decl, args):
    """Raises TypeError if if *args* do not match function declaration."""
    import inspect

    if len(args) != len(decl.argtypes):
        raise TypeError("{0}() takes exactly {1} argument(s) ({2} given)"
                        .format(decl.__name__, len(decl.argtypes), len(args)))

    if decl.pyfunc:
        # External functions (eg. c_function) don't have PyFunc associated with them.
        spec = inspect.getargspec(decl.pyfunc)
        mask = map(llvm.types_equal,
                   (decl.argtypes[name].llvm_type for name in spec.args),
                   (llvm.TypeOf(val) for val in args))

        if not all(mask):
            wrong_args = ", ".join((a for a, ok in zip(spec.args, mask) if not ok))
            raise TypeError("{0}() called with wrong argument type(s) for {1}"
                            .format(decl.__name__, wrong_args))


def _unpack_translation_error(func_lines, args, before=2, after=5):
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
