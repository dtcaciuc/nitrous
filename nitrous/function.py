import ast
import ctypes

from contextlib import contextmanager
from . import llvm


BOOL_INST = {
    ast.And: llvm.BuildAnd,
    ast.Or: llvm.BuildOr
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
        from .function import Function
        from .lib import _range
        import functools
        import inspect

        # Types can provide a susbtitution if they're used directly
        # as an argument type (eg. Structure needs to be implicitly
        # passed as Reference() to said structure.
        argtypes = dict(
            (k, t.argtype if hasattr(t, "argtype") else t)
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
        decl.globals["range"] = _range
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


# TODO reenable
class ExternalFunction(object):
    """Stores information about externally defined function included in the module."""

    def __init__(self, name, func, restype, argtypes):
        self.__name__ = name
        self.__n2o_func__ = func
        self.__n2o_restype__ = restype
        self.__n2o_argtypes__ = argtypes


# FIXME restore included functions
def include_function(self, name, restype, argtypes, lib=None, libdir=None):
    """Includes externally defined function for use with the module.

    Typically this is a function in an external shared or static library. C and
    Fortran functions should be good to interface with; unfortunately C++ should
    be kept at a distance from for the time being.

    :param name: function name as it's listed in library symbols.
    :param restype: function return value type
    :param argtypes: sequence of function argument types
    :param lib: library name where function is defined and we'll link with
    :param libdir: directory where library is located

    """
    from .function import ExternalFunction

    func = _create_function(self.module, name, restype, argtypes)

    if lib is not None:
        self.libs.append(lib)

    if libdir is not None:
        self.libdirs.append(libdir)

    return ExternalFunction(name, func, restype, argtypes)


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

    def __init__(self, builder, globals_, opts):
        self.builder = builder
        self.opts = opts

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

        # List of compiled functions that has been used
        # but were not yet declared.
        self.new_funcs = []

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
                # Register value type, if supplied;
                # also see if value has a registered type already.
                type_ = type_ or self.typeof(value)
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
            super(FunctionBuilder, self).visit(node)
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
            self.store(node.id, self.pop())
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
                self.push(getattr(v, node.attr))
            else:
                raise NotImplementedError("Unsupported attribute context {0}".format(node.ctx))

    def visit_Tuple(self, node):

        if isinstance(node.ctx, ast.Load):
            v = []
            for e_node in node.elts:
                self.visit(e_node)
                v.append(self.pop())

            self.push(tuple(v))

        elif isinstance(node.ctx, ast.Store):
            rhs = self.pop()
            try:
                rhs_len = len(rhs)
            except TypeError:
                raise TypeError("{0} is not an interable".format(rhs))

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
        from .types import Reference

        ast.NodeVisitor.generic_visit(self, node)
        i = self.pop()
        v = self.pop()

        # Index is a nd tuple
        vt = self.typeof(v)

        if isinstance(vt, Reference):
            vt = vt.value_type

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

    def visit_UnaryOp(self, node):
        from .types import UNARY_INST, type_key

        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.pop()

        op_type = type(node.op)
        if op_type == ast.Not:
            # Boolean `not`
            rhs = emit_nonzero(self.builder, rhs)
            inst = llvm.BuildNot
        else:
            inst = UNARY_INST[type_key(llvm.TypeOf(rhs))][op_type]

        v = inst(self.builder, rhs, op_type.__name__.lower())
        self.push(v)

    def visit_BinOp(self, node):
        from .types import BINARY_INST, type_key, types_equal

        ast.NodeVisitor.generic_visit(self, node)

        rhs = self.pop()
        lhs = self.pop()
        op = node.op

        ty = llvm.TypeOf(lhs)
        if not types_equal(ty, llvm.TypeOf(rhs)):
            raise TypeError("Conflicting operand types for {0}: {1} and {2}"
                            .format(op, lhs, rhs))

        if isinstance(op, ast.Div) and llvm.GetTypeKind(ty) == llvm.IntegerTypeKind:
            inst = llvm.BuildSDiv if self.opts["cdiv"] else llvm.build_py_idiv
        else:
            inst = BINARY_INST[type_key(ty)][type(op)]

        v = inst(self.builder, lhs, rhs, type(op).__name__.lower())
        self.push(v, ty)

    def visit_BoolOp(self, node):
        ast.NodeVisitor.generic_visit(self, node)

        rhs = emit_nonzero(self.builder, self.pop())
        # Expressions like `a > 1 or b > 1 or c > 1` collapse into one `or` with 3 .values
        for _ in range(len(node.values) - 1):
            rhs = BOOL_INST[type(node.op)](self.builder,
                                           emit_nonzero(self.builder, self.pop()),
                                           rhs, "cmp")
        self.push(rhs)

    def visit_Compare(self, node):
        from .types import COMPARE_INST, _INTEGRAL_COMPARE_INST, type_key, types_equal

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

        # FIXME move this reponsibility to individual types.
        if llvm.GetTypeKind(ty) == llvm.PointerTypeKind:
            inst, ops = _INTEGRAL_COMPARE_INST
        else:
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

        self.visit(node.test)
        test = self.pop()

        true_ = llvm.ConstInt(llvm.IntType(1), 1, True)
        t = llvm.BuildICmp(self.builder, llvm.IntEQ, test, true_, "t")
        llvm.BuildCondBr(self.builder, t, body_bb, exit_bb)

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

        self.visit(node.iter)
        start, stop, step = self.pop()

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
        ast.NodeVisitor.generic_visit(self, node)
        args = [self.pop() for _ in range(len(node.args))][::-1]
        func = self.pop()

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

    def _get_or_create_function(self, decl):
        """Returns LLVM function value for given declaration;
        creates one if it doesn't exist yet.

        """
        # XXX this function may be another one already registered which happened
        # to have the same unqualified name (e.g. imported from another module.)
        # Check and disallow repetition of unqualified names in one module.

        module = llvm.GetParentModule__(self.builder)
        name = _qualify(module, decl.__name__)
        llvm_func = llvm.GetNamedFunction(module, name)

        if not llvm_func:
            llvm_func = _create_function(module, decl)
            self.new_funcs.append(Function(decl, llvm_func))

        return llvm_func


class UnpackAugAssign(ast.NodeTransformer):
    """Replaces augmented assignments with non-augmented ones.

    Eg., ``x[i] += y`` to ``x[i] = x[i] + y``.

    """

    def visit_AugAssign(self, node):
        from ast import BinOp, Assign, Load, copy_location
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
    b = FunctionBuilder(builder, dict(resolve_constants(func.globals)), func.decl.options)
    llvm.PositionBuilderAtEnd(builder, llvm.AppendBasicBlock(func.llvm_func, "entry"))

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


def entry_alloca(func, type_, name):
    """Reserves stack space for a variable at function entry point."""
    entry = llvm.GetEntryBasicBlock(func)
    builder = llvm.CreateBuilder()

    llvm.PositionBuilder(builder, entry, llvm.GetFirstInstruction(entry))
    a = llvm.BuildAlloca(builder, type_, name)

    llvm.DisposeBuilder(builder)
    return a


def entry_array_alloca(func, element_type, n, name):
    """Reserves stack space for an array of size *n* at function entry point."""
    entry = llvm.GetEntryBasicBlock(func)
    builder = llvm.CreateBuilder()

    llvm.PositionBuilder(builder, entry, llvm.GetFirstInstruction(entry))
    a = llvm.BuildArrayAlloca(builder, element_type, n, name)

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


def emit_nonzero(builder, v):
    """Emits check to see whether the value of *v* is non-zero."""
    ty = llvm.TypeOf(v)
    kind = llvm.GetTypeKind(ty)
    if kind in (llvm.IntegerTypeKind, llvm.PointerTypeKind):
        v = llvm.BuildICmp(builder, llvm.IntNE, v, llvm.ConstNull(ty), "nz")
    else:
        raise TypeError("Incompatible type for boolean expressions")
        # TODO say which type exactly
    return v


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


def _qualify(module, symbol):
    """Qualifies symbol with parent module name."""
    return "__".join((llvm.GetModuleName(module), symbol))


def _create_function(module, decl, name=None):
    """Declares an an LLVM function based on its declaration."""
    name = name or _qualify(module, decl.__name__)
    argtypes = [decl.argtypes[arg] for arg in decl.args]
    restype = decl.restype

    llvm_restype = restype.llvm_type if restype is not None else llvm.VoidType()

    llvm_argtypes = (llvm.TypeRef * len(argtypes))()
    for i, ty in enumerate(argtypes):
        llvm_argtypes[i] = ty.llvm_type

    llvm_func_type = llvm.FunctionType(llvm_restype, llvm_argtypes, len(llvm_argtypes), 0)
    llvm_func = llvm.AddFunction(module, name, llvm_func_type)
    llvm.SetLinkage(llvm_func, llvm.ExternalLinkage)

    if decl.options["inline"]:
        llvm.AddFunctionAttr(llvm_func, llvm.AlwaysInlineAttribute)

    return llvm_func


def _validate_function_args(decl, args):
    """Raises TypeError if if *args* do not match function declaration."""
    from .types import types_equal
    import inspect

    if len(args) != len(decl.argtypes):
        raise TypeError("{0}() takes exactly {1} argument(s) ({2} given)"
                        .format(decl.__name__, len(decl.argtypes), len(args)))

    spec = inspect.getargspec(decl.pyfunc)
    mask = map(types_equal,
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
