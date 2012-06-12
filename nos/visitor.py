import ast
import ctypes

from contextlib import contextmanager
from .exceptions import CompilationError
from . import llvm


BOOL_INST = {
    ast.And: llvm.BuildAnd,
    ast.Or: llvm.BuildOr
}


class Visitor(ast.NodeVisitor):

    def __init__(self, module, builder, global_vars, local_vars):
        self.module = module
        self.builder = builder
        self.global_vars = global_vars

        # Stack of dictionaries of scoped variables;
        # Top scope is the function, then any nested indented
        # blocks (eg. if, for) as they occur. On block exit,
        # the associated scope is popped off.
        self.local_vars = [local_vars]

        # Stack of information for current loop and its parent ones.
        self.loop_info = []

        # Value stack used to assemble LLVM IR as the syntax tree is traversed.
        self.stack = []

    def _local_var(self, name):
        """Finds and returns local variable with a given *name*.

        Traverses the scope stack outwards until *name* is found; throws
        KeyError if unsuccessful.

        """
        for scope in self.local_vars[::-1]:
            v = scope.get(name)
            if v is not None:
                return v

        raise KeyError(name)

    @contextmanager
    def _local_scope(self):
        """Temporarily create new local variable scope."""
        self.local_vars.append({})
        yield
        self.local_vars.pop()

    def _store(self, value, name):
        """Stores *value* on the stack under *name*.

        Allocates stack space if *name* is not an existing variable. Returns
        the pointer to allocated space.

        """
        try:
            v = self._local_var(name)
        except KeyError:
            # First time storing the variable; allocate stack space
            # and register with most nested scope.
            func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
            v = entry_alloca(func, llvm.TypeOf(value), name + "_ptr")
            self.local_vars[-1][name] = v

        llvm.BuildStore(self.builder, value, v)
        return v

    def _load(self, node):
        # TODO pretty awkward, passing node only for line number used in raising error.
        try:
            # Try variables; they are all LLVM values and stack pointers.
            v = self._local_var(node.id)
            # Stack variables are pointers and carry _ptr prefix;
            # drop it when loading the value.
            name = llvm.GetValueName(v).rstrip("_ptr")
            return llvm.BuildLoad(self.builder, v, name)
        except KeyError:
            try:
                # Constant values or emitter functions declared externally.
                return self.global_vars[node.id]
            except KeyError:
                # Last thing to try is {module 1}...{module n}.{symbol} import.
                try:
                    modulename, attrname = node.id.rsplit(".", 1)
                except ValueError:
                    raise CompilationError(
                        "Line {0}: {1} is not defined or available at this point"
                        .format(node.lineno, node.id)
                    )
                return getattr(__import__(modulename, {}, {}, [attrname]), attrname)

    def visit_Num(self, node):
        self.stack.append(emit_constant(node.n))

    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):
            self.stack.append(self._load(node))
        elif isinstance(node.ctx, ast.Store):
            self.stack.append(node.id)
        else:
            raise ValueError("Uknown Name context {0!s}".format(type(node.ctx)))

    def visit_Subscript(self, node):
        """Label subscript of form `var_expr[index_expr]`.

        If in Load() context, return the value; if in Store()
        context, return the prepared GEP; parent node which knows
        about the source data will complete the instruction.

        """
        ast.NodeVisitor.generic_visit(self, node)
        i = self.stack.pop()
        v = self.stack.pop()

        addr = llvm.BuildGEP(self.builder, v, ctypes.byref(i), 1, "addr")
        if isinstance(node.ctx, ast.Load):
            self.stack.append(llvm.BuildLoad(self.builder, addr, "element"))
        elif isinstance(node.ctx, ast.Store):
            self.stack.append(addr)
        else:
            raise CompilationError("Unsupported subscript context {0}".format(node.ctx))

    def visit_Assign(self, node):
        target = node.targets[0]
        if len(node.targets) > 1:
            raise CompilationError("Unpacking assignment is not supported")

        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()

        if isinstance(target, ast.Name):
            # foo = rhs
            self._store(value=rhs, name=self.stack.pop())
        elif isinstance(target, ast.Subscript):
            # foo[i] = rhs; foo[i] is the GEP, previous pushed by visit_Subscript
            llvm.BuildStore(self.builder, rhs, self.stack.pop())
        else:
            raise CompilationError("Unsupported augmented assignment target {0}"
                                   .format(node.target))

    def visit_AugAssign(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()

        if isinstance(node.target, ast.Name):
            # foo += rhs
            name = self.stack.pop()
            lhs = self._load(node.target)
            rhs = emit_binary_op(self.builder, node.op, lhs, rhs)
            self._store(value=rhs, name=name)
        elif isinstance(node.target, ast.Subscript):
            # foo[i] += rhs; foo[i] is the GEP, previous pushed by visit_Subscript
            lhs_addr = self.stack.pop()
            lhs = llvm.BuildLoad(self.builder, lhs_addr, "element")
            rhs = emit_binary_op(self.builder, node.op, lhs, rhs)
            llvm.BuildStore(self.builder, rhs, lhs_addr)
        else:
            raise CompilationError("Unsupported augmented assignment target {0}"
                                   .format(node.target))

    def visit_Return(self, node):
        from .types import Bool

        ast.NodeVisitor.generic_visit(self, node)
        v = self.stack.pop()

        # Special case; if we're returning boolean, cast to i8
        # FIXME Move this to Bool.emit_cast_to or similar?
        t = llvm.TypeOf(v)
        if (llvm.GetTypeKind(t) == llvm.IntegerTypeKind
            and llvm.GetIntTypeWidth(t) == 1):
            v = llvm.BuildCast(self.builder, llvm.ZExt, v, Bool.llvm_type, "tmp")

        llvm.BuildRet(self.builder, v)

    def visit_BinOp(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()
        lhs = self.stack.pop()

        v = emit_binary_op(self.builder, node.op, lhs, rhs)
        self.stack.append(v)

    def visit_BoolOp(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()
        lhs = self.stack.pop()

        v = BOOL_INST[type(node.op)](self.builder, lhs, rhs, "tmp")
        self.stack.append(v)

    def visit_Compare(self, node):
        from .types import COMPARE_INST, type_key, types_equal

        if len(node.ops) > 1 or len(node.comparators) > 1:
            raise CompilationError("Only simple `if` expressions are supported")

        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()
        lhs = self.stack.pop()
        op = node.ops[0]

        ty = llvm.TypeOf(lhs)
        if not types_equal(ty, llvm.TypeOf(rhs)):
            raise CompilationError(
                "Cannot apply {0} to {1} and {2}; type kind doesn't match"
                .format(op, lhs, rhs)
            )

        inst, ops = COMPARE_INST[type_key(ty)]
        v = inst(self.builder, ops[type(op)], lhs, rhs, "tmp")

        self.stack.append(v)

    def visit_IfExp(self, node):
        from .types import types_equal

        self.visit(node.test)
        test_expr = self.stack.pop()

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        if_branch_bb = llvm.AppendBasicBlock(func, "if")
        else_branch_bb = llvm.AppendBasicBlock(func, "else")
        merge_bb = llvm.AppendBasicBlock(func, "merge")

        llvm.BuildCondBr(self.builder, test_expr, if_branch_bb, else_branch_bb)

        llvm.PositionBuilderAtEnd(self.builder, if_branch_bb)
        self.visit(node.body)
        if_expr = self.stack.pop()
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        if_branch_bb = llvm.GetInsertBlock(self.builder)

        llvm.PositionBuilderAtEnd(self.builder, else_branch_bb)
        self.visit(node.orelse)
        else_expr = self.stack.pop()
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        else_branch_bb = llvm.GetInsertBlock(self.builder)

        expr_type = llvm.TypeOf(if_expr)
        if not types_equal(expr_type, llvm.TypeOf(else_expr)):
            raise CompilationError(
                "`if` expression clause return types don't match"
            )

        llvm.PositionBuilderAtEnd(self.builder, merge_bb)
        phi = llvm.BuildPhi(self.builder, expr_type, "phi")
        llvm.AddIncoming(phi, ctypes.byref(if_expr), ctypes.byref(if_branch_bb), 1)
        llvm.AddIncoming(phi, ctypes.byref(else_expr), ctypes.byref(else_branch_bb), 1)

        self.stack.append(phi)

    def visit_If(self, node):
        self.visit(node.test)
        test_expr = self.stack.pop()

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        if_branch_bb = llvm.AppendBasicBlock(func, "if")
        else_branch_bb = llvm.AppendBasicBlock(func, "else")
        merge_bb = llvm.AppendBasicBlock(func, "merge")

        merged_if = False
        merged_else = False

        llvm.BuildCondBr(self.builder, test_expr, if_branch_bb, else_branch_bb)

        llvm.PositionBuilderAtEnd(self.builder, if_branch_bb)

        with self._local_scope():
            for b in node.body:
                self.visit(b)

        # Branching to merge bock only if the clause block hasn't terminated yet.
        if not llvm.IsATerminatorInst(llvm.GetLastInstruction(if_branch_bb)):
            llvm.BuildBr(self.builder, merge_bb)
            merged_if = True

        llvm.PositionBuilderAtEnd(self.builder, else_branch_bb)

        with self._local_scope():
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
            raise CompilationError("`else` in a `for` statement is not supported")

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
        target = self.stack.pop()

        self.visit(node.iter)
        start, stop, step = self.stack.pop()

        # Loop counter
        i_ptr = self._store(start, target)
        llvm.BuildBr(self.builder, test_bb)

        # Loop test
        llvm.PositionBuilderAtEnd(self.builder, test_bb)
        i = llvm.BuildLoad(self.builder, i_ptr, "loop_i")
        t = llvm.BuildICmp(self.builder, llvm.IntSLT, i, stop, "loop_{0}_cmp".format(target))
        llvm.BuildCondBr(self.builder, t, body_bb, exit_bb)

        llvm.PositionBuilderAtEnd(self.builder, body_bb)

        # Posting entrance and exit blocks (for continue/break respectively)
        self.loop_info.append((step_bb, exit_bb))

        with self._local_scope():
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
        args = [self.stack.pop() for _ in range(len(node.args))][::-1]
        func = self.stack.pop()

        if hasattr(func, "__nos_func__"):
            # Function is compiled; check arguments for validity and make a direct call
            _validate_function_args(func, args)
            result = llvm.BuildCall(self.builder, func.__nos_func__,
                                    (llvm.ValueRef * len(args))(*args),
                                    len(args), func.func_name + "_result")
        else:
            # Function is either CPython one or an LLVM emitter.
            result = func(*args)
            if getattr(result, "__nos_emitter__", False):
                result = result.emit(self.module, self.builder)

        self.stack.append(result)


class FlattenAttributes(ast.NodeTransformer):
    """Flattens attributes into name nodes.

    Eg. Attribute(value=Attribute("a"), attr="b") -> Name(id="a.b")

    """

    def __init__(self, builder):
        self.builder = builder
        self.stack = []

    def visit_Attribute(self, node):
        if not isinstance(node.ctx, ast.Load):
            raise CompilationError("Setting attributes not supported")

        node = ast.NodeTransformer.generic_visit(self, node)
        assert isinstance(node.value, ast.Name)

        return ast.Name(id=node.value.id + "." + node.attr, ctx=ast.Load())


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
    from .types import Long, Double

    if isinstance(value, float):
        return llvm.ConstReal(Double.llvm_type, value)
    elif isinstance(value, int):
        return llvm.ConstInt(Long.llvm_type, value, True)
    else:
        raise TypeError("Uknown Number type {0!s}".format(type(value)))


def emit_binary_op(builder, op, lhs, rhs):
    from .types import BINARY_INST, type_key, types_equal

    ty = llvm.TypeOf(lhs)
    if not types_equal(ty, llvm.TypeOf(rhs)):
        raise CompilationError(
            "Cannot apply {0} to {1} and {2}; type kind doesn't match"
            .format(op, lhs, rhs)
        )

    return BINARY_INST[type_key(ty)][type(op)](
        builder, lhs, rhs, type(op).__name__.lower()
    )


def _validate_function_args(func, args):
    """Raises TypeError if if *args* do not match annotated function signature."""
    from .types import types_equal
    import inspect

    if len(args) != len(func.__nos_argtypes__):
        raise TypeError("{0}() takes exactly {1} argument(s) ({2} given)"
                        .format(func.func_name, len(func.__nos_argtypes__), len(args)))

    spec = inspect.getargspec(func)
    mask = map(types_equal,
               (func.__nos_argtypes__[name].llvm_type for name in spec.args),
               (llvm.TypeOf(val) for val in args))

    if not all(mask):
        wrong_args = ", ".join((a for a, ok in zip(spec.args, mask) if not ok))
        raise TypeError("{0}() called with wrong argument type(s) for {1}"
                        .format(func.func_name, wrong_args))
