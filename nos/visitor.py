import ast
import ctypes

from .exceptions import CompilationError
from . import llvm


OPS = {
    llvm.DoubleTypeKind: {
        ast.Add: llvm.BuildFAdd,
        ast.Sub: llvm.BuildFSub,
        ast.Mult: llvm.BuildFMul,
        ast.Div: llvm.BuildFDiv,
    },
    llvm.IntegerTypeKind: {
        ast.Add: llvm.BuildAdd,
        ast.Sub: llvm.BuildSub,
        ast.Mult: llvm.BuildMul,
        ast.Div: llvm.BuildSDiv,
    }
}

ICMP = {
    llvm.IntegerTypeKind: llvm.BuildICmp,
    llvm.DoubleTypeKind: llvm.BuildFCmp,
}

ICMP_OPS = {
    llvm.IntegerTypeKind: {
        ast.Eq: llvm.IntEQ,
        ast.Gt: llvm.IntSGT,
        ast.GtE: llvm.IntSGE,
        ast.Lt: llvm.IntSLT,
        ast.LtE: llvm.IntSLE,
        ast.NotEq: llvm.IntNE
    },
    llvm.DoubleTypeKind: {
        # TODO Currently allow unordered floats; possible
        # optimization to introduce ordered-only mode?
        ast.Eq: llvm.RealUEQ,
        ast.Gt: llvm.RealUGT,
        ast.GtE: llvm.RealUGE,
        ast.Lt: llvm.RealULT,
        ast.LtE: llvm.RealULE,
        ast.NotEq: llvm.RealUNE,
    }
}

BOOL_OPS = {
    ast.And: llvm.BuildAnd,
    ast.Or: llvm.BuildOr
}


class Visitor(ast.NodeVisitor):

    def __init__(self, module, builder, vars):
        self.module = module
        self.builder = builder
        self.vars = vars
        self.stack = []

    def visit_Num(self, node):
        from .types import Long, Double

        if isinstance(node.n, float):
            self.stack.append(llvm.ConstReal(Double.llvm_type, node.n))
        elif isinstance(node.n, int):
            self.stack.append(llvm.ConstInt(Long.llvm_type, node.n, True))
        else:
            raise TypeError("Uknown Number type {0!s}".format(type(node.n)))

    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):
            try:
                self.stack.append(self.vars[node.id])
            except KeyError:
                # `vars` would contain all local/global symbols
                # that were available during decoration, so last thing
                # is to try importing the symbol by its name.
                try:
                    modulename, attrname = node.id.rsplit(".", 1)
                except ValueError:
                    raise CompilationError(
                        "Line {0}: {1} is not defined or available at this point"
                        .format(node.lineno, node.id)
                    )
                attr = getattr(__import__(modulename, {}, {}, [attrname]), attrname)
                self.stack.append(attr)
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
            name = self.stack.pop()
            if name in self.vars:
                # Cannot reassign variables
                raise ValueError("{0!s} is reassigned".format(name))
            self.vars[name] = rhs

        elif isinstance(target, ast.Subscript):
            # foo[i] = rhs; foo[i] is the GEP, previous pushed by visit_Subscript
            llvm.BuildStore(self.builder, rhs, self.stack.pop())

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

        # Operands must be of the same type kind
        # TODO if integer, also verify the bit width?
        type_kind = llvm.GetTypeKind(llvm.TypeOf(lhs))
        if (type_kind != llvm.GetTypeKind(llvm.TypeOf(rhs))):
            raise CompilationError(
                "Cannot apply {0} to {1} and {2}; type kind doesn't match"
                .format(node.op, lhs, rhs)
            )

        v = OPS[type_kind][type(node.op)](self.builder, lhs, rhs, "tmp")
        self.stack.append(v)

    def visit_BoolOp(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()
        lhs = self.stack.pop()

        v = BOOL_OPS[type(node.op)](self.builder, lhs, rhs, "tmp")
        self.stack.append(v)

    def visit_Compare(self, node):
        if len(node.ops) > 1 or len(node.comparators) > 1:
            raise CompilationError("Only simple `if` expressions are supported")

        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()
        lhs = self.stack.pop()

        # TODO for now support simple comparisons only
        type_kind = llvm.GetTypeKind(llvm.TypeOf(lhs))
        if type_kind != llvm.GetTypeKind(llvm.TypeOf(rhs)):
            raise CompilationError(
                "Cannot apply {0} to {1} and {2}; type kind doesn't match"
                .format(node.op, lhs, rhs)
            )

        op = ICMP_OPS[type_kind][type(node.ops[0])]
        v = ICMP[type_kind](self.builder, op, lhs, rhs, "tmp")

        self.stack.append(v)

    def visit_IfExp(self, node):
        self.visit(node.test)
        test_expr = self.stack.pop()

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))
        if_branch_bb = llvm.AppendBasicBlock(func, "if")
        else_branch_bb = llvm.AppendBasicBlock(func, "else")
        merge_bb = llvm.AppendBasicBlock(func, "merge")

        llvm.BuildCondBr(self.builder, test_expr, if_branch_bb, else_branch_bb)

        llvm.PositionBuilderAtEnd(self.builder, if_branch_bb)
        ast.NodeVisitor.visit(self, node.body)
        if_expr = self.stack.pop()
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        if_branch_bb = llvm.GetInsertBlock(self.builder)

        llvm.PositionBuilderAtEnd(self.builder, else_branch_bb)
        ast.NodeVisitor.visit(self, node.orelse)
        else_expr = self.stack.pop()
        llvm.BuildBr(self.builder, merge_bb)

        # Getting updated insertion block in case of nested conditionals
        else_branch_bb = llvm.GetInsertBlock(self.builder)

        expr_type = llvm.TypeOf(if_expr)
        if llvm.GetTypeKind(expr_type) != llvm.GetTypeKind(llvm.TypeOf(else_expr)):
            raise CompilationError(
                "`if` expression clause return types don't match"
            )

        llvm.PositionBuilderAtEnd(self.builder, merge_bb)
        phi = llvm.BuildPhi(self.builder, expr_type, "phi")
        llvm.AddIncoming(phi, ctypes.byref(if_expr), ctypes.byref(if_branch_bb), 1)
        llvm.AddIncoming(phi, ctypes.byref(else_expr), ctypes.byref(else_branch_bb), 1)

        self.stack.append(phi)


    def visit_For(self, node):
        from .types import Long

        if len(node.orelse) != 0:
            raise CompilationError("`else` in a `for` statement is not supported")

        func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(self.builder))

        top_bb = llvm.GetInsertBlock(self.builder)

        loop_bb = llvm.AppendBasicBlock(func, "loop_start")
        body_bb = llvm.AppendBasicBlock(func, "loop_body")
        exit_bb = llvm.AppendBasicBlock(func, "loop_exit")

        # Reorder blocks for better flowing IR listing; not strictly necessary
        llvm.MoveBasicBlockAfter(loop_bb, top_bb)
        llvm.MoveBasicBlockAfter(body_bb, loop_bb)
        llvm.MoveBasicBlockAfter(exit_bb, body_bb)

        llvm.BuildBr(self.builder, loop_bb)

        # Loop header; get loop variable, iteration limits.
        llvm.PositionBuilderAtEnd(self.builder, loop_bb)

        self.visit(node.target)
        target = self.stack.pop()

        self.visit(node.iter)
        start, stop, step = self.stack.pop()

        # TODO Duplicated in assign; refactor
        if target in self.vars:
            # Cannot reassign variables
            raise ValueError("{0!s} is reassigned".format(target))

        i = llvm.BuildPhi(self.builder, Long.llvm_type, "loop_{0}".format(target))
        llvm.AddIncoming(i, ctypes.byref(start), ctypes.byref(top_bb), 1)

        t = llvm.BuildICmp(self.builder, llvm.IntSLT, i, stop, "loop_{0}_cmp".format(target))
        llvm.BuildCondBr(self.builder, t, body_bb, exit_bb)

        # Loop body; adding loop variable for the local scope
        self.vars[target] = i

        llvm.PositionBuilderAtEnd(self.builder, body_bb)
        for b in node.body:
            self.visit(b)

        # TODO loop variables in CPython functions are available from
        # the point of declaration to end of the function scope, so this
        # doesn't conform.
        del self.vars[target]

        body_bb = llvm.GetInsertBlock(self.builder)
        i_next = llvm.BuildAdd(self.builder, i, step, "{0}_next".format(target))
        llvm.AddIncoming(i, ctypes.byref(i_next), ctypes.byref(body_bb), 1)
        llvm.BuildBr(self.builder, loop_bb)

        # Loop exit
        llvm.PositionBuilderAtEnd(self.builder, exit_bb)

    def visit_Call(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        args = [self.stack.pop() for _ in range(len(node.args))][::-1]
        func = self.stack.pop()
        result = func(*args)

        # Function returned something that can be LLVM-ed
        if getattr(result, "__nos_emitter__", False):
            result = result.emit(self.module, self.builder)

        self.stack.append(result)


class FlattenAttributes(ast.NodeTransformer):
    """Flattens attributes into name nodes.

    Eg. Attribute(value=Attribute("a"), attr="b") -> Name(id="a.b")

    """

    def __init__(self, builder, vars):
        self.builder = builder
        self.vars = vars
        self.stack = []

    def visit_Attribute(self, node):
        if not isinstance(node.ctx, ast.Load):
            raise CompilationError("Setting attributes not supported")

        node = ast.NodeTransformer.generic_visit(self, node)
        assert isinstance(node.value, ast.Name)

        return ast.Name(id=node.value.id + "." + node.attr, ctx=ast.Load())
