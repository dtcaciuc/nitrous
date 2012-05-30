import ast

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

    def __init__(self, builder, vars):
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
                modulename, attrname = node.id.rsplit(".", 1)
                attr = getattr(__import__(modulename), attrname)
                self.stack.append(attr)
        elif isinstance(node.ctx, ast.Store):
            if node.id in self.vars:
                # Cannot reassign variables
                raise ValueError("{0!s} is reassigned".format(node.id))
            self.stack.append(node.id)
        else:
            raise ValueError("Uknown Name context {0!s}".format(type(node.ctx)))

    def visit_Assign(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        v = self.stack.pop()
        name = self.stack.pop()
        self.vars[name] = v

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
        from . import CompilationError

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
        from . import CompilationError

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

    def visit_Call(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        args = [self.stack.pop() for _ in range(len(node.args))][::-1]
        func = self.stack.pop()
        result = func(*args)

        # Function returned something that can be LLVM-ed
        if getattr(result, "__nos_emitter__", False):
            result = result.emit(self.builder)

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
        from . import CompilationError

        if not isinstance(node.ctx, ast.Load):
            raise CompilationError("Setting attributes not supported")

        node = ast.NodeTransformer.generic_visit(self, node)
        assert isinstance(node.value, ast.Name)

        return ast.Name(id=node.value.id + "." + node.attr, ctx=ast.Load())
