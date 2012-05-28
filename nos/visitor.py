import ast

from . import llvm


float_ops = {
    ast.Add: llvm.BuildFAdd,
    ast.Sub: llvm.BuildFSub,
    ast.Mult: llvm.BuildFMul,
    ast.Div: llvm.BuildFDiv,
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
        ast.NodeVisitor.generic_visit(self, node)
        llvm.BuildRet(self.builder, self.stack.pop())

    def visit_BinOp(self, node):
        ast.NodeVisitor.generic_visit(self, node)
        rhs = self.stack.pop()
        lhs = self.stack.pop()

        # FIXME hardcoded float type
        v = float_ops[type(node.op)](self.builder, lhs, rhs, "tmp")
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
