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
        self.__indent = ''

    def visit_Num(self, node):
        if isinstance(node.n, float):
            self.stack.append(llvm.ConstReal(llvm.DoubleType(), node.n))
        else:
            raise TypeError("Uknown Number type {0!s}".format(type(node.n)))

    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):
            self.stack.append(self.vars[node.id])
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

    def generic_visit(self, node):
        ast.NodeVisitor.generic_visit(self, node)
