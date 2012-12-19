from __future__ import absolute_import
import ctypes
import ast

from .. import llvm


def type_key(ty):
    """Returns unique key for type *ty*.

    In LLVM, getting the same type (eg. IntType(32)) yields
    same unique pointer value each time its invoked.

    """
    return ctypes.cast(ty, ctypes.c_void_p).value


def types_equal(tx, ty):
    """Returns True if *tx* is the same LLVMTypeRef as *ty*.

    To check equality, retrieve and compare raw pointer values.

    """
    return type_key(tx) == type_key(ty)


class Scalar(object):
    """Base for all scalar data types."""

    def __init__(self, c_type, llvm_type, name, tag):
        self.c_type = c_type
        self.llvm_type = llvm_type
        self.name = name
        self.tag = tag

    def __call__(self, v):
        """Nicer equivalent to ``cast(v, Type)``"""
        from nitrous.lib import cast
        return cast(v, self)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<Scalar '{0.name}'>".format(self)


def _int_type(c_type, name):
    """Creates a new integral type"""
    width = ctypes.sizeof(c_type)
    return Scalar(c_type, llvm.IntType(width * 8), name, "i{0}".format(width))


Double = Scalar(ctypes.c_double, llvm.DoubleType(), "Double", "f8")

Float = Scalar(ctypes.c_float, llvm.FloatType(), "Float", "f4")


Long = _int_type(ctypes.c_long, "Long")

Int = _int_type(ctypes.c_int, "Int")

Bool = _int_type(ctypes.c_bool, "Bool")

Byte = _int_type(ctypes.c_byte, "Byte")

Char = _int_type(ctypes.c_char, "Char")


# Akin to size_t in C, this is used for all memory accessing operations.
# TODO switch this to Int by default?
Index = Long


def const_index(v):
    """Creates a new constant index value."""
    return llvm.ConstInt(Index.llvm_type, v, True)


_FLOATING_BINARY_INST = {
    ast.Add: llvm.BuildFAdd,
    ast.Sub: llvm.BuildFSub,
    ast.Mult: llvm.BuildFMul,
    ast.Div: llvm.BuildFDiv,
    ast.Pow: llvm.build_pow,
}

_INTEGRAL_BINARY_INST = {
    ast.Add: llvm.BuildAdd,
    ast.Sub: llvm.BuildSub,
    ast.Mult: llvm.BuildMul,
    ast.Pow: llvm.build_pow,
    # Integer division is consciously left out and
    # handled in function.py/emit_binary_op
}

BINARY_INST = {
    type_key(Double.llvm_type): _FLOATING_BINARY_INST,
    type_key(Float.llvm_type): _FLOATING_BINARY_INST,
    type_key(Long.llvm_type): _INTEGRAL_BINARY_INST,
    type_key(Byte.llvm_type): _INTEGRAL_BINARY_INST,
}

_FLOATING_UNARY_INST = {
    ast.USub: llvm.BuildFNeg
}

_INTEGRAL_UNARY_INST = {
    ast.USub: llvm.BuildNeg
}

UNARY_INST = {
    type_key(Double.llvm_type): _FLOATING_UNARY_INST,
    type_key(Float.llvm_type): _FLOATING_UNARY_INST,
    type_key(Long.llvm_type): _INTEGRAL_UNARY_INST,
    type_key(Byte.llvm_type): _INTEGRAL_UNARY_INST,
}


_FLOATING_COMPARE_INST = (
    llvm.BuildFCmp, {
        ast.Eq: llvm.RealUEQ,
        ast.Gt: llvm.RealUGT,
        ast.GtE: llvm.RealUGE,
        ast.Lt: llvm.RealULT,
        ast.LtE: llvm.RealULE,
        ast.NotEq: llvm.RealUNE,
    }
)


_INTEGRAL_COMPARE_INST = (
    llvm.BuildICmp, {
        ast.Eq: llvm.IntEQ,
        ast.Gt: llvm.IntSGT,
        ast.GtE: llvm.IntSGE,
        ast.Lt: llvm.IntSLT,
        ast.LtE: llvm.IntSLE,
        ast.NotEq: llvm.IntNE
    }
)


COMPARE_INST = {
    type_key(Double.llvm_type): _FLOATING_COMPARE_INST,
    type_key(Float.llvm_type): _FLOATING_COMPARE_INST,
    type_key(Long.llvm_type): _INTEGRAL_COMPARE_INST,
    type_key(Int.llvm_type): _INTEGRAL_COMPARE_INST,
    type_key(Byte.llvm_type): _INTEGRAL_COMPARE_INST
}


Dynamic = object()


class Pointer(object):
    """Pointer to memory block, each element of type `element_type`."""

    def __init__(self, element_type, shape=(None,)):
        self.element_type = element_type
        self.shape = shape

    def __repr__(self):
        return "<Pointer to {0}>".format(self.element_type)

    @property
    def llvm_type(self):
        return llvm.PointerType(self.element_type.llvm_type, 0)

    @property
    def c_type(self):
        return ctypes.POINTER(self.element_type.c_type)

    @property
    def tag(self):
        return "P{0}".format(self.element_type.tag)

    @property
    def null(self):
        """Returns NULL value of current pointer type."""
        return llvm.ConstNull(self.llvm_type)

    def convert(self, p):
        import array

        # TODO it would be nice to just see if the object
        # supports buffer access interface, however it seems that
        # neither array.array nor ctypes byref() results support that.

        pointer_type = ctypes.POINTER(self.element_type.c_type)

        try:
            import numpy as np
            if isinstance(p, np.ndarray):
                return p.ctypes.data_as(pointer_type)
        except ImportError:
            pass

        if isinstance(p, array.array):
            addr, count = p.buffer_info()
            return ctypes.cast(addr, pointer_type)

        return p


class Structure(object):

    FIELD_NAME, FIELD_TYPE = range(2)

    # TODO add packing flag

    def __init__(self, name, *fields):
        self.name = name
        self.fields = fields
        self.c_type = type(name + "_CType",
                           (ctypes.Structure,),
                           {"_fields_": [(f, t.c_type) for f, t in fields]})

        # LLVM type name is uniqued automatically.
        self.llvm_type = llvm.StructCreateNamed(llvm.GetGlobalContext(), name)
        llvm_fields = (llvm.TypeRef * len(fields))(*(t.llvm_type for f, t in fields))
        llvm.StructSetBody(self.llvm_type, llvm_fields, len(fields), False)

    def __repr__(self):
        return "<Structure '{0}', {1} fields>".format(self.name, len(self.fields))

    @property
    def tag(self):
        # Reacquire name from created structure; it will get uniqued
        # if specified name was already taken.
        return "S{0}".format(llvm.GetStructName(self.llvm_type))

    def emit_getattr(self, builder, ref, attr):
        """IR: Emits attribute value load from structure reference."""
        gep, t = self._field_gep(builder, ref, attr)
        return llvm.BuildLoad(builder, gep, "v"), t

    def emit_setattr(self, builder, ref, attr, v):
        """IR: Emits GEP used to set the attribute value."""
        addr, _ = self._field_gep(builder, ref, attr)
        llvm.BuildStore(builder, v, addr)

    def _field_gep(self, builder, p, field):
        """Returns GEP and type for a *field*"""
        for i, (f, t) in enumerate(self.fields):
            if f == field:
                gep = llvm.BuildStructGEP(builder, p, i, "gep")
                return gep, self.fields[i][self.FIELD_TYPE]

        raise KeyError(field)


class Reference(object):
    """Special type to denote reference to an aggregate value."""

    def __init__(self, value_type):
        self.value_type = value_type

    @property
    def c_type(self):
        return ctypes.POINTER(self.value_type.c_type)

    @property
    def llvm_type(self):
        return llvm.PointerType(self.value_type.llvm_type, 0)

    @property
    def tag(self):
        return "R{0}".format(self.value_type.tag)

    def convert(self, v):
        return ctypes.byref(self.value_type.convert(v)
                            if hasattr(self.value_type, "convert")
                            else v)


String = Scalar(ctypes.c_char_p, Pointer(Char).llvm_type, "String", "S")
"""Null-terminated byte string.

This is virtually equivalent to Pointer(Char), except
that it provides a better Python interop by mapping to
``ctypes.c_char_p``

"""


def is_aggregate(ty):
    """Returns True if type is an aggregate."""
    kind = llvm.GetTypeKind(ty.llvm_type)
    # For now the only aggregates we have are Structs
    return kind == llvm.StructTypeKind
