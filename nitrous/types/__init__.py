from __future__ import absolute_import
import ctypes

from .. import llvm


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


def integer_type(c_type, name, tag=None):
    """Creates a new integer type"""
    bytes = ctypes.sizeof(c_type)
    tag = tag or "i{0}".format(bytes)
    return Scalar(c_type, llvm.IntType(bytes * 8), name, tag)


Double = Scalar(ctypes.c_double, llvm.DoubleType(), "Double", "f8")
"""Double-precision floating point number."""

Float = Scalar(ctypes.c_float, llvm.FloatType(), "Float", "f4")
"""Single-precision floating point number."""

Long = integer_type(ctypes.c_long, "Long")
"""Long integer."""

Int = integer_type(ctypes.c_int, "Int")
"""Integer."""

Byte = integer_type(ctypes.c_byte, "Byte")
"""8-bit integer."""

Char = integer_type(ctypes.c_char, "Char")
"""Signed character."""

Bool = integer_type(ctypes.c_bool, "Bool", "b")
"""Boolean value."""


# Akin to size_t in C, this is used for all memory accessing operations.
# TODO switch this to Int by default?
Index = Long
"""Type used for loop counters and to index array/slice elements."""


def const_index(v):
    """Creates a new constant index value."""
    return llvm.ConstInt(Index.llvm_type, v, True)


class Pointer(object):
    """Pointer to memory block, each element of type `element_type`."""

    def __init__(self, element_type):
        self.element_type = element_type

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
