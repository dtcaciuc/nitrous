from nitrous import llvm
from nitrous.function import function
from nitrous.lib import ValueEmitter
from nitrous.types.array import Slice


__all__ = ["Vector", "get_element", "set_element", "load", "store"]


class Vector(object):
    """Represents a vector of elements.

    Vector types are used when multiple primitive data are operated in
    parallel using a single instruction (SIMD). A vector type requires a size *n*
    (number of elements) and an underlying *element_type*.

    """

    def __init__(self, element_type, n):
        self.element_type = element_type
        self.n = n

    def __repr__(self):
        return "<Vector [{0.n} x {0.element_type}]>".format(self)

    @property
    def llvm_type(self):
        return llvm.VectorType(self.element_type.llvm_type, self.n)

    @property
    def c_type(self):
        return (self.element_type.c_type * self.n)

    @property
    def tag(self):
        return "V{0}{1}".format(self.n, self.element_type.tag)

    def __call__(self, *elements):
        """Create new vector with specified initial element values."""
        from nitrous.lib import cast
        from nitrous.types import const_index

        if len(elements) == 0:
            elements = (llvm.ConstNull(self.element_type.llvm_type),) * self.n

        if len(elements) != self.n:
            raise TypeError("Cannot initialize vector of {0} elements with {1} values"
                            .format(self.n, len(elements)))

        def emit(builder):
            v = llvm.ConstNull(self.llvm_type)
            for i, e in enumerate(elements):
                llvm_i = _index(builder, const_index(i))
                cast_e, _ = cast(e, self.element_type).emit(builder)
                v = llvm.BuildInsertElement(builder, v, cast_e, llvm_i, "v.init")

            return v, self

        return ValueEmitter(emit)


def get_element(T):
    """``get_element(T)(v, i) -> e``

    Get vector *v* element at index *i*

    """

    def get_element_(v, i):

        def emit(builder):
            ii = _index(builder, i)
            e = llvm.BuildExtractElement(builder, v, ii, "v.get")
            return e, T.element_type

        return ValueEmitter(emit)

    return get_element_


def set_element(T):
    """``set_element(T)(v, i, e) -> w``

    Set vector *v* element at index *i* to *e* and return new vector.

    """

    def set_element_(v, i, e):

        def emit(builder):
            ii = _index(builder, i)
            w = llvm.BuildInsertElement(builder, v, e, ii, "v.set")
            return w, T

        return ValueEmitter(emit)

    return set_element_


def fill(T):
    """``fill(T)(e) -> v``

    Creates a vector with *e* value in every element.

    """

    @function(T, e=T.element_type)
    def fill_(e):
        v = T()
        for i in range(T.n):
            v = set_element(T)(v, i, e)
        return v

    return fill_


def load(T):
    """``load(T)(s) -> v``

    Load vector elements from slice *s*

    """

    @function(T, s=Slice(T.element_type, (T.n,)))
    def load_(s):
        v = T()
        for i in range(T.n):
            v = set_element(T)(v, i, s[i])
        return v

    return load_


def store(T):
    """``store(T)(v, s) -> None``

    Stores vector elements in sequential locations
    in slice *s*

    """

    @function(v=T, s=Slice(T.element_type, (T.n,)))
    def store_(v, s):
        for i in range(T.n):
            s[i] = get_element(T)(v, i)

    return store_


def _index(builder, i):
    """Prepare 32-bit integer index for vector element access"""
    return llvm.BuildCast(builder, llvm.Trunc, i, llvm.IntType(32), "cast.i32")
