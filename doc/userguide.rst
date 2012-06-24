
Installation
============

Nitrous requires LLVM and clang to be installed (the current officially supported version is 3.1).

The easiest way is to download precompiled binaries from http://llvm.org/releases. The exact install location is not important, however make sure that the ``bin/`` directory is in your system's path.

First Steps
===========

Let's write a customary semi-useful function which gives an overview of what the project is all about::

    from time import time

    def fib(n):
        if n == 0 or n == 1:
            return n
        else:
            return fib(n - 1) + fib(n - 2)


    t0 = time()
    print fib(35), time() - t0
    # 9227465 6.34074497223


The absolute timing number will vary between machines and is not really important. While this is clearly not the best formulation of the problem, in our case, it's actually useful to showcase the speed gains. To achieve them, we need to augment the code as following::

    from time import time
    from nitrous.module import Module  # (1)
    from nitrous.types import Long     #

    m = Module(__name__)               # (2)

    @m.function(Long, n=Long)          # (3)
    def fib(n):
        if n == 0 or n == 1:
            return n
        else:
            return fib(n - 1) + fib(n - 2)

    out = m.build()                    # (4)

    t0 = time()
    print fib(35), time() - t0         # (5)
    # 9227465 6.26375293732

    t1 = time()
    print out.fib(35), time() - t1     # (6)
    # 9227465 0.0859870910645

1. The library symbols reside in ``nitrous`` package.
2. Declare a :class:`~nitrous.module.Module` object. Modules are the top level containers for the optimized functions and provide a way to build them. The name is not really important, but should be unique. Usually, there's one of these per Python module, so we can use it's name.
3. Annotate the function with result and argument types. Here, we'll use :class:`~nitrous.types.Long` type, which is equivalent to C ``long int`` or Python ``int``. Nitrous follows ``ctypes`` convention and names its types after their matching C counterparts of the same width/precision.
4. The last step in the process is to actually build the module. This might take a bit of time, however since normally performed at the top level and is done only once per program run.
5. The original function is not modified and can be still used as before.
6. The build result is a new python module object which contains the optimized version of our function under the same attribute name.

The second timed interval is clearly shorter and yields the same result, which is what we want. The :meth:`~nitrous.module.Module.function` decorator instructed the library to parse ``fib()`` into a syntax tree, then translate it LLVM IR. That, in turn,  got sent through range of available LLVM optimizers, translated to native machine code and loaded back into Python as a shared library using ``ctypes``.


Feature Overview
================

Assignments
-----------

Basic assignments work as expected::

    x = 5.0  # OK, initialized variable x to Double(5.0)

Here, ``x`` is assigned the first time with a constant of type ``float``, which maps to Nitrous ``Double``.

The variable can be reassigned, but only with the value of the same type::

    x = 10.0  # OK, x now stores 10.0
    x = 5     # Error, int constants map to type Long

Casting
-------

The type system necessitates the exact match without any implicit casting. The :func:`nitrous.lib.cast` can be used to perform the conversions when necessary::

    from nitrous.lib import cast
    ...

    x = 5.0              # type Double
    y = 1                # type Long

    x = cast(y, Double)  # OK, assignment through explicit cast

Variable Scope
--------------

Variable lifetime and visibility is limited to the innermost enclosing conditional/loop block rather than the function::

    z = 0.0

    if x > 2.0:
        y = x + 10.0    # First use of y
        z = y + 5.0     # OK, y and z are both in the scope

    x = z               # OK, z is in the current scope
    x = y               # Error, y scope is limited to `if` block

