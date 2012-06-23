
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
    print fib(35), time() - t0
    # 9227465 6.26375293732

    t1 = time()
    print out.fib(35), time() - t1     # (5)
    # 9227465 0.0859870910645

The second timed interval is clearly shorter and yields the same result.

1. The library symbols reside in ``nitrous`` package.
2. Declare a :class:`~nitrous.module.Module` object. Modules are the top level containers for the optimized functions and provide access point to them after compilation. The name is not really important, but should be unique. Usually, there's one of these per Python module, so we can use it's name.
3. Annotate the function with result and argument types. Here, we'll use :class:`~nitrous.types.Long` type, which is equivalent to C ``long int`` or Python ``int``. Nitrous follows ``ctypes`` convention and names its types after their matching C counterparts of the same width/precision.
4. The last step in the process is to actually build the module. This usually takes a bit of time, however since normally performed at the top level and is done only once per program run.
5. The build result is actually another python module which contains the optimized version of our function under the same attribute name.

Type System
===========

The type objects are used in two main spots:

* In function annotations to specify the result and argument types.
* Inside functions themselves to construct variables of that type.
