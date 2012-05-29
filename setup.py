from setuptools import setup, find_packages
import os

os.environ.setdefault("NOS_LLVM_VERSION", "3.1")


def build_llvm_addons():
    from subprocess import call
    from distutils.errors import DistutilsExecError

    MAKEFILE = """
LDFLAGS=$(python-config --ldflags)
CFLAGS=$(python-config --cflags)

all: {root}/nos/llvm/addons.so

{root}/nos/llvm/addons.so: {root}/src/llvm-addons.cc
\tclang -shared -fPIC -D__STDC_LIMIT_MACROS=1 -D__STDC_CONSTANT_MACROS=1 \
\t-o {root}/nos/llvm/addons.so ${{CFLAGS}} ${{LDFLAGS}} -lLLVM-{version} -L/usr/lib64/llvm \
\t{root}/src/llvm-addons.cc

"""
    if not os.path.isdir("build"):
        os.makedirs("build")

    with open("build/Makefile", "w") as makefile:
        makefile.write(MAKEFILE.format(root=os.path.realpath(os.getcwd()),
                                       version=os.environ["NOS_LLVM_VERSION"]))

    status = call(("make", "-f", "build/Makefile"))
    if status != 0:
        raise DistutilsExecError("Make returned {0}".format(status))


build_llvm_addons()

setup(name="nos",
      version="0.1.0",
      description="Run-time LLVM-based compiler for CPython functions",
      author="Dimitri Tcaciuc",
      author_email="dtcaciuc@gmail.com",
      url="https://github.com/dtcaciuc/nos",
      license="MIT License",
      classifiers=["Development Status :: 3 - Alpha",
                   "Intended Audience :: Developers",
                   "Natural Language :: English",
                   "Programming Language :: Python :: 2.7",
                   "Topic :: Software Development :: Libraries"],
      install_requires=["nose", "unittest2"],
      packages=find_packages())
