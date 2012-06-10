from setuptools import setup, find_packages, Extension
from ctypes.util import find_library
from subprocess import Popen, PIPE

CONFIG = """
VERSION = "{version}"
LIB = "{lib}"
LLC = "{bindir}/llc"
CLANG = "clang"
"""

# Determine the name for LLVM config binary
for path in ["llvm-config-3.1", "llvm-config-2.9", "llvm-config"]:
    proc = Popen(["which", path], stdout=PIPE)
    if not proc.wait():
        LLVM_CONFIG = proc.stdout.read().strip()
        break
else:
    raise OSError("Could not locate a suitable llvm-config binary")


def llvm_config(*args):
    p = Popen([LLVM_CONFIG] + list(args), stdout=PIPE)
    return p.communicate()[0].strip().split()


llvm_version = llvm_config("--version")[0]
llvm_lib = "LLVM-{0}".format(llvm_version)

with open("nos/llvm/__config__.py", "w") as config:
    config.write(CONFIG.format(version=llvm_version,
                               lib=find_library(llvm_lib),
                               bindir=llvm_config("--bindir")[0]))


# HACK this isn't really a Python extension, however it's
# a valid shared library so we'll use the available facilities.
llvm_addons = Extension(
    "nos.llvm.addons",
    ["src/llvm-addons.cc"],
    include_dirs=llvm_config("--includedir"),
    extra_compile_args=llvm_config("--cxxflags"),
    define_macros=[("NOS_LLVM_VERSION", int(llvm_version.replace(".", "")))],
    libraries=[llvm_lib],
    library_dirs=llvm_config("--libdir"),
    extra_link_args=llvm_config("--ldflags"),
)


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
      ext_modules=[llvm_addons],
      packages=find_packages(),
     )
