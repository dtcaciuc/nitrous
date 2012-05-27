from setuptools import setup, find_packages
import os

os.environ.setdefault("NOS_LLVM_VERSION", "3.1")


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
