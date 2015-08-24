<<<<<<< HEAD
"Sparse eigenvalue solvers"

from info import __doc__

from arpack import *
from lobpcg import *

__all__ = filter(lambda s:not s.startswith('_'),dir())
=======
"""
Sparse Eigenvalue Solvers
-------------------------

The submodules of sparse.linalg.eigen:
    1. lobpcg: Locally Optimal Block Preconditioned Conjugate Gradient Method

"""
from __future__ import division, print_function, absolute_import

from .arpack import *
from .lobpcg import *

__all__ = [s for s in dir() if not s.startswith('_')]
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
from numpy.testing import Tester
test = Tester().test
bench = Tester().bench
