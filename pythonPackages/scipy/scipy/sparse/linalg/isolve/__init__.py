"Iterative Solvers for Sparse Linear Systems"

<<<<<<< HEAD
#from info import __doc__
from iterative import *
from minres import minres
from lgmres import lgmres
from lsqr import lsqr

__all__ = filter(lambda s:not s.startswith('_'),dir())
=======
from __future__ import division, print_function, absolute_import

#from info import __doc__
from .iterative import *
from .minres import minres
from .lgmres import lgmres
from .lsqr import lsqr
from .lsmr import lsmr

__all__ = [s for s in dir() if not s.startswith('_')]
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
from numpy.testing import Tester
test = Tester().test
bench = Tester().bench
