""" Unit tests for nonnegative least squares
Author: Uwe Schmitt
Sep 2008
"""
<<<<<<< HEAD

from numpy.testing import *
=======
from __future__ import division, print_function, absolute_import

from numpy.testing import assert_, TestCase, run_module_suite
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

from scipy.optimize import nnls
from numpy import arange, dot
from numpy.linalg import norm


class TestNNLS(TestCase):

    def test_nnls(self):
<<<<<<< HEAD
        a=arange(25.0).reshape(-1,5)
        x=arange(5.0)
        y=dot(a,x)
        x, res= nnls(a,y)
        assert res<1e-7
        assert norm(dot(a,x)-y)<1e-7
=======
        a = arange(25.0).reshape(-1,5)
        x = arange(5.0)
        y = dot(a,x)
        x, res = nnls(a,y)
        assert_(res < 1e-7)
        assert_(norm(dot(a,x)-y) < 1e-7)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

if __name__ == "__main__":
    run_module_suite()
