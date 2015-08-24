"""Regression tests for optimize.

"""
<<<<<<< HEAD

from numpy.testing import TestCase, run_module_suite, assert_almost_equal
import scipy.optimize

class TestRegression(TestCase):
    
    def test_newton_x0_is_0(self):
        """Ticket #1074"""

=======
from __future__ import division, print_function, absolute_import

import numpy as np
from numpy.testing import TestCase, run_module_suite, assert_almost_equal, \
        assert_raises

import scipy.optimize


class TestRegression(TestCase):

    def test_newton_x0_is_0(self):
        # Regression test for gh-1601
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        tgt = 1
        res = scipy.optimize.newton(lambda x: x - 1, 0)
        assert_almost_equal(res, tgt)

    def test_newton_integers(self):
<<<<<<< HEAD
        """Ticket #1214"""
        root = scipy.optimize.newton(lambda x: x**2 - 1, x0=2,
                                    fprime=lambda x: 2*x)
        assert_almost_equal(root, 1.0)
    
if __name__ == "__main__":
    run_module_suite()
=======
        # Regression test for gh-1741
        root = scipy.optimize.newton(lambda x: x**2 - 1, x0=2,
                                    fprime=lambda x: 2*x)
        assert_almost_equal(root, 1.0)

    def test_lmdif_errmsg(self):
        # This shouldn't cause a crash on Python 3
        class SomeError(Exception):
            pass
        counter = [0]

        def func(x):
            counter[0] += 1
            if counter[0] < 3:
                return x**2 - np.array([9, 10, 11])
            else:
                raise SomeError()
        assert_raises(SomeError,
                      scipy.optimize.leastsq,
                      func, [1, 2, 3])


if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
