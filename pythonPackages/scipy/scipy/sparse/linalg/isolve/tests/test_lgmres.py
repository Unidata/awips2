#!/usr/bin/env python
"""Tests for the linalg.isolve.lgmres module
"""

<<<<<<< HEAD
from numpy.testing import *
=======
from __future__ import division, print_function, absolute_import

from numpy.testing import TestCase, assert_, run_module_suite
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

from numpy import zeros, array, allclose
from scipy.linalg import norm
from scipy.sparse import csr_matrix

from scipy.sparse.linalg.interface import LinearOperator
from scipy.sparse.linalg import splu
from scipy.sparse.linalg.isolve import lgmres

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
Am = csr_matrix(array([[-2,1,0,0,0,9],
                       [1,-2,1,0,5,0],
                       [0,1,-2,1,0,0],
                       [0,0,1,-2,1,0],
                       [0,3,0,1,-2,1],
                       [1,0,0,0,1,-2]]))
b = array([1,2,3,4,5,6])
count = [0]
<<<<<<< HEAD
=======


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def matvec(v):
    count[0] += 1
    return Am*v
A = LinearOperator(matvec=matvec, shape=Am.shape, dtype=Am.dtype)
<<<<<<< HEAD
=======


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def do_solve(**kw):
    count[0] = 0
    x0, flag = lgmres(A, b, x0=zeros(A.shape[0]), inner_m=6, tol=1e-14, **kw)
    count_0 = count[0]
<<<<<<< HEAD
    assert allclose(A*x0, b, rtol=1e-12, atol=1e-12), norm(A*x0-b)
=======
    assert_(allclose(A*x0, b, rtol=1e-12, atol=1e-12), norm(A*x0-b))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return x0, count_0


class TestLGMRES(TestCase):
    def test_preconditioner(self):
        # Check that preconditioning works
        pc = splu(Am.tocsc())
        M = LinearOperator(matvec=pc.solve, shape=A.shape, dtype=A.dtype)

        x0, count_0 = do_solve()
        x1, count_1 = do_solve(M=M)

<<<<<<< HEAD
        assert count_1 == 3
        assert count_1 < count_0/2
        assert allclose(x1, x0, rtol=1e-14)

    def test_outer_v(self):
        # Check that the augmentation vectors behave as expected
        
        outer_v = []
        x0, count_0 = do_solve(outer_k=6, outer_v=outer_v)
        assert len(outer_v) > 0
        assert len(outer_v) <= 6

        x1, count_1 = do_solve(outer_k=6, outer_v=outer_v)
        assert count_1 == 2, count_1
        assert count_1 < count_0/2
        assert allclose(x1, x0, rtol=1e-14)
=======
        assert_(count_1 == 3)
        assert_(count_1 < count_0/2)
        assert_(allclose(x1, x0, rtol=1e-14))

    def test_outer_v(self):
        # Check that the augmentation vectors behave as expected

        outer_v = []
        x0, count_0 = do_solve(outer_k=6, outer_v=outer_v)
        assert_(len(outer_v) > 0)
        assert_(len(outer_v) <= 6)

        x1, count_1 = do_solve(outer_k=6, outer_v=outer_v)
        assert_(count_1 == 2, count_1)
        assert_(count_1 < count_0/2)
        assert_(allclose(x1, x0, rtol=1e-14))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # ---

        outer_v = []
        x0, count_0 = do_solve(outer_k=6, outer_v=outer_v, store_outer_Av=False)
<<<<<<< HEAD
        assert array([v[1] is None for v in outer_v]).all()
        assert len(outer_v) > 0
        assert len(outer_v) <= 6

        x1, count_1 = do_solve(outer_k=6, outer_v=outer_v)
        assert count_1 == 3, count_1
        assert count_1 < count_0/2
        assert allclose(x1, x0, rtol=1e-14)

if __name__ == "__main__":
    import nose
    nose.run(argv=['', __file__])
=======
        assert_(array([v[1] is None for v in outer_v]).all())
        assert_(len(outer_v) > 0)
        assert_(len(outer_v) <= 6)

        x1, count_1 = do_solve(outer_k=6, outer_v=outer_v)
        assert_(count_1 == 3, count_1)
        assert_(count_1 < count_0/2)
        assert_(allclose(x1, x0, rtol=1e-14))


if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
