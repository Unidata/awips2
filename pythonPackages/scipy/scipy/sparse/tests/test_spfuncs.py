<<<<<<< HEAD
from numpy import array, kron, matrix, diag
from numpy.testing import *

from scipy.sparse.spfuncs import *
from scipy.sparse import csr_matrix, csc_matrix, bsr_matrix
from scipy.sparse.sparsetools import csr_scale_rows, csr_scale_columns, \
        bsr_scale_rows, bsr_scale_columns

=======
from __future__ import division, print_function, absolute_import

from numpy import array, kron, matrix, diag
from numpy.testing import TestCase, run_module_suite, assert_, assert_equal

from scipy.sparse import spfuncs
from scipy.sparse import csr_matrix, csc_matrix, bsr_matrix
from scipy.sparse._sparsetools import csr_scale_rows, csr_scale_columns, \
        bsr_scale_rows, bsr_scale_columns


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class TestSparseFunctions(TestCase):
    def test_scale_rows_and_cols(self):
        D = matrix([[1,0,0,2,3],
                    [0,4,0,5,0],
                    [0,0,6,7,0]])

<<<<<<< HEAD

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        #TODO expose through function
        S = csr_matrix(D)
        v = array([1,2,3])
        csr_scale_rows(3,5,S.indptr,S.indices,S.data,v)
<<<<<<< HEAD
        assert_equal(S.todense(), diag(v)*D )
=======
        assert_equal(S.todense(), diag(v)*D)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        S = csr_matrix(D)
        v = array([1,2,3,4,5])
        csr_scale_columns(3,5,S.indptr,S.indices,S.data,v)
<<<<<<< HEAD
        assert_equal(S.todense(), D*diag(v) )
=======
        assert_equal(S.todense(), D*diag(v))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # blocks
        E = kron(D,[[1,2],[3,4]])
        S = bsr_matrix(E,blocksize=(2,2))
        v = array([1,2,3,4,5,6])
        bsr_scale_rows(3,5,2,2,S.indptr,S.indices,S.data,v)
<<<<<<< HEAD
        assert_equal(S.todense(), diag(v)*E )
=======
        assert_equal(S.todense(), diag(v)*E)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        S = bsr_matrix(E,blocksize=(2,2))
        v = array([1,2,3,4,5,6,7,8,9,10])
        bsr_scale_columns(3,5,2,2,S.indptr,S.indices,S.data,v)
<<<<<<< HEAD
        assert_equal(S.todense(), E*diag(v) )
=======
        assert_equal(S.todense(), E*diag(v))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        E = kron(D,[[1,2,3],[4,5,6]])
        S = bsr_matrix(E,blocksize=(2,3))
        v = array([1,2,3,4,5,6])
        bsr_scale_rows(3,5,2,3,S.indptr,S.indices,S.data,v)
<<<<<<< HEAD
        assert_equal(S.todense(), diag(v)*E )
=======
        assert_equal(S.todense(), diag(v)*E)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        S = bsr_matrix(E,blocksize=(2,3))
        v = array([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])
        bsr_scale_columns(3,5,2,3,S.indptr,S.indices,S.data,v)
<<<<<<< HEAD
        assert_equal(S.todense(), E*diag(v) )





    def test_estimate_blocksize(self):
        mats = []
        mats.append( [[0,1],[1,0]] )
        mats.append( [[1,1,0],[0,0,1],[1,0,1]] )
        mats.append( [[0],[0],[1]] )
        mats = [array(x) for x in mats]

        blks = []
        blks.append( [[1]] )
        blks.append( [[1,1],[1,1]] )
        blks.append( [[1,1],[0,1]] )
        blks.append( [[1,1,0],[1,0,1],[1,1,1]] )
=======
        assert_equal(S.todense(), E*diag(v))

    def test_estimate_blocksize(self):
        mats = []
        mats.append([[0,1],[1,0]])
        mats.append([[1,1,0],[0,0,1],[1,0,1]])
        mats.append([[0],[0],[1]])
        mats = [array(x) for x in mats]

        blks = []
        blks.append([[1]])
        blks.append([[1,1],[1,1]])
        blks.append([[1,1],[0,1]])
        blks.append([[1,1,0],[1,0,1],[1,1,1]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        blks = [array(x) for x in blks]

        for A in mats:
            for B in blks:
                X = kron(A,B)
<<<<<<< HEAD
                r,c = estimate_blocksize(X)
                assert(r >= B.shape[0])
                assert(c >= B.shape[1])
=======
                r,c = spfuncs.estimate_blocksize(X)
                assert_(r >= B.shape[0])
                assert_(c >= B.shape[1])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    def test_count_blocks(self):
        def gold(A,bs):
            R,C = bs
            I,J = A.nonzero()
<<<<<<< HEAD
            return len( set( zip(I/R,J/C) ) )

        mats = []
        mats.append( [[0]] )
        mats.append( [[1]] )
        mats.append( [[1,0]] )
        mats.append( [[1,1]] )
        mats.append( [[0,1],[1,0]] )
        mats.append( [[1,1,0],[0,0,1],[1,0,1]] )
        mats.append( [[0],[0],[1]] )
=======
            return len(set(zip(I//R,J//C)))

        mats = []
        mats.append([[0]])
        mats.append([[1]])
        mats.append([[1,0]])
        mats.append([[1,1]])
        mats.append([[0,1],[1,0]])
        mats.append([[1,1,0],[0,0,1],[1,0,1]])
        mats.append([[0],[0],[1]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        for A in mats:
            for B in mats:
                X = kron(A,B)
                Y = csr_matrix(X)
                for R in range(1,6):
                    for C in range(1,6):
<<<<<<< HEAD
                        assert_equal(count_blocks(Y,(R,C)),gold(X,(R,C)))

        X = kron([[1,1,0],[0,0,1],[1,0,1]],[[1,1]])
        Y = csc_matrix(X)
        assert_equal(count_blocks(X,(1,2)),gold(X,(1,2)))
        assert_equal(count_blocks(Y,(1,2)),gold(X,(1,2)))

=======
                        assert_equal(spfuncs.count_blocks(Y, (R, C)), gold(X, (R, C)))

        X = kron([[1,1,0],[0,0,1],[1,0,1]],[[1,1]])
        Y = csc_matrix(X)
        assert_equal(spfuncs.count_blocks(X, (1, 2)), gold(X, (1, 2)))
        assert_equal(spfuncs.count_blocks(Y, (1, 2)), gold(X, (1, 2)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

if __name__ == "__main__":
    run_module_suite()
