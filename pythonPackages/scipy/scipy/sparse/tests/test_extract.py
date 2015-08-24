"""test sparse matrix construction functions"""

<<<<<<< HEAD
from numpy.testing import *
from scipy.sparse import csr_matrix

import numpy as np
from scipy.sparse.extract import *
=======
from __future__ import division, print_function, absolute_import

from numpy.testing import TestCase, assert_equal
from scipy.sparse import csr_matrix

import numpy as np
from scipy.sparse import extract
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


class TestExtract(TestCase):
    def setUp(self):
<<<<<<< HEAD
        cases = []

        cases.append( csr_matrix( [[1,2]] ) )
        cases.append( csr_matrix( [[1,0]] ) )
        cases.append( csr_matrix( [[0,0]] ) )
        cases.append( csr_matrix( [[1],[2]] ) )
        cases.append( csr_matrix( [[1],[0]] ) )
        cases.append( csr_matrix( [[0],[0]] ) )
        cases.append( csr_matrix( [[1,2],[3,4]] ) )
        cases.append( csr_matrix( [[0,1],[0,0]] ) )
        cases.append( csr_matrix( [[0,0],[1,0]] ) )
        cases.append( csr_matrix( [[0,0],[0,0]] ) )
        cases.append( csr_matrix( [[1,2,0,0,3],[4,5,0,6,7],[0,0,8,9,0]] ) )
        cases.append( csr_matrix( [[1,2,0,0,3],[4,5,0,6,7],[0,0,8,9,0]] ).T )

        self.cases = cases

    def find(self):
        for A in self.cases:
            I,J,V = find(A)
            assert_equal( A.toarray(), csr_matrix(((I,J),V), shape=A.shape) )
=======
        self.cases = [
            csr_matrix([[1,2]]),
            csr_matrix([[1,0]]),
            csr_matrix([[0,0]]),
            csr_matrix([[1],[2]]),
            csr_matrix([[1],[0]]),
            csr_matrix([[0],[0]]),
            csr_matrix([[1,2],[3,4]]),
            csr_matrix([[0,1],[0,0]]),
            csr_matrix([[0,0],[1,0]]),
            csr_matrix([[0,0],[0,0]]),
            csr_matrix([[1,2,0,0,3],[4,5,0,6,7],[0,0,8,9,0]]),
            csr_matrix([[1,2,0,0,3],[4,5,0,6,7],[0,0,8,9,0]]).T,
        ]

    def find(self):
        for A in self.cases:
            I,J,V = extract.find(A)
            assert_equal(A.toarray(), csr_matrix(((I,J),V), shape=A.shape))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    def test_tril(self):
        for A in self.cases:
            B = A.toarray()
            for k in [-3,-2,-1,0,1,2,3]:
<<<<<<< HEAD
                assert_equal( tril(A,k=k).toarray(), np.tril(B,k=k))
=======
                assert_equal(extract.tril(A,k=k).toarray(), np.tril(B,k=k))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    def test_triu(self):
        for A in self.cases:
            B = A.toarray()
            for k in [-3,-2,-1,0,1,2,3]:
<<<<<<< HEAD
                assert_equal( triu(A,k=k).toarray(), np.triu(B,k=k))
=======
                assert_equal(extract.triu(A,k=k).toarray(), np.triu(B,k=k))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
