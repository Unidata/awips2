""" Functions that operate on sparse matrices
"""

<<<<<<< HEAD
__all__ = ['count_blocks','estimate_blocksize']

from csr import isspmatrix_csr, csr_matrix
from csc import isspmatrix_csc
from sparsetools import csr_count_blocks
=======
from __future__ import division, print_function, absolute_import

__all__ = ['count_blocks','estimate_blocksize']

from .csr import isspmatrix_csr, csr_matrix
from .csc import isspmatrix_csc
from ._sparsetools import csr_count_blocks

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def extract_diagonal(A):
    raise NotImplementedError('use .diagonal() instead')

#def extract_diagonal(A):
#    """extract_diagonal(A) returns the main diagonal of A."""
#    #TODO extract k-th diagonal
#    if isspmatrix_csr(A) or isspmatrix_csc(A):
#        fn = getattr(sparsetools, A.format + "_diagonal")
#        y = empty( min(A.shape), dtype=upcast(A.dtype) )
#        fn(A.shape[0],A.shape[1],A.indptr,A.indices,A.data,y)
#        return y
#    elif isspmatrix_bsr(A):
#        M,N = A.shape
#        R,C = A.blocksize
#        y = empty( min(M,N), dtype=upcast(A.dtype) )
<<<<<<< HEAD
#        fn = sparsetools.bsr_diagonal(M/R, N/C, R, C, \
=======
#        fn = sparsetools.bsr_diagonal(M//R, N//C, R, C, \
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#                A.indptr, A.indices, ravel(A.data), y)
#        return y
#    else:
#        return extract_diagonal(csr_matrix(A))

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def estimate_blocksize(A,efficiency=0.7):
    """Attempt to determine the blocksize of a sparse matrix

    Returns a blocksize=(r,c) such that
        - A.nnz / A.tobsr( (r,c) ).nnz > efficiency
    """
    if not (isspmatrix_csr(A) or isspmatrix_csc(A)):
        A = csr_matrix(A)

    if A.nnz == 0:
        return (1,1)

    if not 0 < efficiency < 1.0:
<<<<<<< HEAD
        raise ValueError,'efficiency must satisfy 0.0 < efficiency < 1.0'
=======
        raise ValueError('efficiency must satisfy 0.0 < efficiency < 1.0')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    high_efficiency = (1.0 + efficiency) / 2.0
    nnz = float(A.nnz)
    M,N = A.shape

    if M % 2 == 0 and N % 2 == 0:
<<<<<<< HEAD
        e22 = nnz / ( 4 * count_blocks(A,(2,2)) )
=======
        e22 = nnz / (4 * count_blocks(A,(2,2)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    else:
        e22 = 0.0

    if M % 3 == 0 and N % 3 == 0:
<<<<<<< HEAD
        e33 = nnz / ( 9 * count_blocks(A,(3,3)) )
    else:
        e33 = 0.0


    if e22 > high_efficiency and e33 > high_efficiency:
        e66 = nnz / ( 36 * count_blocks(A,(6,6)) )
=======
        e33 = nnz / (9 * count_blocks(A,(3,3)))
    else:
        e33 = 0.0

    if e22 > high_efficiency and e33 > high_efficiency:
        e66 = nnz / (36 * count_blocks(A,(6,6)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        if e66 > efficiency:
            return (6,6)
        else:
            return (3,3)
    else:
        if M % 4 == 0 and N % 4 == 0:
<<<<<<< HEAD
            e44 = nnz / ( 16 * count_blocks(A,(4,4)) )
=======
            e44 = nnz / (16 * count_blocks(A,(4,4)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        else:
            e44 = 0.0

        if e44 > efficiency:
            return (4,4)
        elif e33 > efficiency:
            return (3,3)
        elif e22 > efficiency:
            return (2,2)
        else:
            return (1,1)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def count_blocks(A,blocksize):
    """For a given blocksize=(r,c) count the number of occupied
    blocks in a sparse matrix A
    """
    r,c = blocksize
    if r < 1 or c < 1:
<<<<<<< HEAD
        raise ValueError,'r and c must be positive'
=======
        raise ValueError('r and c must be positive')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    if isspmatrix_csr(A):
        M,N = A.shape
        return csr_count_blocks(M,N,r,c,A.indptr,A.indices)
    elif isspmatrix_csc(A):
        return count_blocks(A.T,(c,r))
    else:
        return count_blocks(csr_matrix(A),blocksize)
