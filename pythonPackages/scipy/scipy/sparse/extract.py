"""Functions to extract parts of sparse matrices
"""

<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
__docformat__ = "restructuredtext en"

__all__ = ['find', 'tril', 'triu']


<<<<<<< HEAD
from coo import coo_matrix
=======
from .coo import coo_matrix

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def find(A):
    """Return the indices and values of the nonzero elements of a matrix

    Parameters
    ----------
    A : dense or sparse matrix
        Matrix whose nonzero elements are desired.

    Returns
    -------
    (I,J,V) : tuple of arrays
        I,J, and V contain the row indices, column indices, and values
        of the nonzero matrix entries.


<<<<<<< HEAD
    Example
    -------
    >>> from scipy.sparse import csr_matrix
=======
    Examples
    --------
    >>> from scipy.sparse import csr_matrix, find
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> A = csr_matrix([[7.0, 8.0, 0],[0, 0, 9.0]])
    >>> find(A)
    (array([0, 0, 1], dtype=int32), array([0, 1, 2], dtype=int32), array([ 7.,  8.,  9.]))

    """

<<<<<<< HEAD
    A = coo_matrix(A).tocsr()  #sums duplicates
    A.eliminate_zeros()        #removes explicit zeros
    A = A.tocoo(copy=False)    #(cheaply) convert to COO
=======
    A = coo_matrix(A).tocsr()  # sums duplicates
    A.eliminate_zeros()        # removes explicit zeros
    A = A.tocoo(copy=False)    # (cheaply) convert to COO
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    return A.row,A.col,A.data


<<<<<<< HEAD

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def tril(A, k=0, format=None):
    """Return the lower triangular portion of a matrix in sparse format

    Returns the elements on or below the k-th diagonal of the matrix A.
        - k = 0 corresponds to the main diagonal
        - k > 0 is above the main diagonal
        - k < 0 is below the main diagonal

    Parameters
    ----------
    A : dense or sparse matrix
        Matrix whose lower trianglar portion is desired.
    k : integer : optional
        The top-most diagonal of the lower triangle.
    format : string
        Sparse format of the result, e.g. format="csr", etc.

    Returns
    -------
    L : sparse matrix
        Lower triangular portion of A in sparse format.

    See Also
    --------
    triu : upper triangle in sparse format

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.sparse import csr_matrix
    >>> A = csr_matrix( [[1,2,0,0,3],[4,5,0,6,7],[0,0,8,9,0]], dtype='int32' )
    >>> A.todense()
    matrix([[1, 2, 0, 0, 3],
            [4, 5, 0, 6, 7],
            [0, 0, 8, 9, 0]])
    >>> tril(A).todense()
    matrix([[1, 0, 0, 0, 0],
            [4, 5, 0, 0, 0],
            [0, 0, 8, 0, 0]])
    >>> tril(A).nnz
    4
    >>> tril(A, k=1).todense()
    matrix([[1, 2, 0, 0, 0],
            [4, 5, 0, 0, 0],
            [0, 0, 8, 9, 0]])
    >>> tril(A, k=-1).todense()
    matrix([[0, 0, 0, 0, 0],
            [4, 0, 0, 0, 0],
            [0, 0, 0, 0, 0]])
=======
    >>> from scipy.sparse import csr_matrix, tril
    >>> A = csr_matrix([[1, 2, 0, 0, 3], [4, 5, 0, 6, 7], [0, 0, 8, 9, 0]],
    ...                dtype='int32')
    >>> A.toarray()
    array([[1, 2, 0, 0, 3],
           [4, 5, 0, 6, 7],
           [0, 0, 8, 9, 0]])
    >>> tril(A).toarray()
    array([[1, 0, 0, 0, 0],
           [4, 5, 0, 0, 0],
           [0, 0, 8, 0, 0]])
    >>> tril(A).nnz
    4
    >>> tril(A, k=1).toarray()
    array([[1, 2, 0, 0, 0],
           [4, 5, 0, 0, 0],
           [0, 0, 8, 9, 0]])
    >>> tril(A, k=-1).toarray()
    array([[0, 0, 0, 0, 0],
           [4, 0, 0, 0, 0],
           [0, 0, 0, 0, 0]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> tril(A, format='csc')
    <3x5 sparse matrix of type '<type 'numpy.int32'>'
            with 4 stored elements in Compressed Sparse Column format>

    """

    # convert to COOrdinate format where things are easy
    A = coo_matrix(A, copy=False)

    mask = A.row + k >= A.col

<<<<<<< HEAD
    row  = A.row[mask]
    col  = A.col[mask]
    data = A.data[mask]

    return coo_matrix( (data,(row,col)), shape=A.shape ).asformat(format)
=======
    row = A.row[mask]
    col = A.col[mask]
    data = A.data[mask]

    return coo_matrix((data,(row,col)), shape=A.shape).asformat(format)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


def triu(A, k=0, format=None):
    """Return the upper triangular portion of a matrix in sparse format

    Returns the elements on or above the k-th diagonal of the matrix A.
        - k = 0 corresponds to the main diagonal
        - k > 0 is above the main diagonal
        - k < 0 is below the main diagonal

    Parameters
    ----------
    A : dense or sparse matrix
        Matrix whose upper trianglar portion is desired.
    k : integer : optional
        The bottom-most diagonal of the upper triangle.
    format : string
        Sparse format of the result, e.g. format="csr", etc.

    Returns
    -------
    L : sparse matrix
        Upper triangular portion of A in sparse format.

    See Also
    --------
    tril : lower triangle in sparse format

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.sparse import csr_matrix
    >>> A = csr_matrix( [[1,2,0,0,3],[4,5,0,6,7],[0,0,8,9,0]], dtype='int32' )
    >>> A.todense()
    matrix([[1, 2, 0, 0, 3],
            [4, 5, 0, 6, 7],
            [0, 0, 8, 9, 0]])
    >>> triu(A).todense()
    matrix([[1, 2, 0, 0, 3],
            [0, 5, 0, 6, 7],
            [0, 0, 8, 9, 0]])
    >>> triu(A).nnz
    8
    >>> triu(A, k=1).todense()
    matrix([[0, 2, 0, 0, 3],
            [0, 0, 0, 6, 7],
            [0, 0, 0, 9, 0]])
    >>> triu(A, k=-1).todense()
    matrix([[1, 2, 0, 0, 3],
            [4, 5, 0, 6, 7],
            [0, 0, 8, 9, 0]])
=======
    >>> from scipy.sparse import csr_matrix, triu
    >>> A = csr_matrix([[1, 2, 0, 0, 3], [4, 5, 0, 6, 7], [0, 0, 8, 9, 0]],
    ...                dtype='int32')
    >>> A.toarray()
    array([[1, 2, 0, 0, 3],
           [4, 5, 0, 6, 7],
           [0, 0, 8, 9, 0]])
    >>> triu(A).toarray()
    array([[1, 2, 0, 0, 3],
           [0, 5, 0, 6, 7],
           [0, 0, 8, 9, 0]])
    >>> triu(A).nnz
    8
    >>> triu(A, k=1).toarray()
    array([[0, 2, 0, 0, 3],
           [0, 0, 0, 6, 7],
           [0, 0, 0, 9, 0]])
    >>> triu(A, k=-1).toarray()
    array([[1, 2, 0, 0, 3],
           [4, 5, 0, 6, 7],
           [0, 0, 8, 9, 0]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> triu(A, format='csc')
    <3x5 sparse matrix of type '<type 'numpy.int32'>'
            with 8 stored elements in Compressed Sparse Column format>

    """

    # convert to COOrdinate format where things are easy
    A = coo_matrix(A, copy=False)

    mask = A.row + k <= A.col

<<<<<<< HEAD
    row  = A.row[mask]
    col  = A.col[mask]
    data = A.data[mask]

    return coo_matrix( (data,(row,col)), shape=A.shape ).asformat(format)
=======
    row = A.row[mask]
    col = A.col[mask]
    data = A.data[mask]

    return coo_matrix((data,(row,col)), shape=A.shape).asformat(format)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
