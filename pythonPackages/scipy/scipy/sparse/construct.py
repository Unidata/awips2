"""Functions to construct sparse matrices
"""
<<<<<<< HEAD

__docformat__ = "restructuredtext en"

__all__ = [ 'spdiags', 'eye', 'identity', 'kron', 'kronsum',
            'hstack', 'vstack', 'bmat', 'rand']


from warnings import warn

import numpy as np

from sputils import upcast

from csr import csr_matrix
from csc import csc_matrix
from bsr import bsr_matrix
from coo import coo_matrix
from lil import lil_matrix
from dia import dia_matrix
=======
from __future__ import division, print_function, absolute_import

__docformat__ = "restructuredtext en"

__all__ = ['spdiags', 'eye', 'identity', 'kron', 'kronsum',
           'hstack', 'vstack', 'bmat', 'rand', 'random', 'diags', 'block_diag']


import numpy as np

from scipy._lib.six import xrange

from .sputils import upcast, get_index_dtype

from .csr import csr_matrix
from .csc import csc_matrix
from .bsr import bsr_matrix
from .coo import coo_matrix
from .dia import dia_matrix

from .base import issparse

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def spdiags(data, diags, m, n, format=None):
    """
    Return a sparse matrix from diagonals.

    Parameters
    ----------
<<<<<<< HEAD
    data   : array_like
        matrix diagonals stored row-wise
    diags  : diagonals to set
=======
    data : array_like
        matrix diagonals stored row-wise
    diags : diagonals to set
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        - k = 0  the main diagonal
        - k > 0  the k-th upper diagonal
        - k < 0  the k-th lower diagonal
    m, n : int
        shape of the result
<<<<<<< HEAD
    format : format of the result (e.g. "csr")
        By default (format=None) an appropriate sparse matrix
        format is returned.  This choice is subject to change.

    See Also
    --------
=======
    format : str, optional
        Format of the result. By default (format=None) an appropriate sparse
        matrix format is returned.  This choice is subject to change.

    See Also
    --------
    diags : more convenient form of this function
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    dia_matrix : the sparse DIAgonal format.

    Examples
    --------
<<<<<<< HEAD
    >>> data = array([[1,2,3,4],[1,2,3,4],[1,2,3,4]])
    >>> diags = array([0,-1,2])
    >>> spdiags(data, diags, 4, 4).todense()
    matrix([[1, 0, 3, 0],
            [1, 2, 0, 4],
            [0, 2, 3, 0],
            [0, 0, 3, 4]])
=======
    >>> from scipy.sparse import spdiags
    >>> data = np.array([[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]])
    >>> diags = np.array([0, -1, 2])
    >>> spdiags(data, diags, 4, 4).toarray()
    array([[1, 0, 3, 0],
           [1, 2, 0, 4],
           [0, 2, 3, 0],
           [0, 0, 3, 4]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    """
    return dia_matrix((data, diags), shape=(m,n)).asformat(format)

<<<<<<< HEAD
=======

def diags(diagonals, offsets, shape=None, format=None, dtype=None):
    """
    Construct a sparse matrix from diagonals.

    Parameters
    ----------
    diagonals : sequence of array_like
        Sequence of arrays containing the matrix diagonals,
        corresponding to `offsets`.
    offsets : sequence of int
        Diagonals to set:
          - k = 0  the main diagonal
          - k > 0  the k-th upper diagonal
          - k < 0  the k-th lower diagonal
    shape : tuple of int, optional
        Shape of the result. If omitted, a square matrix large enough
        to contain the diagonals is returned.
    format : {"dia", "csr", "csc", "lil", ...}, optional
        Matrix format of the result.  By default (format=None) an
        appropriate sparse matrix format is returned.  This choice is
        subject to change.
    dtype : dtype, optional
        Data type of the matrix.

    See Also
    --------
    spdiags : construct matrix from diagonals

    Notes
    -----
    This function differs from `spdiags` in the way it handles
    off-diagonals.

    The result from `diags` is the sparse equivalent of::

        np.diag(diagonals[0], offsets[0])
        + ...
        + np.diag(diagonals[k], offsets[k])

    Repeated diagonal offsets are disallowed.

    .. versionadded:: 0.11

    Examples
    --------
    >>> from scipy.sparse import diags
    >>> diagonals = [[1, 2, 3, 4], [1, 2, 3], [1, 2]]
    >>> diags(diagonals, [0, -1, 2]).toarray()
    array([[1, 0, 1, 0],
           [1, 2, 0, 2],
           [0, 2, 3, 0],
           [0, 0, 3, 4]])

    Broadcasting of scalars is supported (but shape needs to be
    specified):

    >>> diags([1, -2, 1], [-1, 0, 1], shape=(4, 4)).toarray()
    array([[-2.,  1.,  0.,  0.],
           [ 1., -2.,  1.,  0.],
           [ 0.,  1., -2.,  1.],
           [ 0.,  0.,  1., -2.]])


    If only one diagonal is wanted (as in `numpy.diag`), the following
    works as well:

    >>> diags([1, 2, 3], 1).toarray()
    array([[ 0.,  1.,  0.,  0.],
           [ 0.,  0.,  2.,  0.],
           [ 0.,  0.,  0.,  3.],
           [ 0.,  0.,  0.,  0.]])
    """
    # if offsets is not a sequence, assume that there's only one diagonal
    try:
        iter(offsets)
    except TypeError:
        # now check that there's actually only one diagonal
        try:
            iter(diagonals[0])
        except TypeError:
            diagonals = [np.atleast_1d(diagonals)]
        else:
            raise ValueError("Different number of diagonals and offsets.")
    else:
        diagonals = list(map(np.atleast_1d, diagonals))
    offsets = np.atleast_1d(offsets)

    # Basic check
    if len(diagonals) != len(offsets):
        raise ValueError("Different number of diagonals and offsets.")

    # Determine shape, if omitted
    if shape is None:
        m = len(diagonals[0]) + abs(int(offsets[0]))
        shape = (m, m)

    # Determine data type, if omitted
    if dtype is None:
        dtype = np.common_type(*diagonals)

    # Construct data array
    m, n = shape

    M = max([min(m + offset, n - offset) + max(0, offset)
             for offset in offsets])
    M = max(0, M)
    data_arr = np.zeros((len(offsets), M), dtype=dtype)

    for j, diagonal in enumerate(diagonals):
        offset = offsets[j]
        k = max(0, offset)
        length = min(m + offset, n - offset)
        if length <= 0:
            raise ValueError("Offset %d (index %d) out of bounds" % (offset, j))
        try:
            data_arr[j, k:k+length] = diagonal
        except ValueError:
            if len(diagonal) != length and len(diagonal) != 1:
                raise ValueError(
                    "Diagonal length (index %d: %d at offset %d) does not "
                    "agree with matrix size (%d, %d)." % (
                    j, len(diagonal), offset, m, n))
            raise

    return dia_matrix((data_arr, offsets), shape=(m, n)).asformat(format)


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def identity(n, dtype='d', format=None):
    """Identity matrix in sparse format

    Returns an identity matrix with shape (n,n) using a given
    sparse format and dtype.

    Parameters
    ----------
<<<<<<< HEAD
    n : integer
        Shape of the identity matrix.
    dtype :
        Data type of the matrix
    format : string
=======
    n : int
        Shape of the identity matrix.
    dtype : dtype, optional
        Data type of the matrix
    format : str, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Sparse format of the result, e.g. format="csr", etc.

    Examples
    --------
<<<<<<< HEAD
    >>> identity(3).todense()
    matrix([[ 1.,  0.,  0.],
            [ 0.,  1.,  0.],
            [ 0.,  0.,  1.]])
=======
    >>> from scipy.sparse import identity
    >>> identity(3).toarray()
    array([[ 1.,  0.,  0.],
           [ 0.,  1.,  0.],
           [ 0.,  0.,  1.]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> identity(3, dtype='int8', format='dia')
    <3x3 sparse matrix of type '<type 'numpy.int8'>'
            with 3 stored elements (1 diagonals) in DIAgonal format>

    """
<<<<<<< HEAD

    if format in ['csr','csc']:
        indptr  = np.arange(n+1, dtype=np.intc)
        indices = np.arange(n,   dtype=np.intc)
        data    = np.ones(n,     dtype=dtype)
        cls = eval('%s_matrix' % format)
        return cls((data,indices,indptr),(n,n))
    elif format == 'coo':
        row  = np.arange(n, dtype=np.intc)
        col  = np.arange(n, dtype=np.intc)
        data = np.ones(n, dtype=dtype)
        return coo_matrix((data,(row,col)),(n,n))
    elif format == 'dia':
        data = np.ones(n, dtype=dtype)
        diags = [0]
        return dia_matrix((data,diags), shape=(n,n))
    else:
        return identity(n, dtype=dtype, format='csr').asformat(format)


def eye(m, n, k=0, dtype='d', format=None):
    """eye(m, n) returns a sparse (m x n) matrix where the k-th diagonal
    is all ones and everything else is zeros.
    """
    m,n = int(m),int(n)
=======
    return eye(n, n, dtype=dtype, format=format)


def eye(m, n=None, k=0, dtype=float, format=None):
    """Sparse matrix with ones on diagonal

    Returns a sparse (m x n) matrix where the k-th diagonal
    is all ones and everything else is zeros.

    Parameters
    ----------
    m : int
        Number of rows in the matrix.
    n : int, optional
        Number of columns. Default: `m`.
    k : int, optional
        Diagonal to place ones on. Default: 0 (main diagonal).
    dtype : dtype, optional
        Data type of the matrix.
    format : str, optional
        Sparse format of the result, e.g. format="csr", etc.

    Examples
    --------
    >>> from scipy import sparse
    >>> sparse.eye(3).toarray()
    array([[ 1.,  0.,  0.],
           [ 0.,  1.,  0.],
           [ 0.,  0.,  1.]])
    >>> sparse.eye(3, dtype=np.int8)
    <3x3 sparse matrix of type '<type 'numpy.int8'>'
        with 3 stored elements (1 diagonals) in DIAgonal format>

    """
    if n is None:
        n = m
    m,n = int(m),int(n)

    if m == n and k == 0:
        # fast branch for special formats
        if format in ['csr', 'csc']:
            idx_dtype = get_index_dtype(maxval=n)
            indptr = np.arange(n+1, dtype=idx_dtype)
            indices = np.arange(n, dtype=idx_dtype)
            data = np.ones(n, dtype=dtype)
            cls = {'csr': csr_matrix, 'csc': csc_matrix}[format]
            return cls((data,indices,indptr),(n,n))
        elif format == 'coo':
            idx_dtype = get_index_dtype(maxval=n)
            row = np.arange(n, dtype=idx_dtype)
            col = np.arange(n, dtype=idx_dtype)
            data = np.ones(n, dtype=dtype)
            return coo_matrix((data,(row,col)),(n,n))

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    diags = np.ones((1, max(0, min(m + k, n))), dtype=dtype)
    return spdiags(diags, k, m, n).asformat(format)


def kron(A, B, format=None):
    """kronecker product of sparse matrices A and B

    Parameters
    ----------
    A : sparse or dense matrix
        first matrix of the product
    B : sparse or dense matrix
        second matrix of the product
<<<<<<< HEAD
    format : string
=======
    format : str, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        format of the result (e.g. "csr")

    Returns
    -------
    kronecker product in a sparse matrix format


    Examples
    --------
<<<<<<< HEAD
    >>> A = csr_matrix(array([[0,2],[5,0]]))
    >>> B = csr_matrix(array([[1,2],[3,4]]))
    >>> kron(A,B).todense()
    matrix([[ 0,  0,  2,  4],
            [ 0,  0,  6,  8],
            [ 5, 10,  0,  0],
            [15, 20,  0,  0]])

    >>> kron(A,[[1,2],[3,4]]).todense()
    matrix([[ 0,  0,  2,  4],
            [ 0,  0,  6,  8],
            [ 5, 10,  0,  0],
            [15, 20,  0,  0]])
=======
    >>> from scipy import sparse
    >>> A = sparse.csr_matrix(np.array([[0, 2], [5, 0]]))
    >>> B = sparse.csr_matrix(np.array([[1, 2], [3, 4]]))
    >>> sparse.kron(A, B).toarray()
    array([[ 0,  0,  2,  4],
           [ 0,  0,  6,  8],
           [ 5, 10,  0,  0],
           [15, 20,  0,  0]])

    >>> sparse.kron(A, [[1, 2], [3, 4]]).toarray()
    array([[ 0,  0,  2,  4],
           [ 0,  0,  6,  8],
           [ 5, 10,  0,  0],
           [15, 20,  0,  0]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    """
    B = coo_matrix(B)

    if (format is None or format == "bsr") and 2*B.nnz >= B.shape[0] * B.shape[1]:
<<<<<<< HEAD
        #B is fairly dense, use BSR
=======
        # B is fairly dense, use BSR
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        A = csr_matrix(A,copy=True)

        output_shape = (A.shape[0]*B.shape[0], A.shape[1]*B.shape[1])

        if A.nnz == 0 or B.nnz == 0:
            # kronecker product is the zero matrix
<<<<<<< HEAD
            return coo_matrix( output_shape )
=======
            return coo_matrix(output_shape)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        B = B.toarray()
        data = A.data.repeat(B.size).reshape(-1,B.shape[0],B.shape[1])
        data = data * B

        return bsr_matrix((data,A.indices,A.indptr), shape=output_shape)
    else:
<<<<<<< HEAD
        #use COO
=======
        # use COO
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        A = coo_matrix(A)
        output_shape = (A.shape[0]*B.shape[0], A.shape[1]*B.shape[1])

        if A.nnz == 0 or B.nnz == 0:
            # kronecker product is the zero matrix
<<<<<<< HEAD
            return coo_matrix( output_shape )

        # expand entries of a into blocks
        row  = A.row.repeat(B.nnz)
        col  = A.col.repeat(B.nnz)
=======
            return coo_matrix(output_shape)

        # expand entries of a into blocks
        row = A.row.repeat(B.nnz)
        col = A.col.repeat(B.nnz)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        data = A.data.repeat(B.nnz)

        row *= B.shape[0]
        col *= B.shape[1]

        # increment block indices
        row,col = row.reshape(-1,B.nnz),col.reshape(-1,B.nnz)
        row += B.row
        col += B.col
        row,col = row.reshape(-1),col.reshape(-1)

        # compute block entries
        data = data.reshape(-1,B.nnz) * B.data
        data = data.reshape(-1)

        return coo_matrix((data,(row,col)), shape=output_shape).asformat(format)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def kronsum(A, B, format=None):
    """kronecker sum of sparse matrices A and B

    Kronecker sum of two sparse matrices is a sum of two Kronecker
    products kron(I_n,A) + kron(B,I_m) where A has shape (m,m)
    and B has shape (n,n) and I_m and I_n are identity matrices
    of shape (m,m) and (n,n) respectively.

    Parameters
    ----------
    A
        square matrix
    B
        square matrix
<<<<<<< HEAD
    format : string
=======
    format : str
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        format of the result (e.g. "csr")

    Returns
    -------
    kronecker sum in a sparse matrix format

    Examples
    --------


    """
    A = coo_matrix(A)
    B = coo_matrix(B)

    if A.shape[0] != A.shape[1]:
        raise ValueError('A is not square')

    if B.shape[0] != B.shape[1]:
        raise ValueError('B is not square')

    dtype = upcast(A.dtype, B.dtype)

<<<<<<< HEAD
    L = kron(identity(B.shape[0],dtype=dtype), A, format=format)
    R = kron(B, identity(A.shape[0],dtype=dtype), format=format)

    return (L+R).asformat(format) #since L + R is not always same format
=======
    L = kron(eye(B.shape[0],dtype=dtype), A, format=format)
    R = kron(B, eye(A.shape[0],dtype=dtype), format=format)

    return (L+R).asformat(format)  # since L + R is not always same format


def _compressed_sparse_stack(blocks, axis):
    """
    Stacking fast path for CSR/CSC matrices
    (i) vstack for CSR, (ii) hstack for CSC.
    """
    other_axis = 1 if axis == 0 else 0
    data = np.concatenate([b.data for b in blocks])
    indices = np.concatenate([b.indices for b in blocks])
    indptr = []
    last_indptr = 0
    constant_dim = blocks[0].shape[other_axis]
    sum_dim = 0
    for b in blocks:
        if b.shape[other_axis] != constant_dim:
            raise ValueError('incompatible dimensions for axis %d' % other_axis)
        sum_dim += b.shape[axis]
        indptr.append(b.indptr[:-1] + last_indptr)
        last_indptr += b.indptr[-1]
    indptr.append([last_indptr])
    indptr = np.concatenate(indptr)
    if axis == 0:
        return csr_matrix((data, indices, indptr),
                          shape=(sum_dim, constant_dim))
    else:
        return csc_matrix((data, indices, indptr),
                          shape=(constant_dim, sum_dim))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


def hstack(blocks, format=None, dtype=None):
    """
    Stack sparse matrices horizontally (column wise)

    Parameters
    ----------
    blocks
        sequence of sparse matrices with compatible shapes
<<<<<<< HEAD
    format : string
        sparse format of the result (e.g. "csr")
        by default an appropriate sparse matrix format is returned.
        This choice is subject to change.
=======
    format : str
        sparse format of the result (e.g. "csr")
        by default an appropriate sparse matrix format is returned.
        This choice is subject to change.
    dtype : dtype, optional
        The data-type of the output matrix.  If not given, the dtype is
        determined from that of `blocks`.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    See Also
    --------
    vstack : stack sparse matrices vertically (row wise)

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.sparse import coo_matrix, vstack
    >>> A = coo_matrix([[1,2],[3,4]])
    >>> B = coo_matrix([[5],[6]])
    >>> hstack( [A,B] ).todense()
    matrix([[1, 2, 5],
            [3, 4, 6]])
=======
    >>> from scipy.sparse import coo_matrix, hstack
    >>> A = coo_matrix([[1, 2], [3, 4]])
    >>> B = coo_matrix([[5], [6]])
    >>> hstack([A,B]).toarray()
    array([[1, 2, 5],
           [3, 4, 6]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    """
    return bmat([blocks], format=format, dtype=dtype)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def vstack(blocks, format=None, dtype=None):
    """
    Stack sparse matrices vertically (row wise)

    Parameters
    ----------
    blocks
        sequence of sparse matrices with compatible shapes
<<<<<<< HEAD
    format : string
        sparse format of the result (e.g. "csr")
        by default an appropriate sparse matrix format is returned.
        This choice is subject to change.
=======
    format : str, optional
        sparse format of the result (e.g. "csr")
        by default an appropriate sparse matrix format is returned.
        This choice is subject to change.
    dtype : dtype, optional
        The data-type of the output matrix.  If not given, the dtype is
        determined from that of `blocks`.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    See Also
    --------
    hstack : stack sparse matrices horizontally (column wise)

    Examples
    --------
    >>> from scipy.sparse import coo_matrix, vstack
<<<<<<< HEAD
    >>> A = coo_matrix([[1,2],[3,4]])
    >>> B = coo_matrix([[5,6]])
    >>> vstack( [A,B] ).todense()
    matrix([[1, 2],
            [3, 4],
            [5, 6]])

    """
    return bmat([ [b] for b in blocks ], format=format, dtype=dtype)
=======
    >>> A = coo_matrix([[1, 2], [3, 4]])
    >>> B = coo_matrix([[5, 6]])
    >>> vstack([A, B]).toarray()
    array([[1, 2],
           [3, 4],
           [5, 6]])

    """
    return bmat([[b] for b in blocks], format=format, dtype=dtype)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def bmat(blocks, format=None, dtype=None):
    """
    Build a sparse matrix from sparse sub-blocks

    Parameters
    ----------
<<<<<<< HEAD
    blocks
        grid of sparse matrices with compatible shapes
        an entry of None implies an all-zero matrix
    format : sparse format of the result (e.g. "csr")
        by default an appropriate sparse matrix format is returned.
        This choice is subject to change.
=======
    blocks : array_like
        Grid of sparse matrices with compatible shapes.
        An entry of None implies an all-zero matrix.
    format : {'bsr', 'coo', 'csc', 'csr', 'dia', 'dok', 'lil'}, optional
        The sparse format of the result (e.g. "csr").  By default an
        appropriate sparse matrix format is returned.
        This choice is subject to change.
    dtype : dtype, optional
        The data-type of the output matrix.  If not given, the dtype is
        determined from that of `blocks`.

    Returns
    -------
    bmat : sparse matrix

    See Also
    --------
    block_diag, diags
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Examples
    --------
    >>> from scipy.sparse import coo_matrix, bmat
<<<<<<< HEAD
    >>> A = coo_matrix([[1,2],[3,4]])
    >>> B = coo_matrix([[5],[6]])
    >>> C = coo_matrix([[7]])
    >>> bmat( [[A,B],[None,C]] ).todense()
    matrix([[1, 2, 5],
            [3, 4, 6],
            [0, 0, 7]])

    >>> bmat( [[A,None],[None,C]] ).todense()
    matrix([[1, 2, 0],
            [3, 4, 0],
            [0, 0, 7]])
=======
    >>> A = coo_matrix([[1, 2], [3, 4]])
    >>> B = coo_matrix([[5], [6]])
    >>> C = coo_matrix([[7]])
    >>> bmat([[A, B], [None, C]]).toarray()
    array([[1, 2, 5],
           [3, 4, 6],
           [0, 0, 7]])

    >>> bmat([[A, None], [None, C]]).toarray()
    array([[1, 2, 0],
           [3, 4, 0],
           [0, 0, 7]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    """

    blocks = np.asarray(blocks, dtype='object')

<<<<<<< HEAD
    if np.rank(blocks) != 2:
        raise ValueError('blocks must have rank 2')

    M,N = blocks.shape

    block_mask   = np.zeros(blocks.shape,    dtype=np.bool)
    brow_lengths = np.zeros(blocks.shape[0], dtype=np.intc)
    bcol_lengths = np.zeros(blocks.shape[1], dtype=np.intc)
=======
    if blocks.ndim != 2:
        raise ValueError('blocks must be 2-D')

    M,N = blocks.shape

    # check for fast path cases
    if (N == 1 and format in (None, 'csr') and all(isinstance(b, csr_matrix)
                                                   for b in blocks.flat)):
        A = _compressed_sparse_stack(blocks[:,0], 0)
        if dtype is not None:
            A = A.astype(dtype)
        return A
    elif (M == 1 and format in (None, 'csc')
          and all(isinstance(b, csc_matrix) for b in blocks.flat)):
        A = _compressed_sparse_stack(blocks[0,:], 1)
        if dtype is not None:
            A = A.astype(dtype)
        return A

    block_mask = np.zeros(blocks.shape, dtype=bool)
    brow_lengths = np.zeros(M, dtype=np.int64)
    bcol_lengths = np.zeros(N, dtype=np.int64)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # convert everything to COO format
    for i in range(M):
        for j in range(N):
            if blocks[i,j] is not None:
                A = coo_matrix(blocks[i,j])
                blocks[i,j] = A
                block_mask[i,j] = True

                if brow_lengths[i] == 0:
                    brow_lengths[i] = A.shape[0]
                else:
                    if brow_lengths[i] != A.shape[0]:
                        raise ValueError('blocks[%d,:] has incompatible row dimensions' % i)

                if bcol_lengths[j] == 0:
                    bcol_lengths[j] = A.shape[1]
                else:
                    if bcol_lengths[j] != A.shape[1]:
                        raise ValueError('blocks[:,%d] has incompatible column dimensions' % j)

<<<<<<< HEAD

    # ensure that at least one value in each row and col is not None
    if brow_lengths.min() == 0:
        raise ValueError('blocks[%d,:] is all None' % brow_lengths.argmin() )
    if bcol_lengths.min() == 0:
        raise ValueError('blocks[:,%d] is all None' % bcol_lengths.argmin() )

    nnz = sum([ A.nnz for A in blocks[block_mask] ])
    if dtype is None:
        dtype = upcast( *tuple([A.dtype for A in blocks[block_mask]]) )
=======
    # ensure that at least one value in each row and col is not None
    if brow_lengths.min() == 0:
        raise ValueError('blocks[%d,:] is all None' % brow_lengths.argmin())
    if bcol_lengths.min() == 0:
        raise ValueError('blocks[:,%d] is all None' % bcol_lengths.argmin())

    nnz = sum([block.nnz for block in blocks[block_mask]])
    if dtype is None:
        dtype = upcast(*tuple([blk.dtype for blk in blocks[block_mask]]))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    row_offsets = np.concatenate(([0], np.cumsum(brow_lengths)))
    col_offsets = np.concatenate(([0], np.cumsum(bcol_lengths)))

<<<<<<< HEAD
    data = np.empty(nnz, dtype=dtype)
    row  = np.empty(nnz, dtype=np.intc)
    col  = np.empty(nnz, dtype=np.intc)
=======
    shape = (np.sum(brow_lengths), np.sum(bcol_lengths))

    data = np.empty(nnz, dtype=dtype)
    idx_dtype = get_index_dtype(maxval=max(shape))
    row = np.empty(nnz, dtype=idx_dtype)
    col = np.empty(nnz, dtype=idx_dtype)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    nnz = 0
    for i in range(M):
        for j in range(N):
            if blocks[i,j] is not None:
<<<<<<< HEAD
                A = blocks[i,j]
                data[nnz:nnz + A.nnz] = A.data
                row[nnz:nnz + A.nnz]  = A.row
                col[nnz:nnz + A.nnz]  = A.col

                row[nnz:nnz + A.nnz] += row_offsets[i]
                col[nnz:nnz + A.nnz] += col_offsets[j]

                nnz += A.nnz

    shape = (np.sum(brow_lengths), np.sum(bcol_lengths))
    return coo_matrix((data, (row, col)), shape=shape).asformat(format)

def rand(m, n, density=0.01, format="coo", dtype=None):
    """Generate a sparse matrix of the given shape and density with uniformely
=======
                B = blocks[i,j]
                data[nnz:nnz + B.nnz] = B.data
                row[nnz:nnz + B.nnz] = B.row
                col[nnz:nnz + B.nnz] = B.col

                row[nnz:nnz + B.nnz] += row_offsets[i]
                col[nnz:nnz + B.nnz] += col_offsets[j]

                nnz += B.nnz

    return coo_matrix((data, (row, col)), shape=shape).asformat(format)


def block_diag(mats, format=None, dtype=None):
    """
    Build a block diagonal sparse matrix from provided matrices.

    Parameters
    ----------
    mats : sequence of matrices
        Input matrices.
    format : str, optional
        The sparse format of the result (e.g. "csr").  If not given, the matrix
        is returned in "coo" format.
    dtype : dtype specifier, optional
        The data-type of the output matrix.  If not given, the dtype is
        determined from that of `blocks`.

    Returns
    -------
    res : sparse matrix

    Notes
    -----

    .. versionadded:: 0.11.0

    See Also
    --------
    bmat, diags

    Examples
    --------
    >>> from scipy.sparse import coo_matrix, block_diag
    >>> A = coo_matrix([[1, 2], [3, 4]])
    >>> B = coo_matrix([[5], [6]])
    >>> C = coo_matrix([[7]])
    >>> block_diag((A, B, C)).toarray()
    array([[1, 2, 0, 0],
           [3, 4, 0, 0],
           [0, 0, 5, 0],
           [0, 0, 6, 0],
           [0, 0, 0, 7]])

    """
    nmat = len(mats)
    rows = []
    for ia, a in enumerate(mats):
        row = [None]*nmat
        if issparse(a):
            row[ia] = a
        else:
            row[ia] = coo_matrix(a)
        rows.append(row)
    return bmat(rows, format=format, dtype=dtype)


def random(m, n, density=0.01, format='coo', dtype=None,
           random_state=None, data_rvs=None):
    """Generate a sparse matrix of the given shape and density with randomly
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    distributed values.

    Parameters
    ----------
<<<<<<< HEAD
    m, n: int
        shape of the matrix
    density: real
        density of the generated matrix: density equal to one means a full
        matrix, density of 0 means a matrix with no non-zero items.
    format: str
        sparse matrix format.
    dtype: dtype
        type of the returned matrix values.
=======
    m, n : int
        shape of the matrix
    density : real, optional
        density of the generated matrix: density equal to one means a full
        matrix, density of 0 means a matrix with no non-zero items.
    format : str, optional
        sparse matrix format.
    dtype : dtype, optional
        type of the returned matrix values.
    random_state : {numpy.random.RandomState, int}, optional
        Random number generator or random seed. If not given, the singleton
        numpy.random will be used.  This random state will be used
        for sampling the sparsity structure, but not necessarily for sampling
        the values of the structurally nonzero entries of the matrix.
    data_rvs : callable, optional
        Samples a requested number of random values.
        This function should take a single argument specifying the length
        of the ndarray that it will return.  The structurally nonzero entries
        of the sparse random matrix will be taken from the array sampled
        by this function.  By default, uniform [0, 1) random values will be
        sampled using the same random state as is used for sampling
        the sparsity structure.

    Examples
    --------
    >>> from scipy.sparse import random
    >>> from scipy import stats
    >>> class CustomRandomState(object):
    ...     def randint(self, k):
    ...         i = np.random.randint(k)
    ...         return i - i % 2
    >>> rs = CustomRandomState()
    >>> rvs = stats.poisson(25, loc=10).rvs
    >>> S = random(3, 4, density=0.25, random_state=rs, data_rvs=rvs)
    >>> S.A
    array([[ 36.,   0.,  33.,   0.],   # random
           [  0.,   0.,   0.,   0.],
           [  0.,   0.,  36.,   0.]])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Notes
    -----
    Only float types are supported for now.
    """
    if density < 0 or density > 1:
        raise ValueError("density expected to be 0 <= density <= 1")
<<<<<<< HEAD
    if dtype and not dtype in [np.float32, np.float64, np.longdouble]:
=======
    if dtype and (dtype not in [np.float32, np.float64, np.longdouble]):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        raise NotImplementedError("type %s not supported" % dtype)

    mn = m * n

<<<<<<< HEAD
    # XXX: sparse uses intc instead of intp...
    tp = np.intp
=======
    tp = np.intc
    if mn > np.iinfo(tp).max:
        tp = np.int64

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if mn > np.iinfo(tp).max:
        msg = """\
Trying to generate a random sparse matrix such as the product of dimensions is
greater than %d - this is not supported on this machine
"""
        raise ValueError(msg % np.iinfo(tp).max)

    # Number of non zero values
<<<<<<< HEAD
    k = long(density * m * n)

    # Generate a few more values than k so that we can get unique values
    # afterwards.
    # XXX: one could be smarter here
    mlow = 5
    fac = 1.02
    gk = min(k + mlow, fac * k)

    def _gen_unique_rand(_gk):
        id = np.random.rand(_gk)
        return np.unique(np.floor(id * mn))[:k]

    id = _gen_unique_rand(gk)
    while id.size < k:
        gk *= 1.05
        id = _gen_unique_rand(gk)

    j = np.floor(id * 1. / m).astype(tp)
    i = (id - j * m).astype(tp)
    vals = np.random.rand(k).astype(dtype)
    return coo_matrix((vals, (i, j)), shape=(m, n)).asformat(format)

#################################
# Deprecated functions
################################

__all__ += [ 'speye','spidentity', 'spkron', 'lil_eye', 'lil_diags' ]

spkron = np.deprecate(kron, old_name='spkron', new_name='scipy.sparse.kron')
speye = np.deprecate(eye, old_name='speye', new_name='scipy.sparse.eye')
spidentity = np.deprecate(identity, old_name='spidentity',
                                    new_name='scipy.sparse.identity')


def lil_eye((r,c), k=0, dtype='d'):
    """Generate a lil_matrix of dimensions (r,c) with the k-th
    diagonal set to 1.

    Parameters
    ----------

    r,c : int
        row and column-dimensions of the output.
    k : int
        - diagonal offset.  In the output matrix,
        - out[m,m+k] == 1 for all m.
    dtype : dtype
        data-type of the output array.

    """
    warn("lil_eye is deprecated." \
            "use scipy.sparse.eye(r, c, k, format='lil') instead", \
            DeprecationWarning)
    return eye(r, c, k, dtype=dtype, format='lil')


#TODO remove this function
def lil_diags(diags, offsets, (m,n), dtype='d'):
    """
    Generate a lil_matrix with the given diagonals.

    Parameters
    ----------
    diags : list of list of values e.g. [[1,2,3],[4,5]]
        values to be placed on each indicated diagonal.
    offsets : list of ints
        diagonal offsets.  This indicates the diagonal on which
        the given values should be placed.
    (r,c) : tuple of ints
        row and column dimensions of the output.
    dtype : dtype
        output data-type.

    Examples
    --------

    >>> lil_diags([[1,2,3],[4,5],[6]],[0,1,2],(3,3)).todense()
    matrix([[ 1.,  4.,  6.],
            [ 0.,  2.,  5.],
            [ 0.,  0.,  3.]])

    """
    offsets_unsorted = list(offsets)
    diags_unsorted = list(diags)
    if len(diags) != len(offsets):
        raise ValueError("Number of diagonals provided should "
                         "agree with offsets.")

    sort_indices = np.argsort(offsets_unsorted)
    diags = [diags_unsorted[k] for k in sort_indices]
    offsets = [offsets_unsorted[k] for k in sort_indices]

    for i,k in enumerate(offsets):
        if len(diags[i]) < m-abs(k):
            raise ValueError("Not enough values specified to fill "
                             "diagonal %s." % k)

    out = lil_matrix((m,n),dtype=dtype)

    from itertools import izip
    for k,diag in izip(offsets,diags):
        for ix,c in enumerate(xrange(np.clip(k,0,n),np.clip(m+k,0,n))):
            out.rows[c-k].append(c)
            out.data[c-k].append(diag[ix])
    return out
=======
    k = int(density * m * n)

    if random_state is None:
        random_state = np.random
    elif isinstance(random_state, (int, np.integer)):
        random_state = np.random.RandomState(random_state)
    if data_rvs is None:
        data_rvs = random_state.rand

    # Use the algorithm from python's random.sample for k < mn/3.
    if mn < 3*k:
        # We should use this line, but choice is only available in numpy >= 1.7
        # ind = random_state.choice(mn, size=k, replace=False)
        ind = random_state.permutation(mn)[:k]
    else:
        ind = np.empty(k, dtype=tp)
        selected = set()
        for i in xrange(k):
            j = random_state.randint(mn)
            while j in selected:
                j = random_state.randint(mn)
            selected.add(j)
            ind[i] = j

    j = np.floor(ind * 1. / m).astype(tp)
    i = (ind - j * m).astype(tp)
    vals = data_rvs(k).astype(dtype)
    return coo_matrix((vals, (i, j)), shape=(m, n)).asformat(format)


def rand(m, n, density=0.01, format="coo", dtype=None, random_state=None):
    """Generate a sparse matrix of the given shape and density with uniformly
    distributed values.

    Parameters
    ----------
    m, n : int
        shape of the matrix
    density : real, optional
        density of the generated matrix: density equal to one means a full
        matrix, density of 0 means a matrix with no non-zero items.
    format : str, optional
        sparse matrix format.
    dtype : dtype, optional
        type of the returned matrix values.
    random_state : {numpy.random.RandomState, int}, optional
        Random number generator or random seed. If not given, the singleton
        numpy.random will be used.

    Notes
    -----
    Only float types are supported for now.

    """
    return random(m, n, density, format, dtype, random_state)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
