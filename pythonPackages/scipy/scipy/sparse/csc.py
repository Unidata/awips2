"""Compressed Sparse Column matrix format"""
<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

__docformat__ = "restructuredtext en"

__all__ = ['csc_matrix', 'isspmatrix_csc']

<<<<<<< HEAD
from warnings import warn

import numpy as np

from sparsetools import csc_tocsr
from sputils import upcast, isintlike

from compressed import _cs_matrix


class csc_matrix(_cs_matrix):
=======

import numpy as np
from scipy._lib.six import xrange

from ._sparsetools import csc_tocsr
from . import _sparsetools
from .sputils import upcast, isintlike, IndexMixin, get_index_dtype

from .compressed import _cs_matrix


class csc_matrix(_cs_matrix, IndexMixin):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    Compressed Sparse Column matrix

    This can be instantiated in several ways:
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        csc_matrix(D)
            with a dense matrix or rank-2 ndarray D

        csc_matrix(S)
            with another sparse matrix S (equivalent to S.tocsc())

        csc_matrix((M, N), [dtype])
            to construct an empty matrix with shape (M, N)
            dtype is optional, defaulting to dtype='d'.

<<<<<<< HEAD
        csc_matrix((data, ij), [shape=(M, N)])
            where ``data`` and ``ij`` satisfy the relationship
            ``a[ij[0, k], ij[1, k]] = data[k]``

        csc_matrix((data, indices, indptr), [shape=(M, N)])
            is the standard CSC representation where the row indices for
            column i are stored in ``indices[indptr[i]:indices[i+1]]``
=======
        csc_matrix((data, (row_ind, col_ind)), [shape=(M, N)])
            where ``data``, ``row_ind`` and ``col_ind`` satisfy the
            relationship ``a[row_ind[k], col_ind[k]] = data[k]``.

        csc_matrix((data, indices, indptr), [shape=(M, N)])
            is the standard CSC representation where the row indices for
            column i are stored in ``indices[indptr[i]:indptr[i+1]]``
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            and their corresponding values are stored in
            ``data[indptr[i]:indptr[i+1]]``.  If the shape parameter is
            not supplied, the matrix dimensions are inferred from
            the index arrays.

<<<<<<< HEAD
    Notes
    -----
=======
    Attributes
    ----------
    dtype : dtype
        Data type of the matrix
    shape : 2-tuple
        Shape of the matrix
    ndim : int
        Number of dimensions (this is always 2)
    nnz
        Number of nonzero elements
    data
        Data array of the matrix
    indices
        CSC format index array
    indptr
        CSC format index pointer array
    has_sorted_indices
        Whether indices are sorted

    Notes
    -----

    Sparse matrices can be used in arithmetic operations: they support
    addition, subtraction, multiplication, division, and matrix power.

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    Advantages of the CSC format
        - efficient arithmetic operations CSC + CSC, CSC * CSC, etc.
        - efficient column slicing
        - fast matrix vector products (CSR, BSR may be faster)

    Disadvantages of the CSC format
      - slow row slicing operations (consider CSR)
      - changes to the sparsity structure are expensive (consider LIL or DOK)


    Examples
<<<<<<< HEAD
    ========

    >>> from scipy.sparse import *
    >>> from scipy import *
    >>> csc_matrix( (3,4), dtype=int8 ).todense()
    matrix([[0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0]], dtype=int8)

    >>> row = array([0,2,2,0,1,2])
    >>> col = array([0,0,1,2,2,2])
    >>> data = array([1,2,3,4,5,6])
    >>> csc_matrix( (data,(row,col)), shape=(3,3) ).todense()
    matrix([[1, 0, 4],
            [0, 0, 5],
            [2, 3, 6]])

    >>> indptr = array([0,2,3,6])
    >>> indices = array([0,2,2,0,1,2])
    >>> data = array([1,2,3,4,5,6])
    >>> csc_matrix( (data,indices,indptr), shape=(3,3) ).todense()
    matrix([[1, 0, 4],
            [0, 0, 5],
            [2, 3, 6]])

    """

    def __getattr__(self, attr):
        if attr == 'rowind':
            warn("rowind attribute no longer in use. Use .indices instead",
                    DeprecationWarning)
            return self.indices
        else:
            return _cs_matrix.__getattr__(self, attr)

    def transpose(self, copy=False):
        from csr import csr_matrix
=======
    --------

    >>> import numpy as np
    >>> from scipy.sparse import csc_matrix
    >>> csc_matrix((3, 4), dtype=np.int8).toarray()
    array([[0, 0, 0, 0],
           [0, 0, 0, 0],
           [0, 0, 0, 0]], dtype=int8)

    >>> row = np.array([0, 2, 2, 0, 1, 2])
    >>> col = np.array([0, 0, 1, 2, 2, 2])
    >>> data = np.array([1, 2, 3, 4, 5, 6])
    >>> csc_matrix((data, (row, col)), shape=(3, 3)).toarray()
    array([[1, 0, 4],
           [0, 0, 5],
           [2, 3, 6]])

    >>> indptr = np.array([0, 2, 3, 6])
    >>> indices = np.array([0, 2, 2, 0, 1, 2])
    >>> data = np.array([1, 2, 3, 4, 5, 6])
    >>> csc_matrix((data, indices, indptr), shape=(3, 3)).toarray()
    array([[1, 0, 4],
           [0, 0, 5],
           [2, 3, 6]])

    """

    def transpose(self, copy=False):
        from .csr import csr_matrix
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        M,N = self.shape
        return csr_matrix((self.data,self.indices,self.indptr),(N,M),copy=copy)

    def __iter__(self):
        csr = self.tocsr()
        for r in xrange(self.shape[0]):
            yield csr[r,:]

<<<<<<< HEAD
    @np.deprecate
    def rowcol(self, ind):
        #TODO remove after 0.7
        row = self.indices[ind]
        col = np.searchsorted(self.indptr, ind+1) - 1
        return (row, col)

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def tocsc(self, copy=False):
        if copy:
            return self.copy()
        else:
            return self

    def tocsr(self):
        M,N = self.shape
<<<<<<< HEAD
        indptr  = np.empty(M + 1,    dtype=np.intc)
        indices = np.empty(self.nnz, dtype=np.intc)
        data    = np.empty(self.nnz, dtype=upcast(self.dtype))

        csc_tocsr(M, N, \
                 self.indptr, self.indices, self.data, \
                 indptr, indices, data)

        from csr import csr_matrix
=======
        idx_dtype = get_index_dtype((self.indptr, self.indices),
                                    maxval=max(self.nnz, N))
        indptr = np.empty(M + 1, dtype=idx_dtype)
        indices = np.empty(self.nnz, dtype=idx_dtype)
        data = np.empty(self.nnz, dtype=upcast(self.dtype))

        csc_tocsr(M, N,
                  self.indptr.astype(idx_dtype),
                  self.indices.astype(idx_dtype),
                  self.data,
                  indptr,
                  indices,
                  data)

        from .csr import csr_matrix
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        A = csr_matrix((data, indices, indptr), shape=self.shape)
        A.has_sorted_indices = True
        return A

<<<<<<< HEAD

    def __getitem__(self, key):
        # use CSR to implement fancy indexing
        if isinstance(key, tuple):
            row = key[0]
            col = key[1]

            if isintlike(row) or isinstance(row, slice):
                return self.T[col,row].T
            else:
                #[[1,2],??] or [[[1],[2]],??]
                if isintlike(col) or isinstance(col,slice):
                    return self.T[col,row].T
                else:
                    row = np.asarray(row, dtype=np.intc)
                    col = np.asarray(col, dtype=np.intc)
                    if len(row.shape) == 1:
                        return self.T[col,row]
                    elif len(row.shape) == 2:
                        row = row.reshape(-1)
                        col = col.reshape(-1,1)
                        return self.T[col,row].T
                    else:
                        raise NotImplementedError('unsupported indexing')

            return self.T[col,row].T
        elif isintlike(key) or isinstance(key,slice):
            return self.T[:,key].T                              #[i] or [1:2]
        else:
            return self.T[:,key].T                              #[[1,2]]

=======
    def __getitem__(self, key):
        # Use CSR to implement fancy indexing.

        row, col = self._unpack_index(key)
        # Things that return submatrices. row or col is a int or slice.
        if (isinstance(row, slice) or isinstance(col, slice) or
                isintlike(row) or isintlike(col)):
            return self.T[col, row].T
        # Things that return a sequence of values.
        else:
            return self.T[col, row]

    def nonzero(self):
        # CSC can't use _cs_matrix's .nonzero method because it
        # returns the indices sorted for self transposed.

        # Get row and col indices, from _cs_matrix.tocoo
        major_dim, minor_dim = self._swap(self.shape)
        minor_indices = self.indices
        major_indices = np.empty(len(minor_indices), dtype=self.indptr.dtype)
        _sparsetools.expandptr(major_dim, self.indptr, major_indices)
        row, col = self._swap((major_indices, minor_indices))

        # Sort them to be in C-style order
        ind = np.lexsort((col, row))
        row = row[ind]
        col = col[ind]

        return row, col

    nonzero.__doc__ = _cs_matrix.nonzero.__doc__

    def getrow(self, i):
        """Returns a copy of row i of the matrix, as a (1 x n)
        CSR matrix (row vector).
        """
        # we convert to CSR to maintain compatibility with old impl.
        # in spmatrix.getrow()
        return self._get_submatrix(i, slice(None)).tocsr()

    def getcol(self, i):
        """Returns a copy of column i of the matrix, as a (m x 1)
        CSC matrix (column vector).
        """
        return self._get_submatrix(slice(None), i)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # these functions are used by the parent class (_cs_matrix)
    # to remove redudancy between csc_matrix and csr_matrix
    def _swap(self,x):
        """swap the members of x if this is a column-oriented matrix
        """
        return (x[1],x[0])

<<<<<<< HEAD

from sputils import _isinstance

def isspmatrix_csc(x):
    return _isinstance(x, csc_matrix)
=======
def isspmatrix_csc(x):
    return isinstance(x, csc_matrix)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
