<<<<<<< HEAD
from warnings import warn

from numpy import asarray
from scipy.sparse import isspmatrix_csc, isspmatrix_csr, isspmatrix, \
        SparseEfficiencyWarning, csc_matrix

import _superlu
=======
from __future__ import division, print_function, absolute_import

from warnings import warn

import numpy as np
from numpy import asarray, empty, ravel, nonzero
from scipy.sparse import (isspmatrix_csc, isspmatrix_csr, isspmatrix,
                          SparseEfficiencyWarning, csc_matrix)

from . import _superlu
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

noScikit = False
try:
    import scikits.umfpack as umfpack
except ImportError:
<<<<<<< HEAD
    import umfpack
    noScikit = True

isUmfpack = hasattr( umfpack, 'UMFPACK_OK' )

useUmfpack = True


__all__ = [ 'use_solver', 'spsolve', 'splu', 'spilu', 'factorized' ]

def use_solver( **kwargs ):
    """
    Valid keyword arguments with defaults (other ignored):
      useUmfpack = True
      assumeSortedIndices = False

    The default sparse solver is umfpack when available. This can be changed by
    passing useUmfpack = False, which then causes the always present SuperLU
    based solver to be used.

    Umfpack requires a CSR/CSC matrix to have sorted column/row indices. If
    sure that the matrix fulfills this, pass assumeSortedIndices=True
    to gain some speed.
=======
    noScikit = True

useUmfpack = not noScikit

__all__ = ['use_solver', 'spsolve', 'splu', 'spilu', 'factorized',
           'MatrixRankWarning']


class MatrixRankWarning(UserWarning):
    pass


def use_solver(**kwargs):
    """
    Select default sparse direct solver to be used.

    Parameters
    ----------
    useUmfpack : bool, optional
        Use UMFPACK over SuperLU. Has effect only if scikits.umfpack is
        installed. Default: True

    Notes
    -----
    The default sparse solver is umfpack when available
    (scikits.umfpack is installed). This can be changed by passing
    useUmfpack = False, which then causes the always present SuperLU
    based solver to be used.

    Umfpack requires a CSR/CSC matrix to have sorted column/row indices. If
    sure that the matrix fulfills this, pass ``assumeSortedIndices=True``
    to gain some speed.

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    if 'useUmfpack' in kwargs:
        globals()['useUmfpack'] = kwargs['useUmfpack']

<<<<<<< HEAD
    if isUmfpack:
        umfpack.configure( **kwargs )


def spsolve(A, b, permc_spec=None, use_umfpack=True):
    """Solve the sparse linear system Ax=b
    """
    if isspmatrix( b ):
        b = b.toarray()

    if b.ndim > 1:
        if max( b.shape ) == b.size:
            b = b.squeeze()
        else:
            raise ValueError, "rhs must be a vector (has shape %s)" % (b.shape,)

    if not (isspmatrix_csc(A) or isspmatrix_csr(A)):
        A = csc_matrix(A)
        warn('spsolve requires CSC or CSR matrix format', SparseEfficiencyWarning)

    A.sort_indices()
    A = A.asfptype()  #upcast to a floating point format

    M, N = A.shape
    if (M != N):
        raise ValueError, "matrix must be square (has shape %s)" % (M,N)
    if M != b.size:
        raise ValueError, "matrix - rhs size mismatch (%s - %s)"\
              % (A.shape, b.size)

    use_umfpack = use_umfpack and useUmfpack

    if isUmfpack and use_umfpack:
        if noScikit:
            warn( 'scipy.sparse.linalg.dsolve.umfpack will be removed,'\
                    ' install scikits.umfpack instead', DeprecationWarning )
        if A.dtype.char not in 'dD':
            raise ValueError, "convert matrix data to double, please, using"\
                  " .astype(), or set linsolve.useUmfpack = False"

        b = asarray(b, dtype=A.dtype).reshape(-1)

        family = {'d' : 'di', 'D' : 'zi'}
        umf = umfpack.UmfpackContext( family[A.dtype.char] )
        return umf.linsolve( umfpack.UMFPACK_A, A, b,
                             autoTranspose = True )

    else:
        if isspmatrix_csc(A):
            flag = 1 # CSC format
        elif isspmatrix_csr(A):
            flag = 0 # CSR format
        else:
            A = csc_matrix(A)
            flag = 1

        b = asarray(b, dtype=A.dtype)
        options = dict(ColPerm=permc_spec)
        return _superlu.gssv(N, A.nnz, A.data, A.indices, A.indptr, b, flag,
                             options=options)[0]
=======
    #TODO: pass other options to scikit


def spsolve(A, b, permc_spec=None, use_umfpack=True):
    """Solve the sparse linear system Ax=b, where b may be a vector or a matrix.

    Parameters
    ----------
    A : ndarray or sparse matrix
        The square matrix A will be converted into CSC or CSR form
    b : ndarray or sparse matrix
        The matrix or vector representing the right hand side of the equation.
        If a vector, b.size must be (n,) or (n, 1)
    permc_spec : str, optional
        How to permute the columns of the matrix for sparsity preservation.
        (default: 'COLAMD')

        - ``NATURAL``: natural ordering.
        - ``MMD_ATA``: minimum degree ordering on the structure of A^T A.
        - ``MMD_AT_PLUS_A``: minimum degree ordering on the structure of A^T+A.
        - ``COLAMD``: approximate minimum degree column ordering
    use_umfpack : bool, optional
        if True (default) then use umfpack for the solution.  This is
        only referenced if b is a vector and ``scikit-umfpack`` is installed.

    Returns
    -------
    x : ndarray or sparse matrix
        the solution of the sparse linear equation.
        If b is a vector, then x is a vector of size A.shape[1]
        If b is a matrix, then x is a matrix of size (A.shape[1], b.shape[1])

    Notes
    -----
    For solving the matrix expression AX = B, this solver assumes the resulting
    matrix X is sparse, as is often the case for very sparse inputs.  If the
    resulting X is dense, the construction of this sparse result will be
    relatively expensive.  In that case, consider converting A to a dense
    matrix and using scipy.linalg.solve or its variants.
    """
    if not (isspmatrix_csc(A) or isspmatrix_csr(A)):
        A = csc_matrix(A)
        warn('spsolve requires A be CSC or CSR matrix format',
                SparseEfficiencyWarning)

    # b is a vector only if b have shape (n,) or (n, 1)
    b_is_sparse = isspmatrix(b)
    if not b_is_sparse:
        b = asarray(b)
    b_is_vector = ((b.ndim == 1) or (b.ndim == 2 and b.shape[1] == 1))

    A.sort_indices()
    A = A.asfptype()  # upcast to a floating point format

    # validate input shapes
    M, N = A.shape
    if (M != N):
        raise ValueError("matrix must be square (has shape %s)" % ((M, N),))

    if M != b.shape[0]:
        raise ValueError("matrix - rhs dimension mismatch (%s - %s)"
                         % (A.shape, b.shape[0]))

    use_umfpack = use_umfpack and useUmfpack

    if b_is_vector and use_umfpack:
        if b_is_sparse:
            b_vec = b.toarray()
        else:
            b_vec = b
        b_vec = asarray(b_vec, dtype=A.dtype).ravel()

        if noScikit:
            raise RuntimeError('Scikits.umfpack not installed.')

        if A.dtype.char not in 'dD':
            raise ValueError("convert matrix data to double, please, using"
                  " .astype(), or set linsolve.useUmfpack = False")

        family = {'d': 'di', 'D': 'zi'}
        umf = umfpack.UmfpackContext(family[A.dtype.char])
        x = umf.linsolve(umfpack.UMFPACK_A, A, b_vec,
                         autoTranspose=True)
    else:
        if b_is_vector and b_is_sparse:
            b = b.toarray()
            b_is_sparse = False

        if not b_is_sparse:
            if isspmatrix_csc(A):
                flag = 1  # CSC format
            else:
                flag = 0  # CSR format

            options = dict(ColPerm=permc_spec)
            x, info = _superlu.gssv(N, A.nnz, A.data, A.indices, A.indptr,
                                    b, flag, options=options)
            if info != 0:
                warn("Matrix is exactly singular", MatrixRankWarning)
                x.fill(np.nan)
            if b_is_vector:
                x = x.ravel()
        else:
            # b is sparse
            Afactsolve = factorized(A)

            if not isspmatrix_csc(b):
                warn('spsolve is more efficient when sparse b '
                     'is in the CSC matrix format', SparseEfficiencyWarning)
                b = csc_matrix(b)

            # Create a sparse output matrix by repeatedly applying
            # the sparse factorization to solve columns of b.
            data_segs = []
            row_segs = []
            col_segs = []
            for j in range(b.shape[1]):
                bj = b[:, j].A.ravel()
                xj = Afactsolve(bj)
                w = np.flatnonzero(xj)
                segment_length = w.shape[0]
                row_segs.append(w)
                col_segs.append(np.ones(segment_length, dtype=int)*j)
                data_segs.append(np.asarray(xj[w], dtype=A.dtype))
            sparse_data = np.concatenate(data_segs)
            sparse_row = np.concatenate(row_segs)
            sparse_col = np.concatenate(col_segs)
            x = A.__class__((sparse_data, (sparse_row, sparse_col)),
                           shape=b.shape, dtype=A.dtype)

    return x

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def splu(A, permc_spec=None, diag_pivot_thresh=None,
         drop_tol=None, relax=None, panel_size=None, options=dict()):
    """
    Compute the LU decomposition of a sparse, square matrix.

    Parameters
    ----------
<<<<<<< HEAD
    A
        Sparse matrix to factorize. Should be in CSR or CSC format.

=======
    A : sparse matrix
        Sparse matrix to factorize. Should be in CSR or CSC format.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    permc_spec : str, optional
        How to permute the columns of the matrix for sparsity preservation.
        (default: 'COLAMD')

        - ``NATURAL``: natural ordering.
        - ``MMD_ATA``: minimum degree ordering on the structure of A^T A.
        - ``MMD_AT_PLUS_A``: minimum degree ordering on the structure of A^T+A.
        - ``COLAMD``: approximate minimum degree column ordering

    diag_pivot_thresh : float, optional
        Threshold used for a diagonal entry to be an acceptable pivot.
<<<<<<< HEAD
        See SuperLU user's guide for details [SLU]_
=======
        See SuperLU user's guide for details [1]_
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    drop_tol : float, optional
        (deprecated) No effect.
    relax : int, optional
        Expert option for customizing the degree of relaxing supernodes.
<<<<<<< HEAD
        See SuperLU user's guide for details [SLU]_
    panel_size : int, optional
        Expert option for customizing the panel size.
        See SuperLU user's guide for details [SLU]_
    options : dict, optional
        Dictionary containing additional expert options to SuperLU.
        See SuperLU user guide [SLU]_ (section 2.4 on the 'Options' argument)
=======
        See SuperLU user's guide for details [1]_
    panel_size : int, optional
        Expert option for customizing the panel size.
        See SuperLU user's guide for details [1]_
    options : dict, optional
        Dictionary containing additional expert options to SuperLU.
        See SuperLU user guide [1]_ (section 2.4 on the 'Options' argument)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        for more details. For example, you can specify
        ``options=dict(Equil=False, IterRefine='SINGLE'))``
        to turn equilibration off and perform a single iterative refinement.

    Returns
    -------
<<<<<<< HEAD
    invA : scipy.sparse.linalg.dsolve._superlu.SciPyLUType
=======
    invA : scipy.sparse.linalg.SuperLU
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Object, which has a ``solve`` method.

    See also
    --------
    spilu : incomplete LU decomposition

    Notes
    -----
    This function uses the SuperLU library.

    References
    ----------
<<<<<<< HEAD
    .. [SLU] SuperLU http://crd.lbl.gov/~xiaoye/SuperLU/
=======
    .. [1] SuperLU http://crd.lbl.gov/~xiaoye/SuperLU/
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    """

    if not isspmatrix_csc(A):
        A = csc_matrix(A)
        warn('splu requires CSC matrix format', SparseEfficiencyWarning)

    A.sort_indices()
<<<<<<< HEAD
    A = A.asfptype()  #upcast to a floating point format

    M, N = A.shape
    if (M != N):
        raise ValueError, "can only factor square matrices" #is this true?
=======
    A = A.asfptype()  # upcast to a floating point format

    M, N = A.shape
    if (M != N):
        raise ValueError("can only factor square matrices")  # is this true?
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    _options = dict(DiagPivotThresh=diag_pivot_thresh, ColPerm=permc_spec,
                    PanelSize=panel_size, Relax=relax)
    if options is not None:
        _options.update(options)
    return _superlu.gstrf(N, A.nnz, A.data, A.indices, A.indptr,
                          ilu=False, options=_options)

<<<<<<< HEAD
def spilu(A, drop_tol=None, fill_factor=None, drop_rule=None, permc_spec=None,
          diag_pivot_thresh=None, relax=None, panel_size=None, options=None):
    """
    Compute an incomplete LU decomposition for a sparse, square matrix A.

    The resulting object is an approximation to the inverse of A.

    Parameters
    ----------
    A
        Sparse matrix to factorize

=======

def spilu(A, drop_tol=None, fill_factor=None, drop_rule=None, permc_spec=None,
          diag_pivot_thresh=None, relax=None, panel_size=None, options=None):
    """
    Compute an incomplete LU decomposition for a sparse, square matrix.

    The resulting object is an approximation to the inverse of `A`.

    Parameters
    ----------
    A : (N, N) array_like
        Sparse matrix to factorize
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    drop_tol : float, optional
        Drop tolerance (0 <= tol <= 1) for an incomplete LU decomposition.
        (default: 1e-4)
    fill_factor : float, optional
        Specifies the fill ratio upper bound (>= 1.0) for ILU. (default: 10)
    drop_rule : str, optional
        Comma-separated string of drop rules to use.
        Available rules: ``basic``, ``prows``, ``column``, ``area``,
        ``secondary``, ``dynamic``, ``interp``. (Default: ``basic,area``)

        See SuperLU documentation for details.
<<<<<<< HEAD
    milu : str, optional
        Which version of modified ILU to use. (Choices: ``silu``,
        ``smilu_1``, ``smilu_2`` (default), ``smilu_3``.)
=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Remaining other options
        Same as for `splu`

    Returns
    -------
<<<<<<< HEAD
    invA_approx : scipy.sparse.linalg.dsolve._superlu.SciPyLUType
=======
    invA_approx : scipy.sparse.linalg.SuperLU
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Object, which has a ``solve`` method.

    See also
    --------
    splu : complete LU decomposition

    Notes
    -----
    To improve the better approximation to the inverse, you may need to
<<<<<<< HEAD
    increase ``fill_factor`` AND decrease ``drop_tol``.

    This function uses the SuperLU library.

    References
    ----------
    .. [SLU] SuperLU http://crd.lbl.gov/~xiaoye/SuperLU/

    """

=======
    increase `fill_factor` AND decrease `drop_tol`.

    This function uses the SuperLU library.

    """
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if not isspmatrix_csc(A):
        A = csc_matrix(A)
        warn('splu requires CSC matrix format', SparseEfficiencyWarning)

    A.sort_indices()
<<<<<<< HEAD
    A = A.asfptype()  #upcast to a floating point format

    M, N = A.shape
    if (M != N):
        raise ValueError, "can only factor square matrices" #is this true?
=======
    A = A.asfptype()  # upcast to a floating point format

    M, N = A.shape
    if (M != N):
        raise ValueError("can only factor square matrices")  # is this true?
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    _options = dict(ILU_DropRule=drop_rule, ILU_DropTol=drop_tol,
                    ILU_FillFactor=fill_factor,
                    DiagPivotThresh=diag_pivot_thresh, ColPerm=permc_spec,
                    PanelSize=panel_size, Relax=relax)
    if options is not None:
        _options.update(options)
    return _superlu.gstrf(N, A.nnz, A.data, A.indices, A.indptr,
                          ilu=True, options=_options)

<<<<<<< HEAD
def factorized( A ):
    """
    Return a fuction for solving a sparse linear system, with A pre-factorized.

    Example:
      solve = factorized( A ) # Makes LU decomposition.
      x1 = solve( rhs1 ) # Uses the LU factors.
      x2 = solve( rhs2 ) # Uses again the LU factors.
    """
    if isUmfpack and useUmfpack:
        if noScikit:
            warn( 'scipy.sparse.linalg.dsolve.umfpack will be removed,'\
                    ' install scikits.umfpack instead', DeprecationWarning )
=======

def factorized(A):
    """
    Return a fuction for solving a sparse linear system, with A pre-factorized.

    Parameters
    ----------
    A : (N, N) array_like
        Input.

    Returns
    -------
    solve : callable
        To solve the linear system of equations given in `A`, the `solve`
        callable should be passed an ndarray of shape (N,).

    Examples
    --------
    >>> from scipy.sparse.linalg import factorized
    >>> A = np.array([[ 3. ,  2. , -1. ],
    ...               [ 2. , -2. ,  4. ],
    ...               [-1. ,  0.5, -1. ]])
    >>> solve = factorized(A) # Makes LU decomposition.
    >>> rhs1 = np.array([1, -2, 0])
    >>> solve(rhs1) # Uses the LU factors.
    array([ 1., -2., -2.])

    """
    if useUmfpack:
        if noScikit:
            raise RuntimeError('Scikits.umfpack not installed.')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        if not isspmatrix_csc(A):
            A = csc_matrix(A)
            warn('splu requires CSC matrix format', SparseEfficiencyWarning)

        A.sort_indices()
<<<<<<< HEAD
        A = A.asfptype()  #upcast to a floating point format

        if A.dtype.char not in 'dD':
            raise ValueError, "convert matrix data to double, please, using"\
                  " .astype(), or set linsolve.useUmfpack = False"

        family = {'d' : 'di', 'D' : 'zi'}
        umf = umfpack.UmfpackContext( family[A.dtype.char] )

        # Make LU decomposition.
        umf.numeric( A )

        def solve( b ):
            return umf.solve( umfpack.UMFPACK_A, A, b, autoTranspose = True )

        return solve
    else:
        return splu( A ).solve
=======
        A = A.asfptype()  # upcast to a floating point format

        if A.dtype.char not in 'dD':
            raise ValueError("convert matrix data to double, please, using"
                  " .astype(), or set linsolve.useUmfpack = False")

        family = {'d': 'di', 'D': 'zi'}
        umf = umfpack.UmfpackContext(family[A.dtype.char])

        # Make LU decomposition.
        umf.numeric(A)

        def solve(b):
            return umf.solve(umfpack.UMFPACK_A, A, b, autoTranspose=True)

        return solve
    else:
        return splu(A).solve
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
