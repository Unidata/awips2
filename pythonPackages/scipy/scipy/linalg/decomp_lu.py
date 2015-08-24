"""LU decomposition functions."""

<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
from warnings import warn

from numpy import asarray, asarray_chkfinite

# Local imports
<<<<<<< HEAD
from misc import _datanotshared
from lapack import get_lapack_funcs
from flinalg import get_flinalg_funcs


def lu_factor(a, overwrite_a=False):
    """Compute pivoted LU decomposition of a matrix.
=======
from .misc import _datacopied
from .lapack import get_lapack_funcs
from .flinalg import get_flinalg_funcs

__all__ = ['lu', 'lu_solve', 'lu_factor']


def lu_factor(a, overwrite_a=False, check_finite=True):
    """
    Compute pivoted LU decomposition of a matrix.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    The decomposition is::

        A = P L U

    where P is a permutation matrix, L lower triangular with unit
    diagonal elements, and U upper triangular.

    Parameters
    ----------
<<<<<<< HEAD
    a : array, shape (M, M)
        Matrix to decompose
    overwrite_a : boolean
        Whether to overwrite data in A (may increase performance)

    Returns
    -------
    lu : array, shape (N, N)
        Matrix containing U in its upper triangle, and L in its lower triangle.
        The unit diagonal elements of L are not stored.
    piv : array, shape (N,)
=======
    a : (M, M) array_like
        Matrix to decompose
    overwrite_a : bool, optional
        Whether to overwrite data in A (may increase performance)
    check_finite : bool, optional
        Whether to check that the input matrix contains only finite numbers.
        Disabling may give a performance gain, but may result in problems
        (crashes, non-termination) if the inputs do contain infinities or NaNs.

    Returns
    -------
    lu : (N, N) ndarray
        Matrix containing U in its upper triangle, and L in its lower triangle.
        The unit diagonal elements of L are not stored.
    piv : (N,) ndarray
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Pivot indices representing the permutation matrix P:
        row i of matrix was interchanged with row piv[i].

    See also
    --------
    lu_solve : solve an equation system using the LU factorization of a matrix

    Notes
    -----
<<<<<<< HEAD
    This is a wrapper to the *GETRF routines from LAPACK.

    """
    a1 = asarray(a)
    if len(a1.shape) != 2 or (a1.shape[0] != a1.shape[1]):
        raise ValueError, 'expected square matrix'
    overwrite_a = overwrite_a or (_datanotshared(a1, a))
    getrf, = get_lapack_funcs(('getrf',), (a1,))
    lu, piv, info = getrf(a, overwrite_a=overwrite_a)
=======
    This is a wrapper to the ``*GETRF`` routines from LAPACK.

    """
    if check_finite:
        a1 = asarray_chkfinite(a)
    else:
        a1 = asarray(a)
    if len(a1.shape) != 2 or (a1.shape[0] != a1.shape[1]):
        raise ValueError('expected square matrix')
    overwrite_a = overwrite_a or (_datacopied(a1, a))
    getrf, = get_lapack_funcs(('getrf',), (a1,))
    lu, piv, info = getrf(a1, overwrite_a=overwrite_a)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if info < 0:
        raise ValueError('illegal value in %d-th argument of '
                                'internal getrf (lu_factor)' % -info)
    if info > 0:
        warn("Diagonal number %d is exactly zero. Singular matrix." % info,
                    RuntimeWarning)
    return lu, piv


<<<<<<< HEAD
def lu_solve((lu, piv), b, trans=0, overwrite_b=False):
=======
def lu_solve(lu_and_piv, b, trans=0, overwrite_b=False, check_finite=True):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Solve an equation system, a x = b, given the LU factorization of a

    Parameters
    ----------
    (lu, piv)
        Factorization of the coefficient matrix a, as given by lu_factor
    b : array
        Right-hand side
<<<<<<< HEAD
    trans : {0, 1, 2}
=======
    trans : {0, 1, 2}, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Type of system to solve:

        =====  =========
        trans  system
        =====  =========
        0      a x   = b
        1      a^T x = b
        2      a^H x = b
        =====  =========
<<<<<<< HEAD
=======
    overwrite_b : bool, optional
        Whether to overwrite data in b (may increase performance)
    check_finite : bool, optional
        Whether to check that the input matrices contain only finite numbers.
        Disabling may give a performance gain, but may result in problems
        (crashes, non-termination) if the inputs do contain infinities or NaNs.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Returns
    -------
    x : array
        Solution to the system

    See also
    --------
    lu_factor : LU factorize a matrix

    """
<<<<<<< HEAD
    b1 = asarray_chkfinite(b)
    overwrite_b = overwrite_b or (b1 is not b and not hasattr(b, '__array__'))
=======
    (lu, piv) = lu_and_piv
    if check_finite:
        b1 = asarray_chkfinite(b)
    else:
        b1 = asarray(b)
    overwrite_b = overwrite_b or _datacopied(b1, b)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if lu.shape[0] != b1.shape[0]:
        raise ValueError("incompatible dimensions.")

    getrs, = get_lapack_funcs(('getrs',), (lu, b1))
    x,info = getrs(lu, piv, b1, trans=trans, overwrite_b=overwrite_b)
    if info == 0:
        return x
    raise ValueError('illegal value in %d-th argument of internal gesv|posv'
                                                                    % -info)


<<<<<<< HEAD
def lu(a, permute_l=False, overwrite_a=False):
    """Compute pivoted LU decompostion of a matrix.
=======
def lu(a, permute_l=False, overwrite_a=False, check_finite=True):
    """
    Compute pivoted LU decomposition of a matrix.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    The decomposition is::

        A = P L U

    where P is a permutation matrix, L lower triangular with unit
    diagonal elements, and U upper triangular.

    Parameters
    ----------
<<<<<<< HEAD
    a : array, shape (M, N)
        Array to decompose
    permute_l : boolean
        Perform the multiplication P*L  (Default: do not permute)
    overwrite_a : boolean
        Whether to overwrite data in a (may improve performance)

    Returns
    -------
    (If permute_l == False)
    p : array, shape (M, M)
        Permutation matrix
    l : array, shape (M, K)
        Lower triangular or trapezoidal matrix with unit diagonal.
        K = min(M, N)
    u : array, shape (K, N)
        Upper triangular or trapezoidal matrix

    (If permute_l == True)
    pl : array, shape (M, K)
        Permuted L matrix.
        K = min(M, N)
    u : array, shape (K, N)
=======
    a : (M, N) array_like
        Array to decompose
    permute_l : bool, optional
        Perform the multiplication P*L  (Default: do not permute)
    overwrite_a : bool, optional
        Whether to overwrite data in a (may improve performance)
    check_finite : bool, optional
        Whether to check that the input matrix contains only finite numbers.
        Disabling may give a performance gain, but may result in problems
        (crashes, non-termination) if the inputs do contain infinities or NaNs.

    Returns
    -------
    **(If permute_l == False)**

    p : (M, M) ndarray
        Permutation matrix
    l : (M, K) ndarray
        Lower triangular or trapezoidal matrix with unit diagonal.
        K = min(M, N)
    u : (K, N) ndarray
        Upper triangular or trapezoidal matrix

    **(If permute_l == True)**

    pl : (M, K) ndarray
        Permuted L matrix.
        K = min(M, N)
    u : (K, N) ndarray
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Upper triangular or trapezoidal matrix

    Notes
    -----
    This is a LU factorization routine written for Scipy.

    """
<<<<<<< HEAD
    a1 = asarray_chkfinite(a)
    if len(a1.shape) != 2:
        raise ValueError('expected matrix')
    overwrite_a = overwrite_a or (_datanotshared(a1, a))
=======
    if check_finite:
        a1 = asarray_chkfinite(a)
    else:
        a1 = asarray(a)
    if len(a1.shape) != 2:
        raise ValueError('expected matrix')
    overwrite_a = overwrite_a or (_datacopied(a1, a))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    flu, = get_flinalg_funcs(('lu',), (a1,))
    p, l, u, info = flu(a1, permute_l=permute_l, overwrite_a=overwrite_a)
    if info < 0:
        raise ValueError('illegal value in %d-th argument of '
                                            'internal lu.getrf' % -info)
    if permute_l:
        return l, u
    return p, l, u
