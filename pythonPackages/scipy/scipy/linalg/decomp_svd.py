"""SVD decomposition functions."""
<<<<<<< HEAD

import numpy
from numpy import asarray_chkfinite, zeros, r_, diag
from scipy.linalg import calc_lwork

# Local imports.
from misc import LinAlgError, _datanotshared
from lapack import get_lapack_funcs


def svd(a, full_matrices=True, compute_uv=True, overwrite_a=False):
    """Singular Value Decomposition.

    Factorizes the matrix a into two unitary matrices U and Vh and
    an 1d-array s of singular values (real, non-negative) such that
    a == U S Vh  if S is an suitably shaped matrix of zeros whose
    main diagonal is s.

    Parameters
    ----------
    a : array, shape (M, N)
        Matrix to decompose
    full_matrices : boolean
        If true,  U, Vh are shaped  (M,M), (N,N)
        If false, the shapes are    (M,K), (K,N) where K = min(M,N)
    compute_uv : boolean
        Whether to compute also U, Vh in addition to s (Default: true)
    overwrite_a : boolean
        Whether data in a is overwritten (may improve performance)

    Returns
    -------
    U:  array, shape (M,M) or (M,K) depending on full_matrices
    s:  array, shape (K,)
        The singular values, sorted so that s[i] >= s[i+1]. K = min(M, N)
    Vh: array, shape (N,N) or (K,N) depending on full_matrices

    For compute_uv = False, only s is returned.

    Raises LinAlgError if SVD computation does not converge

    Examples
    --------
    >>> from scipy import random, linalg, allclose, dot
    >>> a = random.randn(9, 6) + 1j*random.randn(9, 6)
=======
from __future__ import division, print_function, absolute_import

import numpy
from numpy import asarray_chkfinite, asarray, zeros, r_, diag

# Local imports.
from .misc import LinAlgError, _datacopied
from .lapack import get_lapack_funcs
from .decomp import _asarray_validated

__all__ = ['svd', 'svdvals', 'diagsvd', 'orth']


def svd(a, full_matrices=True, compute_uv=True, overwrite_a=False,
        check_finite=True):
    """
    Singular Value Decomposition.

    Factorizes the matrix a into two unitary matrices U and Vh, and
    a 1-D array s of singular values (real, non-negative) such that
    ``a == U*S*Vh``, where S is a suitably shaped matrix of zeros with
    main diagonal s.

    Parameters
    ----------
    a : (M, N) array_like
        Matrix to decompose.
    full_matrices : bool, optional
        If True, `U` and `Vh` are of shape ``(M,M)``, ``(N,N)``.
        If False, the shapes are ``(M,K)`` and ``(K,N)``, where
        ``K = min(M,N)``.
    compute_uv : bool, optional
        Whether to compute also `U` and `Vh` in addition to `s`.
        Default is True.
    overwrite_a : bool, optional
        Whether to overwrite `a`; may improve performance.
        Default is False.
    check_finite : bool, optional
        Whether to check that the input matrix contains only finite numbers.
        Disabling may give a performance gain, but may result in problems
        (crashes, non-termination) if the inputs do contain infinities or NaNs.

    Returns
    -------
    U : ndarray
        Unitary matrix having left singular vectors as columns.
        Of shape ``(M,M)`` or ``(M,K)``, depending on `full_matrices`.
    s : ndarray
        The singular values, sorted in non-increasing order.
        Of shape (K,), with ``K = min(M, N)``.
    Vh : ndarray
        Unitary matrix having right singular vectors as rows.
        Of shape ``(N,N)`` or ``(K,N)`` depending on `full_matrices`.

    For ``compute_uv = False``, only `s` is returned.

    Raises
    ------
    LinAlgError
        If SVD computation does not converge.

    See also
    --------
    svdvals : Compute singular values of a matrix.
    diagsvd : Construct the Sigma matrix, given the vector s.

    Examples
    --------
    >>> from scipy import linalg
    >>> a = np.random.randn(9, 6) + 1.j*np.random.randn(9, 6)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> U, s, Vh = linalg.svd(a)
    >>> U.shape, Vh.shape, s.shape
    ((9, 9), (6, 6), (6,))

    >>> U, s, Vh = linalg.svd(a, full_matrices=False)
    >>> U.shape, Vh.shape, s.shape
    ((9, 6), (6, 6), (6,))
    >>> S = linalg.diagsvd(s, 6, 6)
<<<<<<< HEAD
    >>> allclose(a, dot(U, dot(S, Vh)))
    True

    >>> s2 = linalg.svd(a, compute_uv=False)
    >>> allclose(s, s2)
    True

    See also
    --------
    svdvals : return singular values of a matrix
    diagsvd : return the Sigma matrix, given the vector s

    """
    # A hack until full_matrices == 0 support is fixed here.
    if full_matrices == 0:
        import numpy.linalg
        return numpy.linalg.svd(a, full_matrices=0, compute_uv=compute_uv)
    a1 = asarray_chkfinite(a)
    if len(a1.shape) != 2:
        raise ValueError('expected matrix')
    m,n = a1.shape
    overwrite_a = overwrite_a or (_datanotshared(a1, a))
    gesdd, = get_lapack_funcs(('gesdd',), (a1,))
    if gesdd.module_name[:7] == 'flapack':
        lwork = calc_lwork.gesdd(gesdd.prefix, m, n, compute_uv)[1]
        u,s,v,info = gesdd(a1,compute_uv = compute_uv, lwork = lwork,
                                                overwrite_a = overwrite_a)
    else: # 'clapack'
        raise NotImplementedError('calling gesdd from %s' % gesdd.module_name)
=======
    >>> np.allclose(a, np.dot(U, np.dot(S, Vh)))
    True

    >>> s2 = linalg.svd(a, compute_uv=False)
    >>> np.allclose(s, s2)
    True

    """
    a1 = _asarray_validated(a, check_finite=check_finite)
    if len(a1.shape) != 2:
        raise ValueError('expected matrix')
    m,n = a1.shape
    overwrite_a = overwrite_a or (_datacopied(a1, a))

    gesdd, gesdd_lwork = get_lapack_funcs(('gesdd', 'gesdd_lwork'), (a1,))

    # compute optimal lwork
    lwork, info = gesdd_lwork(a1.shape[0], a1.shape[1], compute_uv=compute_uv, full_matrices=full_matrices)
    if info != 0:
        raise ValueError('work array size computation for internal gesdd failed: %d' % info)
    lwork = int(lwork.real)

    # perform decomposition
    u,s,v,info = gesdd(a1, compute_uv=compute_uv, lwork=lwork,
                       full_matrices=full_matrices, overwrite_a=overwrite_a)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if info > 0:
        raise LinAlgError("SVD did not converge")
    if info < 0:
        raise ValueError('illegal value in %d-th argument of internal gesdd'
                                                                    % -info)
    if compute_uv:
        return u, s, v
    else:
        return s

<<<<<<< HEAD
def svdvals(a, overwrite_a=False):
    """Compute singular values of a matrix.

    Parameters
    ----------
    a : array, shape (M, N)
        Matrix to decompose
    overwrite_a : boolean
        Whether data in a is overwritten (may improve performance)

    Returns
    -------
    s:  array, shape (K,)
        The singular values, sorted so that s[i] >= s[i+1]. K = min(M, N)

    Raises LinAlgError if SVD computation does not converge

    See also
    --------
    svd : return the full singular value decomposition of a matrix
    diagsvd : return the Sigma matrix, given the vector s

    """
    return svd(a, compute_uv=0, overwrite_a=overwrite_a)

def diagsvd(s, M, N):
    """Construct the sigma matrix in SVD from singular values and size M,N.

    Parameters
    ----------
    s : array, shape (M,) or (N,)
        Singular values
    M : integer
    N : integer
        Size of the matrix whose singular values are s

    Returns
    -------
    S : array, shape (M, N)
=======

def svdvals(a, overwrite_a=False, check_finite=True):
    """
    Compute singular values of a matrix.

    Parameters
    ----------
    a : (M, N) array_like
        Matrix to decompose.
    overwrite_a : bool, optional
        Whether to overwrite `a`; may improve performance.
        Default is False.
    check_finite : bool, optional
        Whether to check that the input matrix contains only finite numbers.
        Disabling may give a performance gain, but may result in problems
        (crashes, non-termination) if the inputs do contain infinities or NaNs.

    Returns
    -------
    s : (min(M, N),) ndarray
        The singular values, sorted in decreasing order.

    Raises
    ------
    LinAlgError
        If SVD computation does not converge.

    Notes
    -----
    ``svdvals(a)`` only differs from ``svd(a, compute_uv=False)`` by its
    handling of the edge case of empty ``a``, where it returns an
    empty sequence:

    >>> a = np.empty((0, 2))
    >>> from scipy.linalg import svdvals
    >>> svdvals(a)
    array([], dtype=float64)

    See also
    --------
    svd : Compute the full singular value decomposition of a matrix.
    diagsvd : Construct the Sigma matrix, given the vector s.

    """
    a = _asarray_validated(a, check_finite=check_finite)
    if a.size:
        return svd(a, compute_uv=0, overwrite_a=overwrite_a,
                check_finite=False)
    elif len(a.shape) != 2:
        raise ValueError('expected matrix')
    else:
        return numpy.empty(0)


def diagsvd(s, M, N):
    """
    Construct the sigma matrix in SVD from singular values and size M, N.

    Parameters
    ----------
    s : (M,) or (N,) array_like
        Singular values
    M : int
        Size of the matrix whose singular values are `s`.
    N : int
        Size of the matrix whose singular values are `s`.

    Returns
    -------
    S : (M, N) ndarray
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        The S-matrix in the singular value decomposition

    """
    part = diag(s)
    typ = part.dtype.char
    MorN = len(s)
    if MorN == M:
        return r_['-1', part, zeros((M, N-M), typ)]
    elif MorN == N:
        return r_[part, zeros((M-N,N), typ)]
    else:
        raise ValueError("Length of s must be M or N.")


# Orthonormal decomposition

def orth(A):
<<<<<<< HEAD
    """Construct an orthonormal basis for the range of A using SVD

    Parameters
    ----------
    A : array, shape (M, N)

    Returns
    -------
    Q : array, shape (M, K)
=======
    """
    Construct an orthonormal basis for the range of A using SVD

    Parameters
    ----------
    A : (M, N) array_like
        Input array

    Returns
    -------
    Q : (M, K) ndarray
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Orthonormal basis for the range of A.
        K = effective rank of A, as determined by automatic cutoff

    See also
    --------
    svd : Singular value decomposition of a matrix

    """
<<<<<<< HEAD
    u, s, vh = svd(A)
=======
    u, s, vh = svd(A, full_matrices=False)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    M, N = A.shape
    eps = numpy.finfo(float).eps
    tol = max(M,N) * numpy.amax(s) * eps
    num = numpy.sum(s > tol, dtype=int)
    Q = u[:,:num]
    return Q
