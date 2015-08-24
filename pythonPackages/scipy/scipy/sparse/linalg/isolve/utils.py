<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
__docformat__ = "restructuredtext en"

__all__ = []

from warnings import warn

from numpy import asanyarray, asarray, asmatrix, array, matrix, zeros

<<<<<<< HEAD
from scipy.sparse.linalg.interface import aslinearoperator, LinearOperator
=======
from scipy.sparse.linalg.interface import aslinearoperator, LinearOperator, \
     IdentityOperator
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

_coerce_rules = {('f','f'):'f', ('f','d'):'d', ('f','F'):'F',
                 ('f','D'):'D', ('d','f'):'d', ('d','d'):'d',
                 ('d','F'):'D', ('d','D'):'D', ('F','f'):'F',
                 ('F','d'):'D', ('F','F'):'F', ('F','D'):'D',
                 ('D','f'):'D', ('D','d'):'D', ('D','F'):'D',
                 ('D','D'):'D'}

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def coerce(x,y):
    if x not in 'fdFD':
        x = 'd'
    if y not in 'fdFD':
        y = 'd'
    return _coerce_rules[x,y]

<<<<<<< HEAD
def id(x):
    return x

=======

def id(x):
    return x


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def make_system(A, M, x0, b, xtype=None):
    """Make a linear system Ax=b

    Parameters
    ----------
    A : LinearOperator
        sparse or dense matrix (or any valid input to aslinearoperator)
    M : {LinearOperator, Nones}
        preconditioner
        sparse or dense matrix (or any valid input to aslinearoperator)
    x0 : {array_like, None}
        initial guess to iterative method
    b : array_like
        right hand side
<<<<<<< HEAD
    xtype : {'f', 'd', 'F', 'D', None}
=======
    xtype : {'f', 'd', 'F', 'D', None}, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        dtype of the x vector

    Returns
    -------
    (A, M, x, b, postprocess)
        A : LinearOperator
            matrix of the linear system
        M : LinearOperator
            preconditioner
        x : rank 1 ndarray
            initial guess
        b : rank 1 ndarray
            right hand side
        postprocess : function
            converts the solution vector to the appropriate
            type and dimensions (e.g. (N,1) matrix)

    """
    A_ = A
    A = aslinearoperator(A)

    if A.shape[0] != A.shape[1]:
<<<<<<< HEAD
        raise ValueError('expected square matrix (shape=%s)' % shape)
=======
        raise ValueError('expected square matrix, but got shape=%s' % (A.shape,))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    N = A.shape[0]

    b = asanyarray(b)

    if not (b.shape == (N,1) or b.shape == (N,)):
        raise ValueError('A and b have incompatible dimensions')

    if b.dtype.char not in 'fdFD':
<<<<<<< HEAD
        b = b.astype('d') # upcast non-FP types to double
=======
        b = b.astype('d')  # upcast non-FP types to double
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    def postprocess(x):
        if isinstance(b,matrix):
            x = asmatrix(x)
        return x.reshape(b.shape)

    if xtype is None:
        if hasattr(A,'dtype'):
            xtype = A.dtype.char
        else:
            xtype = A.matvec(b).dtype.char
        xtype = coerce(xtype, b.dtype.char)
    else:
<<<<<<< HEAD
        warn('Use of xtype argument is deprecated. '\
                'Use LinearOperator( ... , dtype=xtype) instead.',\
=======
        warn('Use of xtype argument is deprecated. '
                'Use LinearOperator( ... , dtype=xtype) instead.',
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                DeprecationWarning)
        if xtype == 0:
            xtype = b.dtype.char
        else:
            if xtype not in 'fdFD':
<<<<<<< HEAD
                raise ValueError, "xtype must be 'f', 'd', 'F', or 'D'"

    b = asarray(b,dtype=xtype) #make b the same type as x
=======
                raise ValueError("xtype must be 'f', 'd', 'F', or 'D'")

    b = asarray(b,dtype=xtype)  # make b the same type as x
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    b = b.ravel()

    if x0 is None:
        x = zeros(N, dtype=xtype)
    else:
        x = array(x0, dtype=xtype)
        if not (x.shape == (N,1) or x.shape == (N,)):
            raise ValueError('A and x have incompatible dimensions')
        x = x.ravel()

    # process preconditioner
    if M is None:
        if hasattr(A_,'psolve'):
            psolve = A_.psolve
        else:
            psolve = id
        if hasattr(A_,'rpsolve'):
            rpsolve = A_.rpsolve
        else:
            rpsolve = id
<<<<<<< HEAD
        M = LinearOperator(A.shape, matvec=psolve, rmatvec=rpsolve, dtype=A.dtype)
=======
        if psolve is id and rpsolve is id:
            M = IdentityOperator(shape=A.shape, dtype=A.dtype)
        else:
            M = LinearOperator(A.shape, matvec=psolve, rmatvec=rpsolve,
                               dtype=A.dtype)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    else:
        M = aslinearoperator(M)
        if A.shape != M.shape:
            raise ValueError('matrix and preconditioner have different shapes')

    return A, M, x, b, postprocess
