<<<<<<< HEAD
import warnings
import _minpack

from numpy import atleast_1d, dot, take, triu, shape, eye, \
                  transpose, zeros, product, greater, array, \
                  all, where, isscalar, asarray, inf

error = _minpack.error

__all__ = ['fsolve', 'leastsq', 'fixed_point', 'bisection', 'curve_fit']

def check_func(thefunc, x0, args, numinputs, output_shape=None):
    res = atleast_1d(thefunc(*((x0[:numinputs],)+args)))
=======
from __future__ import division, print_function, absolute_import

import warnings
from . import _minpack

import numpy as np
from numpy import (atleast_1d, dot, take, triu, shape, eye,
                   transpose, zeros, product, greater, array,
                   all, where, isscalar, asarray, inf, abs,
                   finfo, inexact, issubdtype, dtype)
from .optimize import OptimizeResult, _check_unknown_options, OptimizeWarning

error = _minpack.error

__all__ = ['fsolve', 'leastsq', 'fixed_point', 'curve_fit']


def _check_func(checker, argname, thefunc, x0, args, numinputs,
                output_shape=None):
    res = atleast_1d(thefunc(*((x0[:numinputs],) + args)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if (output_shape is not None) and (shape(res) != output_shape):
        if (output_shape[0] != 1):
            if len(output_shape) > 1:
                if output_shape[1] == 1:
                    return shape(res)
<<<<<<< HEAD
            msg = "There is a mismatch between the input and output " \
                  "shape of %s." % thefunc.func_name
            raise TypeError(msg)
    return shape(res)
=======
            msg = "%s: there is a mismatch between the input and output " \
                  "shape of the '%s' argument" % (checker, argname)
            func_name = getattr(thefunc, '__name__', None)
            if func_name:
                msg += " '%s'." % func_name
            else:
                msg += "."
            raise TypeError(msg)
    if issubdtype(res.dtype, inexact):
        dt = res.dtype
    else:
        dt = dtype(float)
    return shape(res), dt
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


def fsolve(func, x0, args=(), fprime=None, full_output=0,
           col_deriv=0, xtol=1.49012e-8, maxfev=0, band=None,
<<<<<<< HEAD
           epsfcn=0.0, factor=100, diag=None, warning=True):
=======
           epsfcn=None, factor=100, diag=None):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    Find the roots of a function.

    Return the roots of the (non-linear) equations defined by
    ``func(x) = 0`` given a starting estimate.

    Parameters
    ----------
<<<<<<< HEAD
    func : callable f(x, *args)
        A function that takes at least one (possibly vector) argument.
    x0 : ndarray
        The starting estimate for the roots of ``func(x) = 0``.
    args : tuple
        Any extra arguments to `func`.
    fprime : callable(x)
        A function to compute the Jacobian of `func` with derivatives
        across the rows. By default, the Jacobian will be estimated.
    full_output : bool
        If True, return optional outputs.
    col_deriv : bool
        Specify whether the Jacobian function computes derivatives down
        the columns (faster, because there is no transpose operation).
    warning : bool
        Whether to print a warning message when the call is unsuccessful.
        This option is deprecated, use the warnings module instead.
=======
    func : callable ``f(x, *args)``
        A function that takes at least one (possibly vector) argument.
    x0 : ndarray
        The starting estimate for the roots of ``func(x) = 0``.
    args : tuple, optional
        Any extra arguments to `func`.
    fprime : callable(x), optional
        A function to compute the Jacobian of `func` with derivatives
        across the rows. By default, the Jacobian will be estimated.
    full_output : bool, optional
        If True, return optional outputs.
    col_deriv : bool, optional
        Specify whether the Jacobian function computes derivatives down
        the columns (faster, because there is no transpose operation).
    xtol : float, optional
        The calculation will terminate if the relative error between two
        consecutive iterates is at most `xtol`.
    maxfev : int, optional
        The maximum number of calls to the function. If zero, then
        ``100*(N+1)`` is the maximum where N is the number of elements
        in `x0`.
    band : tuple, optional
        If set to a two-sequence containing the number of sub- and
        super-diagonals within the band of the Jacobi matrix, the
        Jacobi matrix is considered banded (only for ``fprime=None``).
    epsfcn : float, optional
        A suitable step length for the forward-difference
        approximation of the Jacobian (for ``fprime=None``). If
        `epsfcn` is less than the machine precision, it is assumed
        that the relative errors in the functions are of the order of
        the machine precision.
    factor : float, optional
        A parameter determining the initial step bound
        (``factor * || diag * x||``).  Should be in the interval
        ``(0.1, 100)``.
    diag : sequence, optional
        N positive entries that serve as a scale factors for the
        variables.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Returns
    -------
    x : ndarray
        The solution (or the result of the last iteration for
        an unsuccessful call).
    infodict : dict
<<<<<<< HEAD
        A dictionary of optional outputs with the keys::

          * 'nfev': number of function calls
          * 'njev': number of Jacobian calls
          * 'fvec': function evaluated at the output
          * 'fjac': the orthogonal matrix, q, produced by the QR
                    factorization of the final approximate Jacobian
                    matrix, stored column wise
          * 'r': upper triangular matrix produced by QR factorization of same
                 matrix
          * 'qtf': the vector (transpose(q) * fvec)
=======
        A dictionary of optional outputs with the keys:

        ``nfev``
            number of function calls
        ``njev``
            number of Jacobian calls
        ``fvec``
            function evaluated at the output
        ``fjac``
            the orthogonal matrix, q, produced by the QR
            factorization of the final approximate Jacobian
            matrix, stored column wise
        ``r``
            upper triangular matrix produced by QR factorization
            of the same matrix
        ``qtf``
            the vector ``(transpose(q) * fvec)``
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    ier : int
        An integer flag.  Set to 1 if a solution was found, otherwise refer
        to `mesg` for more information.
    mesg : str
        If no solution is found, `mesg` details the cause of failure.

<<<<<<< HEAD
    Other Parameters
    ----------------
=======
    See Also
    --------
    root : Interface to root finding algorithms for multivariate
    functions. See the 'hybr' `method` in particular.

    Notes
    -----
    ``fsolve`` is a wrapper around MINPACK's hybrd and hybrj algorithms.

    """
    options = {'col_deriv': col_deriv,
               'xtol': xtol,
               'maxfev': maxfev,
               'band': band,
               'eps': epsfcn,
               'factor': factor,
               'diag': diag,
               'full_output': full_output}

    res = _root_hybr(func, x0, args, jac=fprime, **options)
    if full_output:
        x = res['x']
        info = dict((k, res.get(k))
                    for k in ('nfev', 'njev', 'fjac', 'r', 'qtf') if k in res)
        info['fvec'] = res['fun']
        return x, info, res['status'], res['message']
    else:
        return res['x']


def _root_hybr(func, x0, args=(), jac=None,
               col_deriv=0, xtol=1.49012e-08, maxfev=0, band=None, eps=None,
               factor=100, diag=None, full_output=0, **unknown_options):
    """
    Find the roots of a multivariate function using MINPACK's hybrd and
    hybrj routines (modified Powell method).

    Options
    -------
    col_deriv : bool
        Specify whether the Jacobian function computes derivatives down
        the columns (faster, because there is no transpose operation).
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    xtol : float
        The calculation will terminate if the relative error between two
        consecutive iterates is at most `xtol`.
    maxfev : int
        The maximum number of calls to the function. If zero, then
        ``100*(N+1)`` is the maximum where N is the number of elements
        in `x0`.
    band : tuple
        If set to a two-sequence containing the number of sub- and
        super-diagonals within the band of the Jacobi matrix, the
        Jacobi matrix is considered banded (only for ``fprime=None``).
<<<<<<< HEAD
    epsfcn : float
        A suitable step length for the forward-difference
        approximation of the Jacobian (for ``fprime=None``). If
        `epsfcn` is less than the machine precision, it is assumed
=======
    eps : float
        A suitable step length for the forward-difference
        approximation of the Jacobian (for ``fprime=None``). If
        `eps` is less than the machine precision, it is assumed
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        that the relative errors in the functions are of the order of
        the machine precision.
    factor : float
        A parameter determining the initial step bound
        (``factor * || diag * x||``).  Should be in the interval
        ``(0.1, 100)``.
    diag : sequence
        N positive entries that serve as a scale factors for the
        variables.

<<<<<<< HEAD
    Notes
    -----
    ``fsolve`` is a wrapper around MINPACK's hybrd and hybrj algorithms.

    From scipy 0.8.0 `fsolve` returns an array of size one instead of a scalar
    when solving for a single parameter.

    """
    if not warning :
        msg = "The warning keyword is deprecated. Use the warnings module."
        warnings.warn(msg, DeprecationWarning)
    x0 = array(x0,ndmin=1)
    n = len(x0)
    if type(args) != type(()): args = (args,)
    check_func(func,x0,args,n,(n,))
    Dfun = fprime
    if Dfun is None:
        if band is None:
            ml,mu = -10,-10
        else:
            ml,mu = band[:2]
        if (maxfev == 0):
            maxfev = 200*(n+1)
        retval = _minpack._hybrd(func, x0, args, full_output, xtol,
                                 maxfev, ml, mu, epsfcn, factor, diag)
    else:
        check_func(Dfun,x0,args,n,(n,n))
        if (maxfev == 0):
            maxfev = 100*(n+1)
        retval = _minpack._hybrj(func, Dfun, x0, args, full_output,
                                 col_deriv, xtol, maxfev, factor,diag)

    errors = {0:["Improper input parameters were entered.",TypeError],
              1:["The solution converged.", None],
              2:["The number of calls to function has "
                 "reached maxfev = %d." % maxfev, ValueError],
              3:["xtol=%f is too small, no further improvement "
                 "in the approximate\n  solution "
                 "is possible." % xtol, ValueError],
              4:["The iteration is not making good progress, as measured "
                 "by the \n  improvement from the last five "
                 "Jacobian evaluations.", ValueError],
              5:["The iteration is not making good progress, "
                 "as measured by the \n  improvement from the last "
                 "ten iterations.", ValueError],
              'unknown': ["An error occurred.", TypeError]}

    info = retval[-1]    # The FORTRAN return value
    if (info != 1 and not full_output):
        if info in [2,3,4,5]:
            msg = errors[info][0]
            warnings.warn(msg, RuntimeWarning)
        else:
            try:
                raise errors[info][1](errors[info][0])
            except KeyError:
                raise errors['unknown'][1](errors['unknown'][0])

    if full_output:
        try:
            return retval + (errors[info][0],)  # Return all + the message
        except KeyError:
            return retval + (errors['unknown'][0],)
    else:
        return retval[0]
=======
    """
    _check_unknown_options(unknown_options)
    epsfcn = eps

    x0 = asarray(x0).flatten()
    n = len(x0)
    if not isinstance(args, tuple):
        args = (args,)
    shape, dtype = _check_func('fsolve', 'func', func, x0, args, n, (n,))
    if epsfcn is None:
        epsfcn = finfo(dtype).eps
    Dfun = jac
    if Dfun is None:
        if band is None:
            ml, mu = -10, -10
        else:
            ml, mu = band[:2]
        if maxfev == 0:
            maxfev = 200 * (n + 1)
        retval = _minpack._hybrd(func, x0, args, 1, xtol, maxfev,
                                 ml, mu, epsfcn, factor, diag)
    else:
        _check_func('fsolve', 'fprime', Dfun, x0, args, n, (n, n))
        if (maxfev == 0):
            maxfev = 100 * (n + 1)
        retval = _minpack._hybrj(func, Dfun, x0, args, 1,
                                 col_deriv, xtol, maxfev, factor, diag)

    x, status = retval[0], retval[-1]

    errors = {0: ["Improper input parameters were entered.", TypeError],
              1: ["The solution converged.", None],
              2: ["The number of calls to function has "
                  "reached maxfev = %d." % maxfev, ValueError],
              3: ["xtol=%f is too small, no further improvement "
                  "in the approximate\n  solution "
                  "is possible." % xtol, ValueError],
              4: ["The iteration is not making good progress, as measured "
                  "by the \n  improvement from the last five "
                  "Jacobian evaluations.", ValueError],
              5: ["The iteration is not making good progress, "
                  "as measured by the \n  improvement from the last "
                  "ten iterations.", ValueError],
              'unknown': ["An error occurred.", TypeError]}

    if status != 1 and not full_output:
        if status in [2, 3, 4, 5]:
            msg = errors[status][0]
            warnings.warn(msg, RuntimeWarning)
        else:
            try:
                raise errors[status][1](errors[status][0])
            except KeyError:
                raise errors['unknown'][1](errors['unknown'][0])

    info = retval[1]
    info['fun'] = info.pop('fvec')
    sol = OptimizeResult(x=x, success=(status == 1), status=status)
    sol.update(info)
    try:
        sol['message'] = errors[status][0]
    except KeyError:
        info['message'] = errors['unknown'][0]

    return sol
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


def leastsq(func, x0, args=(), Dfun=None, full_output=0,
            col_deriv=0, ftol=1.49012e-8, xtol=1.49012e-8,
<<<<<<< HEAD
            gtol=0.0, maxfev=0, epsfcn=0.0, factor=100, diag=None,warning=True):
    """Minimize the sum of squares of a set of equations.
=======
            gtol=0.0, maxfev=0, epsfcn=None, factor=100, diag=None):
    """
    Minimize the sum of squares of a set of equations.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    ::

        x = arg min(sum(func(y)**2,axis=0))
                 y

    Parameters
    ----------
    func : callable
        should take at least one (possibly length N vector) argument and
<<<<<<< HEAD
        returns M floating point numbers.
    x0 : ndarray
        The starting estimate for the minimization.
    args : tuple
        Any extra arguments to func are placed in this tuple.
    Dfun : callable
        A function or method to compute the Jacobian of func with derivatives
        across the rows. If this is None, the Jacobian will be estimated.
    full_output : bool
        non-zero to return all optional outputs.
    col_deriv : bool
        non-zero to specify that the Jacobian function computes derivatives
        down the columns (faster, because there is no transpose operation).
    ftol : float
        Relative error desired in the sum of squares.
    xtol : float
        Relative error desired in the approximate solution.
    gtol : float
        Orthogonality desired between the function vector and the columns of
        the Jacobian.
    maxfev : int
        The maximum number of calls to the function. If zero, then 100*(N+1) is
        the maximum where N is the number of elements in x0.
    epsfcn : float
        A suitable step length for the forward-difference approximation of the
        Jacobian (for Dfun=None). If epsfcn is less than the machine precision,
        it is assumed that the relative errors in the functions are of the
        order of the machine precision.
    factor : float
        A parameter determining the initial step bound
        (``factor * || diag * x||``). Should be in interval ``(0.1, 100)``.
    diag : sequence
        N positive entries that serve as a scale factors for the variables.
    warning : bool
        Whether to print a warning message when the call is unsuccessful.
        Deprecated, use the warnings module instead.
=======
        returns M floating point numbers. It must not return NaNs or
        fitting might fail.
    x0 : ndarray
        The starting estimate for the minimization.
    args : tuple, optional
        Any extra arguments to func are placed in this tuple.
    Dfun : callable, optional
        A function or method to compute the Jacobian of func with derivatives
        across the rows. If this is None, the Jacobian will be estimated.
    full_output : bool, optional
        non-zero to return all optional outputs.
    col_deriv : bool, optional
        non-zero to specify that the Jacobian function computes derivatives
        down the columns (faster, because there is no transpose operation).
    ftol : float, optional
        Relative error desired in the sum of squares.
    xtol : float, optional
        Relative error desired in the approximate solution.
    gtol : float, optional
        Orthogonality desired between the function vector and the columns of
        the Jacobian.
    maxfev : int, optional
        The maximum number of calls to the function. If `Dfun` is provided
        then the default `maxfev` is 100*(N+1) where N is the number of elements
        in x0, otherwise the default `maxfev` is 200*(N+1).
    epsfcn : float, optional
        A variable used in determining a suitable step length for the forward-
        difference approximation of the Jacobian (for Dfun=None). 
        Normally the actual step length will be sqrt(epsfcn)*x
        If epsfcn is less than the machine precision, it is assumed that the 
        relative errors are of the order of the machine precision.
    factor : float, optional
        A parameter determining the initial step bound
        (``factor * || diag * x||``). Should be in interval ``(0.1, 100)``.
    diag : sequence, optional
        N positive entries that serve as a scale factors for the variables.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Returns
    -------
    x : ndarray
        The solution (or the result of the last iteration for an unsuccessful
        call).
    cov_x : ndarray
        Uses the fjac and ipvt optional outputs to construct an
<<<<<<< HEAD
        estimate of the jacobian around the solution.  ``None`` if a
        singular matrix encountered (indicates very flat curvature in
        some direction).  This matrix must be multiplied by the
        residual standard deviation to get the covariance of the
        parameter estimates -- see curve_fit.
    infodict : dict
        a dictionary of optional outputs with the keys::

            - 'nfev' : the number of function calls
            - 'fvec' : the function evaluated at the output
            - 'fjac' : A permutation of the R matrix of a QR
                     factorization of the final approximate
                     Jacobian matrix, stored column wise.
                     Together with ipvt, the covariance of the
                     estimate can be approximated.
            - 'ipvt' : an integer array of length N which defines
                     a permutation matrix, p, such that
                     fjac*p = q*r, where r is upper triangular
                     with diagonal elements of nonincreasing
                     magnitude. Column j of p is column ipvt(j)
                     of the identity matrix.
            - 'qtf'  : the vector (transpose(q) * fvec).
=======
        estimate of the jacobian around the solution. None if a
        singular matrix encountered (indicates very flat curvature in
        some direction).  This matrix must be multiplied by the
        residual variance to get the covariance of the
        parameter estimates -- see curve_fit.
    infodict : dict
        a dictionary of optional outputs with the key s:

        ``nfev``
            The number of function calls
        ``fvec``
            The function evaluated at the output
        ``fjac``
            A permutation of the R matrix of a QR
            factorization of the final approximate
            Jacobian matrix, stored column wise.
            Together with ipvt, the covariance of the
            estimate can be approximated.
        ``ipvt``
            An integer array of length N which defines
            a permutation matrix, p, such that
            fjac*p = q*r, where r is upper triangular
            with diagonal elements of nonincreasing
            magnitude. Column j of p is column ipvt(j)
            of the identity matrix.
        ``qtf``
            The vector (transpose(q) * fvec).

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    mesg : str
        A string message giving information about the cause of failure.
    ier : int
        An integer flag.  If it is equal to 1, 2, 3 or 4, the solution was
        found.  Otherwise, the solution was not found. In either case, the
        optional output variable 'mesg' gives more information.

    Notes
    -----
    "leastsq" is a wrapper around MINPACK's lmdif and lmder algorithms.

<<<<<<< HEAD
    From scipy 0.8.0 `leastsq` returns an array of size one instead of a scalar
    when solving for a single parameter.

    """
    if not warning :
        msg = "The warning keyword is deprecated. Use the warnings module."
        warnings.warn(msg, DeprecationWarning)
    x0 = array(x0,ndmin=1)
    n = len(x0)
    if type(args) != type(()): args = (args,)
    m = check_func(func,x0,args,n)[0]
    if n>m:
        raise TypeError('Improper input: N=%s must not exceed M=%s' % (n,m))
    if Dfun is None:
        if (maxfev == 0):
            maxfev = 200*(n+1)
        retval = _minpack._lmdif(func, x0, args, full_output,
                                 ftol, xtol, gtol,
                                 maxfev, epsfcn, factor, diag)
    else:
        if col_deriv:
            check_func(Dfun,x0,args,n,(n,m))
        else:
            check_func(Dfun,x0,args,n,(m,n))
        if (maxfev == 0):
            maxfev = 100*(n+1)
        retval = _minpack._lmder(func,Dfun,x0,args,full_output,col_deriv,ftol,xtol,gtol,maxfev,factor,diag)

    errors = {0:["Improper input parameters.", TypeError],
              1:["Both actual and predicted relative reductions "
                 "in the sum of squares\n  are at most %f" % ftol, None],
              2:["The relative error between two consecutive "
                 "iterates is at most %f" % xtol, None],
              3:["Both actual and predicted relative reductions in "
                 "the sum of squares\n  are at most %f and the "
                 "relative error between two consecutive "
                 "iterates is at \n  most %f" % (ftol,xtol), None],
              4:["The cosine of the angle between func(x) and any "
                 "column of the\n  Jacobian is at most %f in "
                 "absolute value" % gtol, None],
              5:["Number of calls to function has reached "
                 "maxfev = %d." % maxfev, ValueError],
              6:["ftol=%f is too small, no further reduction "
                 "in the sum of squares\n  is possible.""" % ftol, ValueError],
              7:["xtol=%f is too small, no further improvement in "
                 "the approximate\n  solution is possible." % xtol, ValueError],
              8:["gtol=%f is too small, func(x) is orthogonal to the "
                 "columns of\n  the Jacobian to machine "
                 "precision." % gtol, ValueError],
              'unknown':["Unknown error.", TypeError]}

    info = retval[-1]    # The FORTRAN return value

    if (info not in [1,2,3,4] and not full_output):
        if info in [5,6,7,8]:
=======
    cov_x is a Jacobian approximation to the Hessian of the least squares
    objective function.
    This approximation assumes that the objective function is based on the
    difference between some observed target data (ydata) and a (non-linear)
    function of the parameters `f(xdata, params)` ::

           func(params) = ydata - f(xdata, params)

    so that the objective function is ::

           min   sum((ydata - f(xdata, params))**2, axis=0)
         params

    """
    x0 = asarray(x0).flatten()
    n = len(x0)
    if not isinstance(args, tuple):
        args = (args,)
    shape, dtype = _check_func('leastsq', 'func', func, x0, args, n)
    m = shape[0]
    if n > m:
        raise TypeError('Improper input: N=%s must not exceed M=%s' % (n, m))
    if epsfcn is None:
        epsfcn = finfo(dtype).eps
    if Dfun is None:
        if maxfev == 0:
            maxfev = 200*(n + 1)
        retval = _minpack._lmdif(func, x0, args, full_output, ftol, xtol,
                                 gtol, maxfev, epsfcn, factor, diag)
    else:
        if col_deriv:
            _check_func('leastsq', 'Dfun', Dfun, x0, args, n, (n, m))
        else:
            _check_func('leastsq', 'Dfun', Dfun, x0, args, n, (m, n))
        if maxfev == 0:
            maxfev = 100 * (n + 1)
        retval = _minpack._lmder(func, Dfun, x0, args, full_output, col_deriv,
                                 ftol, xtol, gtol, maxfev, factor, diag)

    errors = {0: ["Improper input parameters.", TypeError],
              1: ["Both actual and predicted relative reductions "
                  "in the sum of squares\n  are at most %f" % ftol, None],
              2: ["The relative error between two consecutive "
                  "iterates is at most %f" % xtol, None],
              3: ["Both actual and predicted relative reductions in "
                  "the sum of squares\n  are at most %f and the "
                  "relative error between two consecutive "
                  "iterates is at \n  most %f" % (ftol, xtol), None],
              4: ["The cosine of the angle between func(x) and any "
                  "column of the\n  Jacobian is at most %f in "
                  "absolute value" % gtol, None],
              5: ["Number of calls to function has reached "
                  "maxfev = %d." % maxfev, ValueError],
              6: ["ftol=%f is too small, no further reduction "
                  "in the sum of squares\n  is possible.""" % ftol,
                  ValueError],
              7: ["xtol=%f is too small, no further improvement in "
                  "the approximate\n  solution is possible." % xtol,
                  ValueError],
              8: ["gtol=%f is too small, func(x) is orthogonal to the "
                  "columns of\n  the Jacobian to machine "
                  "precision." % gtol, ValueError],
              'unknown': ["Unknown error.", TypeError]}

    info = retval[-1]    # The FORTRAN return value

    if info not in [1, 2, 3, 4] and not full_output:
        if info in [5, 6, 7, 8]:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            warnings.warn(errors[info][0], RuntimeWarning)
        else:
            try:
                raise errors[info][1](errors[info][0])
            except KeyError:
                raise errors['unknown'][1](errors['unknown'][0])

    mesg = errors[info][0]
    if full_output:
        cov_x = None
<<<<<<< HEAD
        if info in [1,2,3,4]:
            from numpy.dual import inv
            from numpy.linalg import LinAlgError
            perm = take(eye(n),retval[1]['ipvt']-1,0)
            r = triu(transpose(retval[1]['fjac'])[:n,:])
            R = dot(r, perm)
            try:
                cov_x = inv(dot(transpose(R),R))
            except LinAlgError:
                pass
        return (retval[0], cov_x) + retval[1:-1] + (mesg,info)
    else:
        return (retval[0], info)

def _general_function(params, xdata, ydata, function):
    return function(xdata, *params) - ydata

def _weighted_general_function(params, xdata, ydata, function, weights):
    return weights * (function(xdata, *params) - ydata)

def curve_fit(f, xdata, ydata, p0=None, sigma=None, **kw):
=======
        if info in [1, 2, 3, 4]:
            from numpy.dual import inv
            from numpy.linalg import LinAlgError
            perm = take(eye(n), retval[1]['ipvt'] - 1, 0)
            r = triu(transpose(retval[1]['fjac'])[:n, :])
            R = dot(r, perm)
            try:
                cov_x = inv(dot(transpose(R), R))
            except (LinAlgError, ValueError):
                pass
        return (retval[0], cov_x) + retval[1:-1] + (mesg, info)
    else:
        return (retval[0], info)


def _general_function(params, xdata, ydata, function):
    return function(xdata, *params) - ydata


def _weighted_general_function(params, xdata, ydata, function, weights):
    return weights * (function(xdata, *params) - ydata)


def curve_fit(f, xdata, ydata, p0=None, sigma=None, absolute_sigma=False,
              check_finite=True, **kw):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    Use non-linear least squares to fit a function, f, to data.

    Assumes ``ydata = f(xdata, *params) + eps``

    Parameters
    ----------
    f : callable
        The model function, f(x, ...).  It must take the independent
        variable as the first argument and the parameters to fit as
        separate remaining arguments.
<<<<<<< HEAD
    xdata : An N-length sequence or an (k,N)-shaped array
        for functions with k predictors.
        The independent variable where the data is measured.
    ydata : N-length sequence
        The dependent data --- nominally f(xdata, ...)
    p0 : None, scalar, or M-length sequence
=======
    xdata : An M-length sequence or an (k,M)-shaped array
        for functions with k predictors.
        The independent variable where the data is measured.
    ydata : M-length sequence
        The dependent data --- nominally f(xdata, ...)
    p0 : None, scalar, or N-length sequence, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Initial guess for the parameters.  If None, then the initial
        values will all be 1 (if the number of parameters for the function
        can be determined using introspection, otherwise a ValueError
        is raised).
<<<<<<< HEAD
    sigma : None or N-length sequence
        If not None, it represents the standard-deviation of ydata.
        This vector, if given, will be used as weights in the
        least-squares problem.

=======
    sigma : None or M-length sequence, optional
        If not None, the uncertainties in the ydata array. These are used as
        weights in the least-squares problem
        i.e. minimising ``np.sum( ((f(xdata, *popt) - ydata) / sigma)**2 )``
        If None, the uncertainties are assumed to be 1.
    absolute_sigma : bool, optional
        If False, `sigma` denotes relative weights of the data points.
        The returned covariance matrix `pcov` is based on *estimated*
        errors in the data, and is not affected by the overall
        magnitude of the values in `sigma`. Only the relative
        magnitudes of the `sigma` values matter.

        If True, `sigma` describes one standard deviation errors of
        the input data points. The estimated covariance in `pcov` is
        based on these values.
    check_finite : bool, optional
        If True, check that the input arrays do not contain nans of infs,
        and raise a ValueError if they do. Setting this parameter to
        False may silently produce nonsensical results if the input arrays
        do contain nans.
        Default is True.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Returns
    -------
    popt : array
        Optimal values for the parameters so that the sum of the squared error
        of ``f(xdata, *popt) - ydata`` is minimized
    pcov : 2d array
<<<<<<< HEAD
        The estimated covariance of popt.  The diagonals provide the variance
        of the parameter estimate.

    Notes
    -----
    The algorithm uses the Levenburg-Marquardt algorithm:
    scipy.optimize.leastsq. Additional keyword arguments are passed directly
    to that algorithm.
=======
        The estimated covariance of popt. The diagonals provide the variance
        of the parameter estimate. To compute one standard deviation errors
        on the parameters use ``perr = np.sqrt(np.diag(pcov))``.

        How the `sigma` parameter affects the estimated covariance
        depends on `absolute_sigma` argument, as described above.

    Raises
    ------
    OptimizeWarning
        if covariance of the parameters can not be estimated.

    ValueError
        if ydata and xdata contain NaNs.

    See Also
    --------
    leastsq

    Notes
    -----
    The algorithm uses the Levenberg-Marquardt algorithm through `leastsq`.
    Additional keyword arguments are passed directly to that algorithm.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Examples
    --------
    >>> import numpy as np
    >>> from scipy.optimize import curve_fit
    >>> def func(x, a, b, c):
<<<<<<< HEAD
    ...     return a*np.exp(-b*x) + c

    >>> x = np.linspace(0,4,50)
    >>> y = func(x, 2.5, 1.3, 0.5)
    >>> yn = y + 0.2*np.random.normal(size=len(x))

    >>> popt, pcov = curve_fit(func, x, yn)

    """
    if p0 is None or isscalar(p0):
=======
    ...     return a * np.exp(-b * x) + c

    >>> xdata = np.linspace(0, 4, 50)
    >>> y = func(xdata, 2.5, 1.3, 0.5)
    >>> ydata = y + 0.2 * np.random.normal(size=len(xdata))

    >>> popt, pcov = curve_fit(func, xdata, ydata)

    """
    if p0 is None:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        # determine number of parameters by inspecting the function
        import inspect
        args, varargs, varkw, defaults = inspect.getargspec(f)
        if len(args) < 2:
            msg = "Unable to determine number of fit parameters."
            raise ValueError(msg)
<<<<<<< HEAD
        if p0 is None:
            p0 = 1.0
        p0 = [p0]*(len(args)-1)
=======
        if 'self' in args:
            p0 = [1.0] * (len(args)-2)
        else:
            p0 = [1.0] * (len(args)-1)

    # Check input arguments
    if isscalar(p0):
        p0 = array([p0])

    # NaNs can not be handled
    if check_finite:
        ydata = np.asarray_chkfinite(ydata)
    else:
        ydata = np.asarray(ydata)
    if isinstance(xdata, (list, tuple, np.ndarray)):
        # `xdata` is passed straight to the user-defined `f`, so allow
        # non-array_like `xdata`.
        if check_finite:
            xdata = np.asarray_chkfinite(xdata)
        else:
            xdata = np.asarray(xdata)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    args = (xdata, ydata, f)
    if sigma is None:
        func = _general_function
    else:
        func = _weighted_general_function
<<<<<<< HEAD
        args += (1.0/asarray(sigma),)
    res = leastsq(func, p0, args=args, full_output=1, **kw)
    (popt, pcov, infodict, errmsg, ier) = res

    if ier not in [1,2,3,4]:
        msg = "Optimal parameters not found: " + errmsg
        raise RuntimeError(msg)

    if (len(ydata) > len(p0)) and pcov is not None:
        s_sq = (func(popt, *args)**2).sum()/(len(ydata)-len(p0))
        pcov = pcov * s_sq
    else:
        pcov = inf

    return popt, pcov

def check_gradient(fcn,Dfcn,x0,args=(),col_deriv=0):
=======
        args += (1.0 / asarray(sigma),)

    # Remove full_output from kw, otherwise we're passing it in twice.
    return_full = kw.pop('full_output', False)
    res = leastsq(func, p0, args=args, full_output=1, **kw)
    (popt, pcov, infodict, errmsg, ier) = res

    if ier not in [1, 2, 3, 4]:
        msg = "Optimal parameters not found: " + errmsg
        raise RuntimeError(msg)

    warn_cov = False
    if pcov is None:
        # indeterminate covariance
        pcov = zeros((len(popt), len(popt)), dtype=float)
        pcov.fill(inf)
        warn_cov = True
    elif not absolute_sigma:
        if len(ydata) > len(p0):
            s_sq = (asarray(func(popt, *args))**2).sum() / (len(ydata) - len(p0))
            pcov = pcov * s_sq
        else:
            pcov.fill(inf)
            warn_cov = True

    if warn_cov:
        warnings.warn('Covariance of the parameters could not be estimated',
                category=OptimizeWarning)

    if return_full:
        return popt, pcov, infodict, errmsg, ier
    else:
        return popt, pcov


def check_gradient(fcn, Dfcn, x0, args=(), col_deriv=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Perform a simple check on the gradient for correctness.

    """

    x = atleast_1d(x0)
    n = len(x)
<<<<<<< HEAD
    x=x.reshape((n,))
    fvec = atleast_1d(fcn(x,*args))
    m = len(fvec)
    fvec=fvec.reshape((m,))
    ldfjac = m
    fjac = atleast_1d(Dfcn(x,*args))
    fjac=fjac.reshape((m,n))
=======
    x = x.reshape((n,))
    fvec = atleast_1d(fcn(x, *args))
    m = len(fvec)
    fvec = fvec.reshape((m,))
    ldfjac = m
    fjac = atleast_1d(Dfcn(x, *args))
    fjac = fjac.reshape((m, n))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if col_deriv == 0:
        fjac = transpose(fjac)

    xp = zeros((n,), float)
    err = zeros((m,), float)
    fvecp = None
<<<<<<< HEAD
    _minpack._chkder(m,n,x,fvec,fjac,ldfjac,xp,fvecp,1,err)

    fvecp = atleast_1d(fcn(xp,*args))
    fvecp=fvecp.reshape((m,))
    _minpack._chkder(m,n,x,fvec,fjac,ldfjac,xp,fvecp,2,err)

    good = (product(greater(err,0.5),axis=0))

    return (good,err)


# Newton-Raphson method
def newton(func, x0, fprime=None, args=(), tol=1.48e-8, maxiter=50):
    """Find a zero using the Newton-Raphson or secant method.

    Find a zero of the function `func` given a nearby starting point `x0`.
    The Newton-Rapheson method is used if the derivative `fprime` of `func`
    is provided, otherwise the secant method is used.
=======
    _minpack._chkder(m, n, x, fvec, fjac, ldfjac, xp, fvecp, 1, err)

    fvecp = atleast_1d(fcn(xp, *args))
    fvecp = fvecp.reshape((m,))
    _minpack._chkder(m, n, x, fvec, fjac, ldfjac, xp, fvecp, 2, err)

    good = (product(greater(err, 0.5), axis=0))

    return (good, err)


def fixed_point(func, x0, args=(), xtol=1e-8, maxiter=500):
    """
    Find a fixed point of the function.

    Given a function of one or more variables and a starting point, find a
    fixed-point of the function: i.e. where ``func(x0) == x0``.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    func : function
<<<<<<< HEAD
        The function whose zero is wanted. It must be a function of a
        single variable of the form f(x,a,b,c...), where a,b,c... are extra
        arguments that can be passed in the `args` parameter.
    x0 : float
        An initial estimate of the zero that should be somewhere near the
        actual zero.
    fprime : {None, function}, optional
        The derivative of the function when available and convenient. If it
        is None, then the secant method is used. The default is None.
    args : tuple, optional
        Extra arguments to be used in the function call.
    tol : float, optional
        The allowable error of the zero value.
    maxiter : int, optional
        Maximum number of iterations.

    Returns
    -------
    zero : float
        Estimated location where function is zero.

    See Also
    --------
    brentq, brenth, ridder, bisect -- find zeroes in one dimension.
    fsolve -- find zeroes in n dimensions.

    Notes
    -----
    The convergence rate of the Newton-Rapheson method is quadratic while
    that of the secant method is somewhat less. This means that if the
    function is  well behaved the actual error in the estimated zero is
    approximatly the square of the requested tolerance up to roundoff
    error. However, the stopping criterion used here is the step size and
    there is no quarantee that a zero has been found. Consequently the
    result should be verified. Safer algorithms are brentq, brenth, ridder,
    and bisect, but they all require that the root first be bracketed in an
    interval where the function changes sign. The brentq algorithm is
    recommended for general use in one dimemsional problems when such an
    interval has been found.

    """
    msg  = "minpack.newton is moving to zeros.newton"
    warnings.warn(msg, DeprecationWarning)

    if fprime is not None:
        # Newton-Rapheson method
        p0 = x0
        for iter in range(maxiter):
            myargs = (p0,) + args
            fval = func(*myargs)
            fder = fprime(*myargs)
            if fder == 0:
                msg = "derivative was zero."
                warnings.warn(msg, RuntimeWarning)
                return p0
            p = p0 - func(*myargs)/fprime(*myargs)
            if abs(p - p0) < tol:
                return p
            p0 = p
    else:
        # Secant method
        p0 = x0
        if x0 >= 0:
            p1 = x0*(1 + 1e-4) + 1e-4
        else:
            p1 = x0*(1 + 1e-4) - 1e-4
        q0 = func(*((p0,) + args))
        q1 = func(*((p1,) + args))
        for iter in range(maxiter):
            if q1 == q0:
                if p1 != p0:
                    msg = "Tolerance of %s reached" % (p1 - p0)
                    warnings.warn(msg, RuntimeWarning)
                return (p1 + p0)/2.0
            else:
                p = p1 - q1*(p1 - p0)/(q1 - q0)
            if abs(p - p1) < tol:
                return p
            p0 = p1
            q0 = q1
            p1 = p
            q1 = func(*((p1,) + args))
    msg = "Failed to converge after %d iterations, value is %s" % (maxiter, p)
    raise RuntimeError(msg)


# Steffensen's Method using Aitken's Del^2 convergence acceleration.
def fixed_point(func, x0, args=(), xtol=1e-8, maxiter=500):
    """Find the point where func(x) == x

    Given a function of one or more variables and a starting point, find a
    fixed-point of the function: i.e. where func(x)=x.

    Uses Steffensen's Method using Aitken's Del^2 convergence acceleration.
    See Burden, Faires, "Numerical Analysis", 5th edition, pg. 80

    Example
    -------
    >>> from numpy import sqrt, array
    >>> from scipy.optimize import fixed_point
    >>> def func(x, c1, c2):
            return sqrt(c1/(x+c2))
    >>> c1 = array([10,12.])
    >>> c2 = array([3, 5.])
    >>> fixed_point(func, [1.2, 1.3], args=(c1,c2))
=======
        Function to evaluate.
    x0 : array_like
        Fixed point of function.
    args : tuple, optional
        Extra arguments to `func`.
    xtol : float, optional
        Convergence tolerance, defaults to 1e-08.
    maxiter : int, optional
        Maximum number of iterations, defaults to 500.

    Notes
    -----
    Uses Steffensen's Method using Aitken's ``Del^2`` convergence acceleration.
    See Burden, Faires, "Numerical Analysis", 5th edition, pg. 80

    Examples
    --------
    >>> from scipy import optimize
    >>> def func(x, c1, c2):
    ...    return np.sqrt(c1/(x+c2))
    >>> c1 = np.array([10,12.])
    >>> c2 = np.array([3, 5.])
    >>> optimize.fixed_point(func, [1.2, 1.3], args=(c1,c2))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    array([ 1.4920333 ,  1.37228132])

    """
    if not isscalar(x0):
        x0 = asarray(x0)
        p0 = x0
        for iter in range(maxiter):
            p1 = func(p0, *args)
            p2 = func(p1, *args)
            d = p2 - 2.0 * p1 + p0
<<<<<<< HEAD
            p = where(d == 0, p2, p0 - (p1 - p0)*(p1-p0) / d)
            relerr = where(p0 == 0, p, (p-p0)/p0)
            if all(relerr < xtol):
=======
            p = where(d == 0, p2, p0 - (p1 - p0)*(p1 - p0) / d)
            relerr = where(p0 == 0, p, (p-p0)/p0)
            if all(abs(relerr) < xtol):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                return p
            p0 = p
    else:
        p0 = x0
        for iter in range(maxiter):
            p1 = func(p0, *args)
            p2 = func(p1, *args)
            d = p2 - 2.0 * p1 + p0
            if d == 0.0:
                return p2
            else:
<<<<<<< HEAD
                p = p0 - (p1 - p0)*(p1-p0) / d
            if p0 == 0:
                relerr = p
            else:
                relerr = (p-p0)/p0
            if relerr < xtol:
=======
                p = p0 - (p1 - p0)*(p1 - p0) / d
            if p0 == 0:
                relerr = p
            else:
                relerr = (p - p0)/p0
            if abs(relerr) < xtol:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                return p
            p0 = p
    msg = "Failed to converge after %d iterations, value is %s" % (maxiter, p)
    raise RuntimeError(msg)
<<<<<<< HEAD


def bisection(func, a, b, args=(), xtol=1e-10, maxiter=400):
    """Bisection root-finding method.  Given a function and an interval with
    func(a) * func(b) < 0, find the root between a and b.

    """
    msg = "minpack.bisection is deprecated, use zeros.bisect instead"
    warnings.warn(msg, DeprecationWarning)

    i = 1
    eva = func(a,*args)
    evb = func(b,*args)
    if eva*evb >= 0:
        msg = "Must start with interval where func(a) * func(b) < 0"
        raise ValueError(msg)
    while i <= maxiter:
        dist = (b - a)/2.0
        p = a + dist
        if dist < xtol:
            return p
        ev = func(p,*args)
        if ev == 0:
            return p
        i += 1
        if ev*eva > 0:
            a = p
            eva = ev
        else:
            b = p
    msg = "Failed to converge after %d iterations, value is %s" % (maxiter, p)
    raise RuntimeError(msg)
=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
