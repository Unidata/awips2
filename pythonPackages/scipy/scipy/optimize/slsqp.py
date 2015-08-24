<<<<<<< HEAD
"""This module implements the Sequential Least SQuares Programming optimization
algorithm (SLSQP), orginally developed by Dieter Kraft.

See http://www.netlib.org/toms/733

"""

__all__ = ['approx_jacobian','fmin_slsqp']

from _slsqp import slsqp
from numpy import zeros, array, linalg, append, asfarray, concatenate, finfo, \
                  sqrt, vstack
from optimize import approx_fprime, wrap_function
=======
"""
This module implements the Sequential Least SQuares Programming optimization
algorithm (SLSQP), originally developed by Dieter Kraft.
See http://www.netlib.org/toms/733

Functions
---------
.. autosummary::
   :toctree: generated/

    approx_jacobian
    fmin_slsqp

"""

from __future__ import division, print_function, absolute_import

__all__ = ['approx_jacobian','fmin_slsqp']

from scipy.optimize._slsqp import slsqp
from numpy import zeros, array, linalg, append, asfarray, concatenate, finfo, \
                  sqrt, vstack, exp, inf, where, isfinite, atleast_1d
from .optimize import wrap_function, OptimizeResult, _check_unknown_options
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

__docformat__ = "restructuredtext en"

_epsilon = sqrt(finfo(float).eps)

<<<<<<< HEAD
def approx_jacobian(x,func,epsilon,*args):
    """Approximate the Jacobian matrix of a callable function.
=======

def approx_jacobian(x,func,epsilon,*args):
    """
    Approximate the Jacobian matrix of a callable function.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    x : array_like
        The state vector at which to compute the Jacobian matrix.
<<<<<<< HEAD
    func : callable f(x, *args)
        The vector-valued function.
    epsilon : float\
        The peturbation used to determine the partial derivatives.
    *args : tuple
=======
    func : callable f(x,*args)
        The vector-valued function.
    epsilon : float
        The perturbation used to determine the partial derivatives.
    args : sequence
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Additional arguments passed to func.

    Returns
    -------
    An array of dimensions ``(lenf, lenx)`` where ``lenf`` is the length
    of the outputs of `func`, and ``lenx`` is the number of elements in
    `x`.

    Notes
    -----
    The approximation is done using forward differences.

    """
    x0 = asfarray(x)
<<<<<<< HEAD
    f0 = func(*((x0,)+args))
=======
    f0 = atleast_1d(func(*((x0,)+args)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    jac = zeros([len(x0),len(f0)])
    dx = zeros(len(x0))
    for i in range(len(x0)):
        dx[i] = epsilon
        jac[i] = (func(*((x0+dx,)+args)) - f0)/epsilon
        dx[i] = 0.0
<<<<<<< HEAD
    return jac.transpose()


def fmin_slsqp( func, x0 , eqcons=[], f_eqcons=None, ieqcons=[], f_ieqcons=None,
                bounds = [], fprime = None, fprime_eqcons=None,
                fprime_ieqcons=None, args = (), iter = 100, acc = 1.0E-6,
                iprint = 1, full_output = 0, epsilon = _epsilon ):
=======

    return jac.transpose()


def fmin_slsqp(func, x0, eqcons=(), f_eqcons=None, ieqcons=(), f_ieqcons=None,
               bounds=(), fprime=None, fprime_eqcons=None,
               fprime_ieqcons=None, args=(), iter=100, acc=1.0E-6,
               iprint=1, disp=None, full_output=0, epsilon=_epsilon,
               callback=None):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    Minimize a function using Sequential Least SQuares Programming

    Python interface function for the SLSQP Optimization subroutine
    originally implemented by Dieter Kraft.

    Parameters
    ----------
    func : callable f(x,*args)
        Objective function.
<<<<<<< HEAD
    x0 : ndarray of float
        Initial guess for the independent variable(s).
    eqcons : list
        A list of functions of length n such that
        eqcons[j](x0,*args) == 0.0 in a successfully optimized
        problem.
    f_eqcons : callable f(x,*args)
        Returns an array in which each element must equal 0.0 in a
        successfully optimized problem.  If f_eqcons is specified,
        eqcons is ignored.
    ieqcons : list
        A list of functions of length n such that
        ieqcons[j](x0,*args) >= 0.0 in a successfully optimized
        problem.
    f_ieqcons : callable f(x0,*args)
        Returns an array in which each element must be greater or
        equal to 0.0 in a successfully optimized problem.  If
        f_ieqcons is specified, ieqcons is ignored.
    bounds : list
        A list of tuples specifying the lower and upper bound
        for each independent variable [(xl0, xu0),(xl1, xu1),...]
    fprime : callable `f(x,*args)`
        A function that evaluates the partial derivatives of func.
    fprime_eqcons : callable `f(x,*args)`
=======
    x0 : 1-D ndarray of float
        Initial guess for the independent variable(s).
    eqcons : list, optional
        A list of functions of length n such that
        eqcons[j](x,*args) == 0.0 in a successfully optimized
        problem.
    f_eqcons : callable f(x,*args), optional
        Returns a 1-D array in which each element must equal 0.0 in a
        successfully optimized problem.  If f_eqcons is specified,
        eqcons is ignored.
    ieqcons : list, optional
        A list of functions of length n such that
        ieqcons[j](x,*args) >= 0.0 in a successfully optimized
        problem.
    f_ieqcons : callable f(x,*args), optional
        Returns a 1-D ndarray in which each element must be greater or
        equal to 0.0 in a successfully optimized problem.  If
        f_ieqcons is specified, ieqcons is ignored.
    bounds : list, optional
        A list of tuples specifying the lower and upper bound
        for each independent variable [(xl0, xu0),(xl1, xu1),...]
        Infinite values will be interpreted as large floating values.
    fprime : callable `f(x,*args)`, optional
        A function that evaluates the partial derivatives of func.
    fprime_eqcons : callable `f(x,*args)`, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        A function of the form `f(x, *args)` that returns the m by n
        array of equality constraint normals.  If not provided,
        the normals will be approximated. The array returned by
        fprime_eqcons should be sized as ( len(eqcons), len(x0) ).
<<<<<<< HEAD
    fprime_ieqcons : callable `f(x,*args)`
=======
    fprime_ieqcons : callable `f(x,*args)`, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        A function of the form `f(x, *args)` that returns the m by n
        array of inequality constraint normals.  If not provided,
        the normals will be approximated. The array returned by
        fprime_ieqcons should be sized as ( len(ieqcons), len(x0) ).
<<<<<<< HEAD
    args : sequence
        Additional arguments passed to func and fprime.
    iter : int
        The maximum number of iterations.
    acc : float
        Requested accuracy.
    iprint : int
=======
    args : sequence, optional
        Additional arguments passed to func and fprime.
    iter : int, optional
        The maximum number of iterations.
    acc : float, optional
        Requested accuracy.
    iprint : int, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        The verbosity of fmin_slsqp :

        * iprint <= 0 : Silent operation
        * iprint == 1 : Print summary upon completion (default)
        * iprint >= 2 : Print status of each iterate and summary
<<<<<<< HEAD
    full_output : bool
        If False, return only the minimizer of func (default).
        Otherwise, output final objective function and summary
        information.
    epsilon : float
        The step size for finite-difference derivative estimates.

    Returns
    -------
    x : ndarray of float
=======
    disp : int, optional
        Over-rides the iprint interface (preferred).
    full_output : bool, optional
        If False, return only the minimizer of func (default).
        Otherwise, output final objective function and summary
        information.
    epsilon : float, optional
        The step size for finite-difference derivative estimates.
    callback : callable, optional
        Called after each iteration, as ``callback(x)``, where ``x`` is the
        current parameter vector.

    Returns
    -------
    out : ndarray of float
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        The final minimizer of func.
    fx : ndarray of float, if full_output is true
        The final value of the objective function.
    its : int, if full_output is true
        The number of iterations.
    imode : int, if full_output is true
        The exit mode from the optimizer (see below).
    smode : string, if full_output is true
        Message describing the exit mode from the optimizer.

<<<<<<< HEAD
=======
    See also
    --------
    minimize: Interface to minimization algorithms for multivariate
        functions. See the 'SLSQP' `method` in particular.

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    Notes
    -----
    Exit modes are defined as follows ::

        -1 : Gradient evaluation required (g & a)
         0 : Optimization terminated successfully.
         1 : Function evaluation required (f & c)
         2 : More equality constraints than independent variables
         3 : More than 3*n iterations in LSQ subproblem
         4 : Inequality constraints incompatible
         5 : Singular matrix E in LSQ subproblem
         6 : Singular matrix C in LSQ subproblem
         7 : Rank-deficient equality constraint subproblem HFTI
         8 : Positive directional derivative for linesearch
         9 : Iteration limit exceeded

    Examples
    --------
    Examples are given :ref:`in the tutorial <tutorial-sqlsp>`.

    """
<<<<<<< HEAD

    exit_modes = { -1 : "Gradient evaluation required (g & a)",
                    0 : "Optimization terminated successfully.",
                    1 : "Function evaluation required (f & c)",
                    2 : "More equality constraints than independent variables",
                    3 : "More than 3*n iterations in LSQ subproblem",
                    4 : "Inequality constraints incompatible",
                    5 : "Singular matrix E in LSQ subproblem",
                    6 : "Singular matrix C in LSQ subproblem",
                    7 : "Rank-deficient equality constraint subproblem HFTI",
                    8 : "Positive directional derivative for linesearch",
                    9 : "Iteration limit exceeded" }

    # Now do a lot of function wrapping

    # Wrap func
    feval, func = wrap_function(func, args)
    # Wrap fprime, if provided, or approx_fprime if not
    if fprime:
        geval, fprime = wrap_function(fprime,args)
    else:
        geval, fprime = wrap_function(approx_fprime,(func,epsilon))

    if f_eqcons:
        # Equality constraints provided via f_eqcons
        ceval, f_eqcons = wrap_function(f_eqcons,args)
        if fprime_eqcons:
            # Wrap fprime_eqcons
            geval, fprime_eqcons = wrap_function(fprime_eqcons,args)
        else:
            # Wrap approx_jacobian
            geval, fprime_eqcons = wrap_function(approx_jacobian,
                                                 (f_eqcons,epsilon))
    else:
        # Equality constraints provided via eqcons[]
        eqcons_prime = []
        for i in range(len(eqcons)):
            eqcons_prime.append(None)
            if eqcons[i]:
                # Wrap eqcons and eqcons_prime
                ceval, eqcons[i] = wrap_function(eqcons[i],args)
                geval, eqcons_prime[i] = wrap_function(approx_fprime,
                                                       (eqcons[i],epsilon))

    if f_ieqcons:
        # Inequality constraints provided via f_ieqcons
        ceval, f_ieqcons = wrap_function(f_ieqcons,args)
        if fprime_ieqcons:
            # Wrap fprime_ieqcons
            geval, fprime_ieqcons = wrap_function(fprime_ieqcons,args)
        else:
            # Wrap approx_jacobian
            geval, fprime_ieqcons = wrap_function(approx_jacobian,
                                                  (f_ieqcons,epsilon))
    else:
        # Inequality constraints provided via ieqcons[]
        ieqcons_prime = []
        for i in range(len(ieqcons)):
            ieqcons_prime.append(None)
            if ieqcons[i]:
                # Wrap ieqcons and ieqcons_prime
                ceval, ieqcons[i] = wrap_function(ieqcons[i],args)
                geval, ieqcons_prime[i] = wrap_function(approx_fprime,
                                                        (ieqcons[i],epsilon))

=======
    if disp is not None:
        iprint = disp
    opts = {'maxiter': iter,
            'ftol': acc,
            'iprint': iprint,
            'disp': iprint != 0,
            'eps': epsilon,
            'callback': callback}

    # Build the constraints as a tuple of dictionaries
    cons = ()
    # 1. constraints of the 1st kind (eqcons, ieqcons); no jacobian; take
    #    the same extra arguments as the objective function.
    cons += tuple({'type': 'eq', 'fun': c, 'args': args} for c in eqcons)
    cons += tuple({'type': 'ineq', 'fun': c, 'args': args} for c in ieqcons)
    # 2. constraints of the 2nd kind (f_eqcons, f_ieqcons) and their jacobian
    #    (fprime_eqcons, fprime_ieqcons); also take the same extra arguments
    #    as the objective function.
    if f_eqcons:
        cons += ({'type': 'eq', 'fun': f_eqcons, 'jac': fprime_eqcons,
                  'args': args}, )
    if f_ieqcons:
        cons += ({'type': 'ineq', 'fun': f_ieqcons, 'jac': fprime_ieqcons,
                  'args': args}, )

    res = _minimize_slsqp(func, x0, args, jac=fprime, bounds=bounds,
                          constraints=cons, **opts)
    if full_output:
        return res['x'], res['fun'], res['nit'], res['status'], res['message']
    else:
        return res['x']


def _minimize_slsqp(func, x0, args=(), jac=None, bounds=None,
                    constraints=(),
                    maxiter=100, ftol=1.0E-6, iprint=1, disp=False,
                    eps=_epsilon, callback=None,
                    **unknown_options):
    """
    Minimize a scalar function of one or more variables using Sequential
    Least SQuares Programming (SLSQP).

    Options
    -------
    ftol : float
        Precision goal for the value of f in the stopping criterion.
    eps : float
        Step size used for numerical approximation of the jacobian.
    disp : bool
        Set to True to print convergence messages. If False,
        `verbosity` is ignored and set to 0.
    maxiter : int
        Maximum number of iterations.

    """
    _check_unknown_options(unknown_options)
    fprime = jac
    iter = maxiter
    acc = ftol
    epsilon = eps

    if not disp:
        iprint = 0

    # Constraints are triaged per type into a dictionnary of tuples
    if isinstance(constraints, dict):
        constraints = (constraints, )

    cons = {'eq': (), 'ineq': ()}
    for ic, con in enumerate(constraints):
        # check type
        try:
            ctype = con['type'].lower()
        except KeyError:
            raise KeyError('Constraint %d has no type defined.' % ic)
        except TypeError:
            raise TypeError('Constraints must be defined using a '
                            'dictionary.')
        except AttributeError:
            raise TypeError("Constraint's type must be a string.")
        else:
            if ctype not in ['eq', 'ineq']:
                raise ValueError("Unknown constraint type '%s'." % con['type'])

        # check function
        if 'fun' not in con:
            raise ValueError('Constraint %d has no function defined.' % ic)

        # check jacobian
        cjac = con.get('jac')
        if cjac is None:
            # approximate jacobian function.  The factory function is needed
            # to keep a reference to `fun`, see gh-4240.
            def cjac_factory(fun):
                def cjac(x, *args):
                    return approx_jacobian(x, fun, epsilon, *args)
                return cjac
            cjac = cjac_factory(con['fun'])

        # update constraints' dictionary
        cons[ctype] += ({'fun': con['fun'],
                         'jac': cjac,
                         'args': con.get('args', ())}, )

    exit_modes = {-1: "Gradient evaluation required (g & a)",
                    0: "Optimization terminated successfully.",
                    1: "Function evaluation required (f & c)",
                    2: "More equality constraints than independent variables",
                    3: "More than 3*n iterations in LSQ subproblem",
                    4: "Inequality constraints incompatible",
                    5: "Singular matrix E in LSQ subproblem",
                    6: "Singular matrix C in LSQ subproblem",
                    7: "Rank-deficient equality constraint subproblem HFTI",
                    8: "Positive directional derivative for linesearch",
                    9: "Iteration limit exceeded"}

    # Wrap func
    feval, func = wrap_function(func, args)

    # Wrap fprime, if provided, or approx_jacobian if not
    if fprime:
        geval, fprime = wrap_function(fprime, args)
    else:
        geval, fprime = wrap_function(approx_jacobian, (func, epsilon))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # Transform x0 into an array.
    x = asfarray(x0).flatten()

    # Set the parameters that SLSQP will need
<<<<<<< HEAD
    # meq = The number of equality constraints
    if f_eqcons:
        meq = len(f_eqcons(x))
    else:
        meq = len(eqcons)
    if f_ieqcons:
        mieq = len(f_ieqcons(x))
    else:
        mieq = len(ieqcons)
    # m = The total number of constraints
    m = meq + mieq
    # la = The number of constraints, or 1 if there are no constraints
    la = array([1,m]).max()
=======
    # meq, mieq: number of equality and inequality constraints
    meq = sum(map(len, [atleast_1d(c['fun'](x, *c['args'])) for c in cons['eq']]))
    mieq = sum(map(len, [atleast_1d(c['fun'](x, *c['args'])) for c in cons['ineq']]))
    # m = The total number of constraints
    m = meq + mieq
    # la = The number of constraints, or 1 if there are no constraints
    la = array([1, m]).max()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    # n = The number of independent variables
    n = len(x)

    # Define the workspaces for SLSQP
<<<<<<< HEAD
    n1 = n+1
    mineq = m - meq + n1 + n1
    len_w = (3*n1+m)*(n1+1)+(n1-meq+1)*(mineq+2) + 2*mineq+(n1+mineq)*(n1-meq) \
            + 2*meq + n1 +(n+1)*n/2 + 2*m + 3*n + 3*n1 + 1
=======
    n1 = n + 1
    mineq = m - meq + n1 + n1
    len_w = (3*n1+m)*(n1+1)+(n1-meq+1)*(mineq+2) + 2*mineq+(n1+mineq)*(n1-meq) \
            + 2*meq + n1 + ((n+1)*n)//2 + 2*m + 3*n + 3*n1 + 1
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    len_jw = mineq
    w = zeros(len_w)
    jw = zeros(len_jw)

    # Decompose bounds into xl and xu
<<<<<<< HEAD
    if len(bounds) == 0:
        bounds = [(-1.0E12, 1.0E12) for i in range(n)]
    elif len(bounds) != n:
        raise IndexError, \
        'SLSQP Error:  If bounds is specified, len(bounds) == len(x0)'
    else:
        for i in range(len(bounds)):
            if bounds[i][0] > bounds[i][1]:
                raise ValueError, \
                'SLSQP Error: lb > ub in bounds[' + str(i) +']  ' + str(bounds[4])

    xl = array( [ b[0] for b in bounds ] )
    xu = array( [ b[1] for b in bounds ] )


=======
    if bounds is None or len(bounds) == 0:
        xl, xu = array([-1.0E12]*n), array([1.0E12]*n)
    else:
        bnds = array(bounds, float)
        if bnds.shape[0] != n:
            raise IndexError('SLSQP Error: the length of bounds is not '
                             'compatible with that of x0.')

        bnderr = where(bnds[:, 0] > bnds[:, 1])[0]
        if bnderr.any():
            raise ValueError('SLSQP Error: lb > ub in bounds %s.' %
                             ', '.join(str(b) for b in bnderr))
        xl, xu = bnds[:, 0], bnds[:, 1]

        # filter -inf, inf and NaN values
        infbnd = ~isfinite(bnds)
        xl[infbnd[:, 0]] = -1.0E12
        xu[infbnd[:, 1]] = 1.0E12
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # Initialize the iteration counter and the mode value
    mode = array(0,int)
    acc = array(acc,float)
    majiter = array(iter,int)
    majiter_prev = 0

    # Print the header if iprint >= 2
    if iprint >= 2:
<<<<<<< HEAD
        print "%5s %5s %16s %16s" % ("NIT","FC","OBJFUN","GNORM")

    while 1:

        if mode == 0 or mode == 1: # objective and constraint evaluation requird
=======
        print("%5s %5s %16s %16s" % ("NIT","FC","OBJFUN","GNORM"))

    while 1:

        if mode == 0 or mode == 1:  # objective and constraint evaluation requird
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

            # Compute objective function
            fx = func(x)
            # Compute the constraints
<<<<<<< HEAD
            if f_eqcons:
                c_eq = f_eqcons(x)
            else:
                c_eq = array([ eqcons[i](x) for i in range(meq) ])
            if f_ieqcons:
                c_ieq = f_ieqcons(x)
            else:
                c_ieq = array([ ieqcons[i](x) for i in range(len(ieqcons)) ])

            # Now combine c_eq and c_ieq into a single matrix
            if m == 0:
                # no constraints
                c = zeros([la])
            else:
                # constraints exist
                if meq > 0 and mieq == 0:
                    # only equality constraints
                    c = c_eq
                if meq == 0 and mieq > 0:
                    # only inequality constraints
                    c = c_ieq
                if meq > 0 and mieq > 0:
                    # both equality and inequality constraints exist
                    c = append(c_eq, c_ieq)

        if mode == 0 or mode == -1: # gradient evaluation required
=======
            if cons['eq']:
                c_eq = concatenate([atleast_1d(con['fun'](x, *con['args']))
                                     for con in cons['eq']])
            else:
                c_eq = zeros(0)
            if cons['ineq']:
                c_ieq = concatenate([atleast_1d(con['fun'](x, *con['args']))
                                     for con in cons['ineq']])
            else:
                c_ieq = zeros(0)

            # Now combine c_eq and c_ieq into a single matrix
            c = concatenate((c_eq, c_ieq))

        if mode == 0 or mode == -1:  # gradient evaluation required
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

            # Compute the derivatives of the objective function
            # For some reason SLSQP wants g dimensioned to n+1
            g = append(fprime(x),0.0)

            # Compute the normals of the constraints
<<<<<<< HEAD
            if fprime_eqcons:
                a_eq = fprime_eqcons(x)
            else:
                a_eq = zeros([meq,n])
                for i in range(meq):
                    a_eq[i] = eqcons_prime[i](x)

            if fprime_ieqcons:
                a_ieq = fprime_ieqcons(x)
            else:
                a_ieq = zeros([mieq,n])
                for i in range(mieq):
                    a_ieq[i] = ieqcons_prime[i](x)

            # Now combine a_eq and a_ieq into a single a matrix
            if m == 0:
                # no constraints
                a = zeros([la,n])
            elif meq > 0 and mieq == 0:
                # only equality constraints
                a = a_eq
            elif meq == 0 and mieq > 0:
                # only inequality constraints
                a = a_ieq
            elif meq > 0 and mieq > 0:
                # both equality and inequality constraints exist
                a = vstack((a_eq,a_ieq))
=======
            if cons['eq']:
                a_eq = vstack([con['jac'](x, *con['args'])
                               for con in cons['eq']])
            else:  # no equality constraint
                a_eq = zeros((meq, n))

            if cons['ineq']:
                a_ieq = vstack([con['jac'](x, *con['args'])
                                for con in cons['ineq']])
            else:  # no inequality constraint
                a_ieq = zeros((mieq, n))

            # Now combine a_eq and a_ieq into a single a matrix
            if m == 0:  # no constraints
                a = zeros((la, n))
            else:
                a = vstack((a_eq, a_ieq))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            a = concatenate((a,zeros([la,1])),1)

        # Call SLSQP
        slsqp(m, meq, x, xl, xu, fx, c, g, a, acc, majiter, mode, w, jw)

<<<<<<< HEAD
        # Print the status of the current iterate if iprint > 2 and the
        # major iteration has incremented
        if iprint >= 2 and majiter > majiter_prev:
            print "%5i %5i % 16.6E % 16.6E" % (majiter,feval[0],
                                               fx,linalg.norm(g))
=======
        # call callback if major iteration has incremented
        if callback is not None and majiter > majiter_prev:
            callback(x)

        # Print the status of the current iterate if iprint > 2 and the
        # major iteration has incremented
        if iprint >= 2 and majiter > majiter_prev:
            print("%5i %5i % 16.6E % 16.6E" % (majiter,feval[0],
                                               fx,linalg.norm(g)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # If exit mode is not -1 or 1, slsqp has completed
        if abs(mode) != 1:
            break

        majiter_prev = int(majiter)

    # Optimization loop complete.  Print status if requested
    if iprint >= 1:
<<<<<<< HEAD
        print exit_modes[int(mode)] + "    (Exit mode " + str(mode) + ')'
        print "            Current function value:", fx
        print "            Iterations:", majiter
        print "            Function evaluations:", feval[0]
        print "            Gradient evaluations:", geval[0]

    if not full_output:
        return x
    else:
        return [list(x),
                float(fx),
                int(majiter),
                int(mode),
                exit_modes[int(mode)] ]
=======
        print(exit_modes[int(mode)] + "    (Exit mode " + str(mode) + ')')
        print("            Current function value:", fx)
        print("            Iterations:", majiter)
        print("            Function evaluations:", feval[0])
        print("            Gradient evaluations:", geval[0])

    return OptimizeResult(x=x, fun=fx, jac=g, nit=int(majiter), nfev=feval[0],
                          njev=geval[0], status=int(mode),
                          message=exit_modes[int(mode)], success=(mode == 0))


if __name__ == '__main__':

    # objective function
    def fun(x, r=[4, 2, 4, 2, 1]):
        """ Objective function """
        return exp(x[0]) * (r[0] * x[0]**2 + r[1] * x[1]**2 +
                            r[2] * x[0] * x[1] + r[3] * x[1] +
                            r[4])

    # bounds
    bnds = array([[-inf]*2, [inf]*2]).T
    bnds[:, 0] = [0.1, 0.2]

    # constraints
    def feqcon(x, b=1):
        """ Equality constraint """
        return array([x[0]**2 + x[1] - b])

    def jeqcon(x, b=1):
        """ Jacobian of equality constraint """
        return array([[2*x[0], 1]])

    def fieqcon(x, c=10):
        """ Inequality constraint """
        return array([x[0] * x[1] + c])

    def jieqcon(x, c=10):
        """ Jacobian of Inequality constraint """
        return array([[1, 1]])

    # constraints dictionaries
    cons = ({'type': 'eq', 'fun': feqcon, 'jac': jeqcon, 'args': (1, )},
          {'type': 'ineq', 'fun': fieqcon, 'jac': jieqcon, 'args': (10,)})

    # Bounds constraint problem
    print(' Bounds constraints '.center(72, '-'))
    print(' * fmin_slsqp')
    x, f = fmin_slsqp(fun, array([-1, 1]), bounds=bnds, disp=1,
                      full_output=True)[:2]
    print(' * _minimize_slsqp')
    res = _minimize_slsqp(fun, array([-1, 1]), bounds=bnds,
                          **{'disp': True})

    # Equality and inequality constraints problem
    print(' Equality and inequality constraints '.center(72, '-'))
    print(' * fmin_slsqp')
    x, f = fmin_slsqp(fun, array([-1, 1]),
                      f_eqcons=feqcon, fprime_eqcons=jeqcon,
                      f_ieqcons=fieqcon, fprime_ieqcons=jieqcon,
                      disp=1, full_output=True)[:2]
    print(' * _minimize_slsqp')
    res = _minimize_slsqp(fun, array([-1, 1]), constraints=cons,
                          **{'disp': True})
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
