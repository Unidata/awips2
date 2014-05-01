# 'Safe' Newton-Raphson for numerical root-finding
#
# Written by Scott M. Ransom <ransom@cfa.harvard.edu>
# last revision: 14 Nov 98
#
# Cosmetic changes by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-11-23
#

"""
Newton-Raphson for numerical root finding

Example::

    >>>from Scientific.Functions.FindRoot import newtonRaphson
    >>>from Scientific.N import pi, sin, cos
    >>>def func(x):
    >>>    return (2*x*cos(x) - sin(x))*cos(x) - x + pi/4.0
    >>>newtonRaphson(func, 0.0, 1.0, 1.0e-12)

    yields 0.952847864655.
"""

from FirstDerivatives import DerivVar

def newtonRaphson(function, lox, hix, xacc):
    
    """
    X{Newton-Raphson} algorithm for X{root} finding.
    The algorithm used is a safe version of Newton-Raphson
    (see page 366 of Numerical Recipes in C, 2ed).
    
    @param function: function of one numerical variable that
        uses only those operations that are defined for  DerivVar
        objects in the module L{Scientific.Functions.FirstDerivatives}
    @type function: callable
    @param lox: lower limit of the search interval
    @type lox: C{float}
    @param hix: upper limit of the search interval
    @type hix: C[{loat}
    @param xacc: requested absolute precision of the root
    @type xacc: C{float}
    @returns: a root of function between lox and hix
    @rtype:  C{float}
    @raises ValueError: if C{function(lox)} and C{function(hix)} have
        the same sign, or if there is no convergence after 500 iterations
    """

    maxit = 500
    tmp = function(DerivVar(lox))
    fl = tmp[0]
    tmp = function(DerivVar(hix))
    fh = tmp[0]
    if ((fl > 0.0 and fh > 0.0) or (fl < 0.0 and fh < 0.0)):
        raise ValueError("Root must be bracketed")
    if (fl == 0.0): return lox
    if (fh == 0.0): return hix
    if (fl < 0.0):
        xl=lox
        xh=hix
    else:
        xh=lox
        xl=hix
    rts=0.5*(lox+hix)
    dxold=abs(hix-lox)
    dx=dxold
    tmp = function(DerivVar(rts))
    f = tmp[0]
    df = tmp[1][0]
    for j in range(maxit):
        if ((((rts-xh)*df-f)*((rts-xl)*df-f) > 0.0)
            or (abs(2.0*f) > abs(dxold*df))):
            dxold=dx
            dx=0.5*(xh-xl)
            rts=xl+dx
            if (xl == rts): return rts
        else:
            dxold=dx
            dx=f/df
            temp=rts
            rts=rts-dx
            if (temp == rts): return rts
        if (abs(dx) < xacc): return rts
        tmp = function(DerivVar(rts))
        f = tmp[0]
        df = tmp[1][0]
        if (f < 0.0):
            xl=rts
        else:
            xh=rts
    raise ValueError("Maximum number of iterations exceeded")

# Test code

if __name__ == '__main__':

    from Scientific.Numeric import pi, sin, cos

    def _func(x):
        return ((2*x*cos(x) - sin(x))*cos(x) - x + pi/4.0)
    
    _r = newtonRaphson(_func, 0.0, 1.0, 1.0e-12)
    _theo =  0.9528478646549419474413332
    print ''
    print 'Finding the root (between 0.0 and 1.0) of:'
    print '    (2*x*cos(x) - sin(x))*cos(x) - x + pi/4 = 0'
    print ''
    print 'Safe-style Newton-Raphson gives (xacc = 1.0e-12) =', _r
    print 'Theoretical result (correct to all shown digits) = %15.14f' % _theo
    print ''
