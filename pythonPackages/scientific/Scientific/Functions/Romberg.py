# Romberg quadratures for numeric integration.
#
# Written by Scott M. Ransom <ransom@cfa.harvard.edu>
# last revision: 14 Nov 98
#
# Cosmetic changes by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-11-23
#

"""
Numerical integration using the Romberg algorithm
"""

def trapezoid(function, interval, numtraps):
    """
    Numerical X{integration} using the X{trapezoidal} rule
    
    Example::

      >>>from Scientific.Functions.Romberg import trapezoid
      >>>from Scientific.N import pi, tan
      >>>trapezoid(tan, (0.0, pi/3.0), 100)

      yields 0.69317459482518262

    @param function: a function of one variable
    @type function: callable
    @param interval: the lower and upper limit of the integration interval
    @type interval: sequence of two C{float}s
    @param numtraps: the number of trapezoids
    @type numtraps: C{int}
    @returns: the numerical integral of the function over the interval
    @rtype: number
    """
    lox, hix = interval
    h = float(hix-lox)/numtraps
    sum = 0.5*(function(lox)+function(hix))
    for i in range(1, numtraps):
        sum = sum + function(lox + i*h)
    return h*sum

def _difftrap(function, interval, numtraps):
    # Perform part of the trapezoidal rule to integrate a function.
    # Assume that we had called _difftrap with all lower powers-of-2
    # starting with 1.  Calling _difftrap only returns the summation
    # of the new ordinates.  It does _not_ multiply by the width
    # of the trapezoids.  This must be performed by the caller.
    #     'function' is the function to evaluate.
    #     'interval' is a sequence with lower and upper limits
    #                of integration.
    #     'numtraps' is the number of trapezoids to use (must be a
    #                power-of-2).
    if numtraps<=0:
        print "numtraps must be > 0 in _difftrap()."
        return
    elif numtraps==1:
        return 0.5*(function(interval[0])+function(interval[1]))
    else:
        numtosum = numtraps/2
        h = float(interval[1]-interval[0])/numtosum
        lox = interval[0] + 0.5 * h;
        sum = 0.0
        for i in range(0, numtosum):
            sum = sum + function(lox + i*h)
        return sum

def _romberg_diff(b, c, k):
    # Compute the differences for the Romberg quadrature corrections.
    # See Forman Acton's "Real Computing Made Real," p 143.
    tmp = 4.0**k
    return (tmp * c - b)/(tmp - 1.0)

def romberg(function, interval, accuracy=1.0E-7):
    """
    Numerical X{integration} using the X{Romberg} method
    
    Example::

      >>>from Scientific.Functions.Romberg import romberg
      >>>from Scientific.N import pi, tan
      >>>romberg(tan, (0.0, pi/3.0))

      yields '0.693147180562'

    @param function: a function of one variable
    @type function: callable
    @param interval: the lower and upper limit of the integration interval
    @type interval: sequence of two C{float}s
    @param accuracy: convergence criterion (absolute error)
    @type accuracy: C{float}
    @returns: the numerical integral of the function over the interval
    @rtype: number
    """
    i = n = 1
    intrange = interval[1] - interval[0]
    ordsum = _difftrap(function, interval, n)
    result = intrange * ordsum
    resmat = [[result]]
    lastresult = result + accuracy * 2.0
    while (abs(result - lastresult) > accuracy):
        n = n * 2
        ordsum = ordsum + _difftrap(function, interval, n)
        resmat.append([])
        resmat[i].append(intrange * ordsum / n)
        for k in range(i):
            resmat[i].append(_romberg_diff(resmat[i-1][k],
                                          resmat[i][k], k+1))
        result = resmat[i][i]
        lastresult = resmat[i-1][i-1]
        i = i + 1
    return result

# Test code

if __name__ == '__main__':

    from math import tan, pi
    print ''
    r = romberg(tan, (0.5, 1.0))
    t = trapezoid(tan, (0.5, 1.0), 1000)
    theo = 0.485042229942291
    print ''
    print 'Trapezoidal rule with 1000 function evaluations  =', t
    print 'Theoretical result (correct to all shown digits) = %15.14f' % theo
    print ''
