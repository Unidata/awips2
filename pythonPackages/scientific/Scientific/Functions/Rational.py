# This module defines a univariate rational function class
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-6-12
#

"""
Rational functions in one variable
"""

from Polynomial import Polynomial
from Scientific import N; Numeric = N

class RationalFunction:

    """Rational Function

    Instances of this class represent rational functions
    in a single variable. They can be evaluated like functions.

    Rational functions support addition, subtraction, multiplication,
    and division.
    """

    def __init__(self, numerator, denominator=[1.]):
        """
        @param numerator: polynomial in one variable, or a list
            of polynomial coefficients
        @type numerator: L{Scientific.Functions.Polynomial.Polynomial}
            or C{list} of numbers
        @param denominator: polynomial in one variable, or a list
            of polynomial coefficients
        @type denominator: L{Scientific.Functions.Polynomial.Polynomial}
            or C{list} of numbers
        """
        if hasattr(numerator, 'is_polynomial'):
            self.numerator = numerator
        else:
            self.numerator = Polynomial(numerator)
        if hasattr(denominator, 'is_polynomial'):
            self.denominator = denominator
        else:
            self.denominator = Polynomial(denominator)
        self._normalize()

    is_rational_function = 1

    def __call__(self, value):
        return self.numerator(value)/self.denominator(value)

    def __repr__(self):
        return "RationalFunction(%s,%s)" % (repr(list(self.numerator.coeff)),
                                            repr(list(self.denominator.coeff)))

    def _normalize(self):
        self.numerator = self._truncate(self.numerator)
        self.denominator = self._truncate(self.denominator)
        n = 0
        while 1:
            if self.numerator.coeff[n] != 0:
                break
            if self.denominator.coeff[n] != 0:
                break
            n = n + 1
            if n == len(self.numerator.coeff):
                break
        if n > 0:
            self.numerator = Polynomial(self.numerator.coeff[n:])
            self.denominator = Polynomial(self.denominator.coeff[n:])
        factor = self.denominator.coeff[-1]
        if factor != 1.:
            self.numerator = self.numerator/factor
            self.denominator = self.denominator/factor

    def _truncate(self, poly):
        if poly.coeff[-1] != 0.:
            return poly
        coeff = poly.coeff
        while len(coeff) > 1 and coeff[-1] == 0.:
            coeff = coeff[:-1]
        return Polynomial(coeff)

    def __coerce__(self, other):
        if hasattr(other, 'is_rational_function'):
            return (self, other)
        elif hasattr(other, 'is_polynomial'):
            return (self, RationalFunction(other, [1.]))
        else:
            return (self, RationalFunction([other], [1.]))

    def __mul__(self, other):
        return RationalFunction(self.numerator*other.numerator,
                                self.denominator*other.denominator)
    __rmul__ = __mul__

    def __div__(self, other):
        return RationalFunction(self.numerator*other.denominator,
                                self.denominator*other.numerator)

    def __rdiv__(self, other):
        return RationalFunction(other.numerator*self.denominator,
                                other.denominator*self.numerator)

    def __add__(self, other):
        return RationalFunction(self.numerator*other.denominator+
                                self.denominator*other.numerator,
                                self.denominator*other.denominator)
    __radd__ = __add__

    def __sub__(self, other):
        return RationalFunction(self.numerator*other.denominator-
                                self.denominator*other.numerator,
                                self.denominator*other.denominator)

    def __rsub__(self, other):
        return RationalFunction(other.numerator*self.denominator-
                                other.denominator*self.numerator,
                                self.denominator*other.denominator)

    def divide(self, shift=0):
        """
        @param shift: the power of the independent variable by which
            the numerator is multiplied prior to division
        @type shift: C{int} (non-negative)
        @return: a polynomial and a rational function such that the
            sum of the two is equal to the original rational function. The
            returned rational function's numerator is of lower order than
            its denominator.
        @rtype: (L{Scientific.Functions.Polynomial.Polynomial},
            L{RationalFunction})
        """
        num = Numeric.array(self.numerator.coeff, copy=1)
        if shift > 0:
            num = Numeric.concatenate((shift*[0.], num))
        den = self.denominator.coeff
        den_order = len(den)
        coeff = []
        while len(num) >= den_order:
            q = num[-1]/den[-1]
            coeff.append(q)
            num[-den_order:] = num[-den_order:]-q*den
            num = num[:-1]
        if not coeff:
            coeff = [0]
        coeff.reverse()
        if len(num) == 0:
            num = [0]
        return Polynomial(coeff), RationalFunction(num, den)

    def zeros(self):
        """
        Find the X{zeros} (X{roots}) of the numerator by diagonalization
        of the associated Frobenius matrix.

        @returns: an array containing the zeros
        @rtype: C{Numeric.array}
        """
        return self.numerator.zeros()

    def poles(self):
        """
        Find the X{poles} (zeros of the denominator) by diagonalization
        of the associated Frobenius matrix.

        @returns: an array containing the poles
        @rtype: C{Numeric.array}
        """
        return self.denominator.zeros()


# Test code

if __name__ == '__main__':

    p = Polynomial([1., 1.])
    invp = 1./p
    pr = RationalFunction(p)
    print pr+invp

