# Automatic first-order derivatives
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-6-12
#

"""
Automatic differentiation for functions with any number of variables

Instances of the class DerivVar represent the values of a function and
its partial X{derivatives} with respect to a list of variables. All
common mathematical operations and functions are available for these
numbers. There is no restriction on the type of the numbers fed into
the code; it works for real and complex numbers as well as for any
Python type that implements the necessary operations.

This module is as far as possible compatible with the n-th order
derivatives module Derivatives. If only first-order derivatives
are required, this module is faster than the general one.

Example::

  print sin(DerivVar(2))

produces the output::

  (0.909297426826, [-0.416146836547])

The first number is the value of sin(2); the number in the following
list is the value of the derivative of sin(x) at x=2, i.e. cos(2).

When there is more than one variable, DerivVar must be called with
an integer second argument that specifies the number of the variable.

Example::

    >>>x = DerivVar(7., 0)
    >>>y = DerivVar(42., 1)
    >>>z = DerivVar(pi, 2)
    >>>print (sqrt(pow(x,2)+pow(y,2)+pow(z,2)))

    produces the output

    >>>(42.6950770511, [0.163953328662, 0.98371997197, 0.0735820818365])

The numbers in the list are the partial derivatives with respect
to x, y, and z, respectively.

Note: It doesn't make sense to use DerivVar with different values
for the same variable index in one calculation, but there is
no check for this. I.e.::

    >>>print DerivVar(3, 0)+DerivVar(5, 0)

    produces

    >>>(8, [2])

but this result is meaningless.
"""


from Scientific import N; Numeric = N


# The following class represents variables with derivatives:

class DerivVar:

    """
    Numerical variable with automatic derivatives of first order
    """

    """Variable with derivatives

    Constructor: DerivVar(|value|, |index| = 0)

    Arguments:

    |value| -- the numerical value of the variable

    |index| -- the variable index (an integer), which serves to
               distinguish between variables and as an index for
               the derivative lists. Each explicitly created
               instance of DerivVar must have a unique index.

    Indexing with an integer yields the derivatives of the corresponding
    order.
    """

    def __init__(self, value, index=0, order=1):
        """
        @param value: the numerical value of the variable
        @type value: number
        @param index: the variable index, which serves to
            distinguish between variables and as an index for
            the derivative lists. Each explicitly created
            instance of DerivVar must have a unique index.
        @type index: C{int}
        @param order: the derivative order, must be zero or one
        @type order: C{int}
        @raise ValueError: if order < 0 or order > 1
        """
        if order < 0 or order > 1:
            raise ValueError('Only first-order derivatives')
        self.value = value
        if order == 0:
            self.deriv = []
        elif type(index) == type([]):
            self.deriv = index
        else:
            self.deriv = index*[0] + [1]

    def __getitem__(self, order):
        """
        @param order: derivative order
        @type order: C{int}
        @return: a list of all derivatives of the given order
        @rtype: C{list}
        @raise ValueError: if order < 0 or order > 1
        """
        if order < 0 or order > 1:
            raise ValueError('Index out of range')
        if order == 0:
            return self.value
        else:
            return self.deriv

    def __repr__(self):
        return `(self.value, self.deriv)`

    def __str__(self):
        return str((self.value, self.deriv))

    def __coerce__(self, other):
        if isDerivVar(other):
            return self, other
        else:
            return self, DerivVar(other, [])

    def __cmp__(self, other):
        return cmp(self.value, other.value)

    def __neg__(self):
        return DerivVar(-self.value, map(lambda a: -a, self.deriv))

    def __pos__(self):
        return self

    def __abs__(self): # cf maple signum # derivate of abs
        absvalue = abs(self.value)
        return DerivVar(absvalue, map(lambda a, d=self.value/absvalue:
                                      d*a, self.deriv))
    def __nonzero__(self):
        return self.value != 0

    def __add__(self, other):
        return DerivVar(self.value + other.value,
                        _mapderiv(lambda a,b: a+b, self.deriv, other.deriv))
    __radd__ = __add__

    def __sub__(self, other):
        return DerivVar(self.value - other.value,
                        _mapderiv(lambda a,b: a-b, self.deriv, other.deriv))

    def __rsub__(self, other):
        return DerivVar(other.value - self.value,
                        _mapderiv(lambda a,b: a-b, other.deriv, self.deriv))

    def __mul__(self, other):
        return DerivVar(self.value*other.value,
                        _mapderiv(lambda a,b: a+b,
                                  map(lambda x,f=other.value:f*x, self.deriv),
                                  map(lambda x,f=self.value:f*x, other.deriv)))
    __rmul__ = __mul__

    def __div__(self, other):
        if not other.value:
            raise ZeroDivisionError('DerivVar division')
        inv = 1./other.value
        return DerivVar(self.value*inv,
                        _mapderiv(lambda a,b: a-b,
                                  map(lambda x,f=inv: f*x, self.deriv),
                                  map(lambda x,f=self.value*inv*inv: f*x,
                                      other.deriv)))
    def __rdiv__(self, other):
        return other/self

    def __pow__(self, other, z=None):
        if z is not None:
            raise TypeError('DerivVar does not support ternary pow()')
        val1 = pow(self.value, other.value-1)
        val = val1*self.value
        deriv1 = map(lambda x, f=val1*other.value: f*x, self.deriv)
        if isDerivVar(other) and len(other.deriv) > 0:
            deriv2 = map(lambda x, f=val*Numeric.log(self.value): f*x,
                         other.deriv)
            return DerivVar(val, _mapderiv(lambda a,b: a+b, deriv1, deriv2))
        else:
            return DerivVar(val, deriv1)

    def __rpow__(self, other):
        return pow(other, self)

    def exp(self):
        v = Numeric.exp(self.value)
        return DerivVar(v, map(lambda x, f=v: f*x, self.deriv))

    def log(self):
        v = Numeric.log(self.value)
        d = 1./self.value
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def log10(self):
        v = Numeric.log10(self.value)
        d = 1./(self.value * Numeric.log(10))
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def sqrt(self):
        v = Numeric.sqrt(self.value)
        d = 0.5/v
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def sign(self):
        if self.value == 0:
            raise ValueError("can't differentiate sign() at zero")
        return DerivVar(Numeric.sign(self.value), 0)

    def sin(self):
        v = Numeric.sin(self.value)
        d = Numeric.cos(self.value)
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def cos(self):
        v = Numeric.cos(self.value)
        d = -Numeric.sin(self.value)
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def tan(self):
        v = Numeric.tan(self.value)
        d = 1.+pow(v,2)
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def sinh(self):
        v = Numeric.sinh(self.value)
        d = Numeric.cosh(self.value)
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def cosh(self):
        v = Numeric.cosh(self.value)
        d = Numeric.sinh(self.value)
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def tanh(self):
        v = Numeric.tanh(self.value)
        d = 1./pow(Numeric.cosh(self.value),2)
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def arcsin(self):
        v = Numeric.arcsin(self.value)
        d = 1./Numeric.sqrt(1.-pow(self.value,2))
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def arccos(self):
        v = Numeric.arccos(self.value)
        d = -1./Numeric.sqrt(1.-pow(self.value,2))
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def arctan(self):
        v = Numeric.arctan(self.value)
        d = 1./(1.+pow(self.value,2))
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

    def arctan2(self, other):
        den = self.value*self.value+other.value*other.value
        s = self.value/den
        o = other.value/den
        return DerivVar(Numeric.arctan2(self.value, other.value),
                        _mapderiv(lambda a, b: a-b,
                                  map(lambda x, f=o: f*x, self.deriv),
                                  map(lambda x, f=s: f*x, other.deriv)))

    def gamma(self):
        from transcendental import gamma, psi
        v = gamma(self.value)
        d = v*psi(self.value)
        return DerivVar(v, map(lambda x, f=d: f*x, self.deriv))

# Type check

def isDerivVar(x):
    """
    @param x: an arbitrary object
    @return: True if x is a DerivVar object, False otherwise
    @rtype: bool
    """
    return hasattr(x,'value') and hasattr(x,'deriv')

# Map a binary function on two first derivative lists

def _mapderiv(func, a, b):
    nvars = max(len(a), len(b))
    a = a + (nvars-len(a))*[0]
    b = b + (nvars-len(b))*[0]
    return map(func, a, b)


# Define vector of DerivVars

def DerivVector(x, y, z, index=0):

    """
    @param x: x component of the vector
    @type x: number
    @param y: y component of the vector
    @type y: number
    @param z: z component of the vector
    @type z: number
    @param index: the DerivVar index for the x component. The y and z
                  components receive consecutive indices.
    @type index: C{int}
    @return: a vector whose components are DerivVar objects
    @rtype: L{Scientific.Geometry.Vector}
    """

    from Scientific.Geometry.VectorModule import Vector
    if isDerivVar(x) and isDerivVar(y) and isDerivVar(z):
        return Vector(x, y, z)
    else:
        return Vector(DerivVar(x, index),
                      DerivVar(y, index+1),
                      DerivVar(z, index+2))
