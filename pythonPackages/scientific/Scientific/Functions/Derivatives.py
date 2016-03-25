# Automatic nth-order derivatives
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2007-5-25
#

"""
Automatic differentiation for functions of
any number of variables up to any order

An instance of the class DerivVar represents the value of a function
and the values of its partial X{derivatives} with respect to a list of
variables. All common mathematical operations and functions are
available for these numbers. There is no restriction on the type of
the numbers fed into the code; it works for real and complex numbers
as well as for any Python type that implements the necessary
operations.

If only first-order derivatives are required, the module
FirstDerivatives should be used. It is compatible to this
one, but significantly faster.

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

Higher-order derivatives are requested with an optional third
argument to DerivVar.

Example::

    >>>x = DerivVar(3., 0, 3)
    >>>y = DerivVar(5., 1, 3)
    >>>print sqrt(x*y)

    produces the output

    >>>(3.87298334621,
    >>>    [0.645497224368, 0.387298334621],
    >>>      [[-0.107582870728, 0.0645497224368],
    >>>        [0.0645497224368, -0.0387298334621]],
    >>>          [[[0.053791435364, -0.0107582870728],
    >>>            [-0.0107582870728, -0.00645497224368]],
    >>>           [[-0.0107582870728, -0.00645497224368],
    >>>            [-0.00645497224368, 0.0116189500386]]])

The individual orders can be extracted by indexing::

    >>>print sqrt(x*y)[0]
    >>>3.87298334621
    >>>print sqrt(x*y)[1]
    >>>[0.645497224368, 0.387298334621]

An n-th order derivative is represented by a nested list of
depth n.

When variables with different differentiation orders are mixed,
the result has the lower one of the two orders. An exception are
zeroth-order variables, which are treated as constants.

Caution: Higher-order derivatives are implemented by recursively
using DerivVars to represent derivatives. This makes the code
very slow for high orders.

Note: It doesn't make sense to use multiple DerivVar objects with
different values for the same variable index in one calculation, but
there is no check for this. I.e.::

    >>>print DerivVar(3, 0)+DerivVar(5, 0)

    produces

    >>>(8, [2])

but this result is meaningless.
"""


from Scientific import N; Numeric = N


# The following class represents variables with derivatives:

class DerivVar:

    """
    Numerical variable with automatic derivatives of arbitrary order
    """

    def __init__(self, value, index=0, order = 1, recursive = None):
        """
        @param value: the numerical value of the variable
        @type value: number
        @param index: the variable index, which serves to
            distinguish between variables and as an index for
            the derivative lists. Each explicitly created
            instance of DerivVar must have a unique index.
        @type index: C{int}
        @param order: the derivative order
        @type order: C{int}
        @raise ValueError: if order < 0
        """
        if order < 0:
            raise ValueError('Negative derivative order')
        self.value = value
        if recursive:
            d = 0
        else:
            d = 1
        if type(index) == type([]):
            self.deriv = index
        elif order == 0:
            self.deriv = []
        elif order == 1:
            self.deriv = index*[0] + [d]
        else:
            self.deriv = []
            for i in range(index):
                self.deriv.append(DerivVar(0, index, order-1, 1))
            self.deriv.append(DerivVar(d, index, order-1, 1))
        self.order = order

    def toOrder(self, order):
        """
        @param order: the highest derivative order to be kept
        @type order: C{int}
        @return: a DerivVar object with a lower derivative order
        @rtype: L{DerivVar}
        """
        if self.order <= order:
            return self
        if order == 0:
            return self.value
        return DerivVar(self.value, map(lambda x, o=order-1: x.toOrder(o),
                                        self.deriv), order)

    def __getitem__(self, order):
        """
        @param order: derivative order
        @type order: C{int}
        @return: a list of all derivatives of the given order
        @rtype: C{list}
        @raise ValueError: if order < 0 or order > self.order
        """
        if order < 0 or order > self.order:
            raise ValueError('Index out of range')
        if order == 0:
            return self.value
        else:
            return map(lambda d, i=order-1: _indexDeriv(d,i), self.deriv)

    def __repr__(self):
        return repr(tuple(map(lambda n, x=self: x[n], range(self.order+1))))

    def __str__(self):
        return str(tuple(map(lambda n, x=self: x[n], range(self.order+1))))

    def __coerce__(self, other):
        if isDerivVar(other):
            if self.order==other.order or self.order==0 or other.order==0:
                return self, other
            order = min(self.order, other.order)
            return self.toOrder(order), other.toOrder(order)
        else:
            return self, DerivVar(other, [], 0)

    def __cmp__(self, other):
        return cmp(self.value, other.value)

    def __neg__(self):
        return DerivVar(-self.value,map(lambda a: -a, self.deriv), self.order)

    def __pos__(self):
        return self

    def __abs__(self):
        absvalue = abs(self.value)
        return DerivVar(absvalue, map(lambda a, d=self.value/absvalue:
                                      d*a, self.deriv), self.order)

    def __nonzero__(self):
        return self.value != 0

    def __add__(self, other):
        return DerivVar(self.value + other.value,
                        _mapderiv(lambda a,b: a+b, self.deriv, other.deriv),
                        max(self.order, other.order))
    __radd__ = __add__

    def __sub__(self, other):
        return DerivVar(self.value - other.value,
                        _mapderiv(lambda a,b: a-b, self.deriv, other.deriv),
                        max(self.order, other.order))

    def __rsub__(self, other):
        return DerivVar(other.value - self.value,
                        _mapderiv(lambda a,b: a-b, other.deriv, self.deriv),
                        max(self.order, other.order))

    def __mul__(self, other):
        if self.order < 2:
            s1 = self.value
        else:
            s1 = self.toOrder(self.order-1)
        if other.order < 2:
            o1 = other.value
        else:
            o1 = other.toOrder(other.order-1)
        return DerivVar(self.value*other.value,
                        _mapderiv(lambda a,b: a+b,
                                  map(lambda x,f=o1: f*x, self.deriv),
                                  map(lambda x,f=s1: f*x, other.deriv)),
                        max(self.order, other.order))
    __rmul__ = __mul__

    def __div__(self, other):
        if not other.value:
            raise ZeroDivisionError('DerivVar division')
        if self.order < 2:
            s1 = self.value
        else:
            s1 = self.toOrder(self.order-1)
        if other.order < 2:
            o1i = 1./other.value
        else:
            o1i = 1./other.toOrder(other.order-1)
        return DerivVar(_toFloat(self.value)/other.value,
                        _mapderiv(lambda a,b: a-b,
                                  map(lambda x,f=o1i: x*f, self.deriv),
                                  map(lambda x,f=s1*pow(o1i, 2): f*x,
                                      other.deriv)),
                                  max(self.order, other.order))
    def __rdiv__(self, other):
        return other/self

    def __pow__(self, other, z=None):
        if z is not None:
            raise TypeError('DerivVar does not support ternary pow()')
        if len(other.deriv) > 0:
            return Numeric.exp(Numeric.log(self)*other)
        else:
            if self.order < 2:
                ps1 = other.value*pow(self.value, other.value-1)
            else:
                ps1 = other.value*pow(self.toOrder(self.order-1), other.value-1)
            return DerivVar(pow(self.value, other.value),
                            map(lambda x,f=ps1: f*x, self.deriv),
                            max(self.order, other.order))

    def __rpow__(self, other):
        return pow(other, self)

    def _mathfunc(self, f, d):
        if self.order < 2:
            fd = d(self.value)
        else:
            fd = d(self.toOrder(self.order-1))
        return DerivVar(f(self.value), map(lambda x, f=fd: f*x, self.deriv),
                        self.order)

    def exp(self):
        return self._mathfunc(Numeric.exp, Numeric.exp)

    def log(self):
        return self._mathfunc(Numeric.log, lambda x: 1./x)

    def log10(self):
        return self._mathfunc(Numeric.log10, lambda x: 1./(x*Numeric.log(10)))

    def sqrt(self):
        return self._mathfunc(Numeric.sqrt, lambda x: 0.5/Numeric.sqrt(x))

    def sign(self):
        if self.value == 0:
            raise ValueError("can't differentiate sign() at zero")
        return self._mathfunc(Numeric.sign, lambda x: 0)

    def sin(self):
        return self._mathfunc(Numeric.sin, Numeric.cos)

    def cos(self):
        return self._mathfunc(Numeric.cos, lambda x: -Numeric.sin(x))

    def tan(self):
        return self._mathfunc(Numeric.tan, lambda x: 1.+pow(Numeric.tan(x),2))

    def sinh(self):
        return self._mathfunc(Numeric.sinh, Numeric.cosh)

    def cosh(self):
        return self._mathfunc(Numeric.cosh, Numeric.sinh)

    def tanh(self):
        return self._mathfunc(Numeric.tanh, lambda x:
                              1./pow(Numeric.cosh(x),2))

    def arcsin(self):
        return self._mathfunc(Numeric.arcsin, lambda x:
                              1./Numeric.sqrt(1.-pow(x,2)))

    def arccos(self):
        return self._mathfunc(Numeric.arccos, lambda x:
                              -1./Numeric.sqrt(1.-pow(x,2)))

    def arctan(self):
        return self._mathfunc(Numeric.arctan, lambda x:
                              1./(1+pow(x,2)))

    def arctan2(self, other):
        if self.order < 2:
            s1 = self.value
        else:
            s1 = self.toOrder(self.order-1)
        if other.order < 2:
            o1 = other.value
        else:
            o1 = other.toOrder(other.order-1)
        den = s1*s1+o1*o1
        s1 = s1/den
        o1 = o1/den
        return DerivVar(Numeric.arctan2(self.value, other.value),
                        _mapderiv(lambda a,b: a-b,
                                  map(lambda x,f=o1: x*f, self.deriv),
                                  map(lambda x,f=s1: x*f, other.deriv)),
                                  max(self.order, other.order))

# Type check

def isDerivVar(x):
    """
    @param x: an arbitrary object
    @return: True if x is a DerivVar object, False otherwise
    @rtype: C{bool}
    """
    return hasattr(x,'value') and hasattr(x,'deriv') and hasattr(x,'order')


# Map a binary function on two first derivative lists

def _mapderiv(func, a, b):
    nvars = max(len(a), len(b))
    a = a + (nvars-len(a))*[0]
    b = b + (nvars-len(b))*[0]
    return map(func, a, b)

# Convert argument to float if it is integer

def _toFloat(x):
    if isinstance(x, int):
        return float(x)
    return x

# Subscript for DerivVar or ordinary number

def _indexDeriv(d, i):
    if isDerivVar(d):
        return d[i]
    if i != 0:
        raise ValueError('Internal error')
    return d

# Define vector of DerivVars

def DerivVector(x, y, z, index=0, order = 1):

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
    @param order: the derivative order
    @type order: C{int}
    @return: a vector whose components are DerivVar objects
    @rtype: L{Scientific.Geometry.Vector}
    """

    from Scientific.Geometry.VectorModule import Vector
    if isDerivVar(x) and isDerivVar(y) and isDerivVar(z):
        return Vector(x, y, z)
    else:
        return Vector(DerivVar(x, index, order),
                      DerivVar(y, index+1, order),
                      DerivVar(z, index+2, order))
