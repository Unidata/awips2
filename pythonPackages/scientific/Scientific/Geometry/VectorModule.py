# This module defines 3d geometrical vectors with the standard
# operations on them. The elements are stored in an
# array.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2008-8-27
#

from Scientific import N; Numeric = N


class Vector:

    """Vector in 3D space

    Constructor:


    Vectors support the usual arithmetic operations
    ('v1', 'v2': vectors, 's': scalar): 

     -  'v1+v2'           (addition)
     -  'v1-v2'           (subtraction)
     -  'v1*v2'           (scalar product)
     -  's*v1', 'v1*s'    (multiplication with a scalar)
     -  'v1/s'            (division by a scalar)

    The three coordinates can be extracted by indexing.

    Vectors are B{immutable}, i.e. their elements cannot be changed.

    Vector elements can be any objects on which the standard
    arithmetic operations plus the functions sqrt and arccos are defined.
    """

    is_vector = 1

    def __init__(self, x=None, y=None, z=None):
        """
        There are two supported calling patterns:
         1. C{Vector(x, y, z)}
            (from three coordinates)
         2. C{Vector(coordinates)}
            (from any sequence containing three coordinates)
        """
        if x is None:
            self.array = [0.,0.,0.]
        elif y is None and z is None:
            self.array = x
        else:
            self.array = [x,y,z]
        self.array = Numeric.array(self.array)

    def __getstate__(self):
        return list(self.array)

    def __setstate__(self, state):
        self.array = Numeric.array(state)

    def __copy__(self, memo = None):
        return self
    __deepcopy__ = __copy__

    def __repr__(self):
        return 'Vector(%s,%s,%s)' % (`self.array[0]`,\
                                     `self.array[1]`,`self.array[2]`)

    def __str__(self):
        return `list(self.array)`

    def __add__(self, other):
        return Vector(self.array+other.array)
    __radd__ = __add__

    def __neg__(self):
        return Vector(-self.array)

    def __sub__(self, other):
        return Vector(self.array-other.array)

    def __rsub__(self, other):
        return Vector(other.array-self.array)

    def __mul__(self, other):
        from Scientific import Geometry
        if isVector(other):
            return Numeric.add.reduce(self.array*other.array)
        elif Geometry.isTensor(other):
            product = Geometry.Tensor(self.array).dot(other)
            if product.rank == 1:
                return Vector(product.array)
            else:
                return product
        elif hasattr(other, "_product_with_vector"):
            return other._product_with_vector(self)
        else:
            return Vector(Numeric.multiply(self.array, other))

    def __rmul__(self, other):
        from Scientific import Geometry
        if Geometry.isTensor(other):
            product = other.dot(Geometry.Tensor(self.array))
            if product.rank == 1:
                return Vector(product.array)
            else:
                return product
        else:
            return Vector(Numeric.multiply(self.array, other))

    def __div__(self, other):
        if isVector(other):
            raise TypeError("Can't divide by a vector")
        else:
            return Vector(Numeric.divide(self.array,1.*other))
            
    def __rdiv__(self, other):
        raise TypeError("Can't divide by a vector")

    def __cmp__(self, other):
        if isVector(other):
            return cmp(Numeric.add.reduce(abs(self.array-other.array)), 0)
        return NotImplemented

    def __len__(self):
        return 3

    def __getitem__(self, index):
        return self.array[index]

    def x(self):
        """
        @returns: the x coordinate
        @rtype: type of vector elements
        """
        return self.array[0]
    def y(self):
        """
        @returns: the y coordinate
        @rtype: type of vector elements
        """
        return self.array[1]
    def z(self):
        """
        @returns: the z coordinate
        @rtype: type of vector elements
        """
        return self.array[2]

    def length(self):
        """
        @returns: the length (norm) of the vector
        @rtype: type of vector elements
        """
        return Numeric.sqrt(Numeric.add.reduce(self.array*self.array))

    def normal(self):
        """
        @returns: a normalized (length 1) copy of the vector
        @rtype: L{Vector}
        @raises ZeroDivisionError: if vector length is zero
        """
        len = Numeric.sqrt(Numeric.add.reduce(self.array*self.array))
        if len == 0:
            raise ZeroDivisionError("Can't normalize a zero-length vector")
        return Vector(Numeric.divide(self.array, len))

    def cross(self, other):
        """
        @param other: a vector
        @type other: L{Vector}
        @returns: cross product with other
        @rtype: L{Vector}
        """
        if not isVector(other):
            raise TypeError("Cross product with non-vector")
        return Vector(self.array[1]*other.array[2]
                                -self.array[2]*other.array[1],
                      self.array[2]*other.array[0]
                                -self.array[0]*other.array[2],
                      self.array[0]*other.array[1]
                                -self.array[1]*other.array[0])

    def asTensor(self):
        """
        @returns: an equivalent rank-1 tensor object
        @rtype: L{Scientific.Geometry.Tensor}
        """
        from Scientific import Geometry
        return Geometry.Tensor(self.array, 1)

    def dyadicProduct(self, other):
        """
        @param other: a vector or a tensor
        @type other: L{Vector} or L{Scientific.Geometry.Tensor}
        @returns: the dyadic product with other
        @rtype: L{Scientific.Geometry.Tensor}
        @raises TypeError: if other is not a vector or a tensor
        """
        from Scientific import Geometry
        if isVector(other):
            return Geometry.Tensor(self.array[:, N.NewAxis]
                                   * other.array[N.NewAxis, :], 1)
        elif Geometry.isTensor(other):
            return Geometry.Tensor(self.array, 1)*other
        else:
            raise TypeError("Dyadic product with non-vector")
        
    def angle(self, other):
        """
        @param other: a vector
        @type other: L{Vector}
        @returns: the angle to other
        @rtype: C{float}
        @raises TypeError: if other is not a vector
        """
        if not isVector(other):
            raise TypeError("Angle between vector and non-vector")
        cosa = Numeric.add.reduce(self.array*other.array) / \
               Numeric.sqrt(Numeric.add.reduce(self.array*self.array) * \
                            Numeric.add.reduce(other.array*other.array))
        cosa = max(-1.,min(1.,cosa))
        return Numeric.arccos(cosa)


# Type check

def isVector(x):
    """
    @returns: C{True} if x is a L{Vector}
    """
    return hasattr(x,'is_vector')
