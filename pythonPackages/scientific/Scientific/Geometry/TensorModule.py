# This module defines 3d geometrical tensors with the standard
# operations on them. The elements are stored in an array.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-11-23
#

from Scientific import N; Numeric = N

class Tensor:

    """Tensor in 3D space

    Tensors support the usual arithmetic operations
    ('t1', 't2': tensors, 'v': vector, 's': scalar): 

     -  't1+t2'        (addition)
     -  't1-t2'        (subtraction)
     -  't1*t2'        (tensorial (outer) product)
     -  't1*v'         (contraction with a vector, same as t1.dot(v.asTensor()))
     -  's*t1', 't1*s' (multiplication with a scalar)
     -  't1/s'         (division by a scalar)

    The coordinates can be extracted by indexing; a tensor of rank N
    can be indexed like an array of dimension N.

    Tensors are I{immutable}, i.e. their elements cannot be changed.

    Tensor elements can be any objects on which the standard
    arithmetic operations are defined. However, eigenvalue calculation
    is supported only for float elements.
    """

    is_tensor = 1

    def __init__(self, elements, nocheck = None):
        """
        @param elements: 2D array or nested list specifying the nine
                         tensor components
                         [[xx, xy, xz], [yx, yy, yz], [zx, zy, zz]]
        @type elements: C{Numeric.array} or C{list}
        """
        self.array = N.array(elements)
        if nocheck is None:
            if not N.logical_and.reduce(N.equal(N.array(self.array.shape), 3)):
                raise ValueError('Tensor must have length 3 along any axis')
        self.rank = len(self.array.shape)

    def __repr__(self):
        return 'Tensor(' + str(self) + ')'

    def __str__(self):
        return str(self.array)

    def __add__(self, other):
        return Tensor(self.array+other.array, 1)
    __radd__ = __add__

    def __neg__(self):
        return Tensor(-self.array, 1)

    def __sub__(self, other):
        return Tensor(self.array-other.array, 1)

    def __rsub__(self, other):
        return Tensor(other.array-self.array, 1)

    def __mul__(self, other):
        from Scientific import Geometry
        if isTensor(other):
            a = self.array[self.rank*(slice(None),)+(N.NewAxis,)]
            b = other.array[other.rank*(slice(None),)+(N.NewAxis,)]
            return Tensor(N.innerproduct(a, b), 1)
        elif Geometry.isVector(other):
            return other.__rmul__(self)
        else:
            return Tensor(self.array*other, 1)

    def __rmul__(self, other):
        return Tensor(self.array*other, 1)

    def __div__(self, other):
        if isTensor(other):
            raise TypeError("Can't divide by a tensor")
        else:
            return Tensor(self.array/(1.*other), 1)

    def __rdiv__(self, other):
        raise TypeError("Can't divide by a tensor")

    def __cmp__(self, other):
        if not isTensor(other):
            return NotImplemented
        if self.rank != other.rank:
            return 1
        else:
            return not N.logical_and.reduce(
                N.equal(self.array, other.array).flat)

    def __len__(self):
        return 3

    def __getitem__(self, index):
        elements = self.array[index]
        if type(elements) == type(self.array):
            return Tensor(elements)
        else:
            return elements

    def asVector(self):
        """
        @returns: an equivalent vector object
        @rtype: L{Scientific.Geometry.Vector}
        @raises ValueError: if rank > 1
        """
        from Scientific import Geometry
        if self.rank == 1:
            return Geometry.Vector(self.array)
        else:
            raise ValueError('rank > 1')

    def dot(self, other):
        """
        @returns: the contraction with other
        @rtype: L{Tensor}
        """
        if isTensor(other):
            a = self.array
            b =  N.transpose(other.array, range(1, other.rank)+[0])
            return Tensor(N.innerproduct(a, b), 1)
        else:
            return Tensor(self.array*other, 1)

    def diagonal(self, axis1=0, axis2=1):
        if self.rank == 2:
            return Tensor([self.array[0,0], self.array[1,1], self.array[2,2]])
        else:
            if axis2 < axis1: axis1, axis2 = axis2, axis1
            raise ValueError('Not yet implemented')

    def trace(self, axis1=0, axis2=1):
        """
        @returns: the trace of the tensor
        @rtype: type of tensor elements
        @raises ValueError: if rank !=2 
        """
        if self.rank == 2:
            return self.array[0,0]+self.array[1,1]+self.array[2,2]
        else:
            raise ValueError('Not yet implemented')

    def transpose(self):
        """
        @returns: the transposed (index reversed) tensor
        @rtype: L{Tensor}
        """
        return Tensor(N.transpose(self.array))

    def symmetricalPart(self):
        """
        @returns: the symmetrical part of the tensor
        @rtype: L{Tensor}
        @raises ValueError: if rank !=2 
        """
        if self.rank == 2:
            return Tensor(0.5*(self.array + \
                               N.transpose(self.array,
                                           N.array([1,0]))),
                          1)
        else:
            raise ValueError('Not yet implemented')

    def asymmetricalPart(self):
        """
        @returns: the asymmetrical part of the tensor
        @rtype: L{Tensor}
        @raises ValueError: if rank !=2 
        """
        if self.rank == 2:
            return Tensor(0.5*(self.array - \
                               N.transpose(self.array,
                                           N.array([1,0]))),
                          1)
        else:
            raise ValueError('Not yet implemented')

    def eigenvalues(self):
        """
        @returns: the eigenvalues of the tensor
        @rtype: C{Numeric.array}
        @raises ValueError: if rank !=2 
        """
        if self.rank == 2:
            from Scientific.LA import eigenvalues
            return eigenvalues(self.array)
        else:
            raise ValueError('Undefined operation')

    def diagonalization(self):
        """
        @returns: the eigenvalues of the tensor and a tensor
                  representing the rotation matrix to the diagonal form
        @rtype: (C{Numeric.array}, L{Tensor})
        @raises ValueError: if rank !=2 
        """
        if self.rank == 2:
            from Scientific.LA import eigenvectors
            ev, vectors = eigenvectors(self.array)
            return ev, Tensor(vectors)
        else:
            raise ValueError, 'Undefined operation'

    def inverse(self):
        """
        @returns: the inverse of the tensor
        @rtype: L{Tensor}
        @raises ValueError: if rank !=2 
        """
        if self.rank == 2:
            from Scientific.LA import inverse
            return Tensor(inverse(self.array))
        else:
            raise ValueError('Undefined operation')

# Type check

def isTensor(x):
    """
    @returns: C{True} if x is a L{Tensor}
    """
    return hasattr(x,'is_tensor')
