# This module defines a class representing quaternions.
# It contains just what is needed for using quaternions as representations
# of rotations in 3d space.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-11-23
#

"""
Quaternions as representations of rotations in 3D space
"""

from Scientific import N; Numeric = N
from Scientific.Geometry import Transformation

class Quaternion:

    """
    Quaternion (hypercomplex number)

    This implementation of quaternions is not complete; only the features
    needed for representing rotation matrices by quaternions are
    implemented.

    Quaternions support addition, subtraction, and multiplication,
    as well as multiplication and division by scalars. Division
    by quaternions is not provided, because quaternion multiplication
    is not associative. Use multiplication by the inverse instead.

    The four components can be extracted by indexing.
    """

    def __init__(self, *data):
        """
        There are two calling patterns:

         - Quaternion(q0, q1, q2, q3)  (from four real components)

         - Quaternion(q)  (from a sequence containing the four components)
        """
        if len(data) == 1:
            self.array = Numeric.array(data[0])
        elif len(data) == 4:
            self.array = Numeric.array(data)

    is_quaternion = 1

    def __getitem__(self, item):
        return self.array[item]

    def __add__(self, other):
        return Quaternion(self.array+other.array)

    def __sub__(self, other):
        return Quaternion(self.array-other.array)

    def __mul__(self, other):
        if isQuaternion(other):
            return Quaternion(Numeric.dot(self.asMatrix(),
                                          other.asMatrix())[:, 0])
        else:
            return Quaternion(self.array*other)

    def __rmul__(self, other):
        if isQuaternion(other):
            raise ValueError('Not yet implemented')
        return Quaternion(self.array*other)

    def __div__(self, other):
        if isQuaternion(other):
            raise ValueError('Division by quaternions is not allowed.')
        return Quaternion(self.array/other)

    def __rdiv__(self, other):
        raise ValueError('Division by quaternions is not allowed.')

    def __repr__(self):
        return 'Quaternion(' + str(list(self.array)) + ')'

    def dot(self, other):
        return Numeric.add.reduce(self.array*other.array)

    def norm(self):
        """
        @returns: the norm
        @rtype: C{float}
        """
        return Numeric.sqrt(self.dot(self))

    def normalized(self):
        """
        @returns: the quaternion scaled such that its norm is 1
        @rtype: L{Quaternion}
        """
        return self/self.norm()

    def inverse(self):
        """
        @returns: the inverse
        @rtype: L{Quaternion}
        """
        import Scientific.LA
        inverse = Scientific.LA.inverse(self.asMatrix())
        return Quaternion(inverse[:, 0])

    def asMatrix(self):
        """
        @returns: a 4x4 matrix representation
        @rtype: C{Numeric.array}
        """
        return Numeric.dot(self._matrix, self.array)
    _matrix = Numeric.zeros((4,4,4))
    _matrix[0,0,0] =  1
    _matrix[0,1,1] = -1
    _matrix[0,2,2] = -1
    _matrix[0,3,3] = -1
    _matrix[1,0,1] =  1
    _matrix[1,1,0] =  1
    _matrix[1,2,3] = -1
    _matrix[1,3,2] =  1
    _matrix[2,0,2] =  1
    _matrix[2,1,3] =  1
    _matrix[2,2,0] =  1
    _matrix[2,3,1] = -1
    _matrix[3,0,3] =  1
    _matrix[3,1,2] = -1
    _matrix[3,2,1] =  1
    _matrix[3,3,0] =  1

    def asRotation(self):
        """
        @returns: the corresponding rotation matrix
        @rtype: L{Scientific.Geometry.Transformation.Rotation}
        @raises ValueError: if the quaternion is not normalized
        """
        if Numeric.fabs(self.norm()-1.) > 1.e-5:
            raise ValueError('Quaternion not normalized')
        d = Numeric.dot(Numeric.dot(self._rot, self.array), self.array)
        return Transformation.Rotation(d)

    _rot = Numeric.zeros((3,3,4,4))
    _rot[0,0, 0,0] =  1
    _rot[0,0, 1,1] =  1
    _rot[0,0, 2,2] = -1
    _rot[0,0, 3,3] = -1
    _rot[1,1, 0,0] =  1
    _rot[1,1, 1,1] = -1
    _rot[1,1, 2,2] =  1
    _rot[1,1, 3,3] = -1
    _rot[2,2, 0,0] =  1
    _rot[2,2, 1,1] = -1
    _rot[2,2, 2,2] = -1
    _rot[2,2, 3,3] =  1
    _rot[0,1, 1,2] =  2
    _rot[0,1, 0,3] = -2
    _rot[0,2, 0,2] =  2
    _rot[0,2, 1,3] =  2
    _rot[1,0, 0,3] =  2
    _rot[1,0, 1,2] =  2
    _rot[1,2, 0,1] = -2
    _rot[1,2, 2,3] =  2
    _rot[2,0, 0,2] = -2
    _rot[2,0, 1,3] =  2
    _rot[2,1, 0,1] =  2
    _rot[2,1, 2,3] =  2


# Type check
def isQuaternion(x):
    """
    @param x: any object
    @type x: any
    @returns: C{True} if x is a quaternion
    """
    return hasattr(x,'is_quaternion')

# Test data

if __name__ == '__main__':

    from Scientific.Geometry import Vector
    axis = Vector(1., -2., 1.).normal()
    phi = 0.2
    sin_phi_2 = Numeric.sin(0.5*phi)
    cos_phi_2 = Numeric.cos(0.5*phi)
    quat = Quaternion(cos_phi_2, sin_phi_2*axis[0],
                      sin_phi_2*axis[1], sin_phi_2*axis[2])
    rot = quat.asRotation()
    print rot.axisAndAngle()
