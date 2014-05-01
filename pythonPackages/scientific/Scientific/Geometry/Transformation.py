# This module defines classes that represent coordinate translations,
# rotations, and combinations of translation and rotation.
#
# Written by: Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Contributions from Pierre Legrand <pierre.legrand@synchrotron-soleil.fr>
# last revision: 2008-8-22
# 

"""
Linear transformations in 3D space
"""

from Scientific import Geometry
from Scientific import N; Numeric = N
from math import atan2

#
# Abstract base classes
#
class Transformation:

    """
    Linear coordinate transformation.

    Transformation objects represent linear coordinate transformations
    in a 3D space. They can be applied to vectors, returning another vector.
    If t is a transformation and v is a vector, t(v) returns
    the transformed vector.

    Transformations support composition: if t1 and t2 are transformation
    objects, t1*t2 is another transformation object which corresponds
    to applying t1 B{after} t2.

    This class is an abstract base class. Instances can only be created
    of concrete subclasses, i.e. translations or rotations.
    """

    def __call__(self, vector):
        """
        @param vector: the input vector
        @type vector: L{Scientific.Geometry.Vector}
        @returns: the transformed vector
        @rtype: L{Scientific.Geometry.Vector}
        """
        return NotImplementedError

    def inverse(self):
        """
        @returns: the inverse transformation
        @rtype: L{Transformation}
        """
        return NotImplementedError

#
# Rigid body transformations
#
class RigidBodyTransformation(Transformation):

    """
    Combination of translations and rotations
    """

    def rotation(self):
        """
        @returns: the rotational component
        @rtype: L{Rotation}
        """
        pass

    def translation(self):
        """
        @returns: the translational component. In the case of a mixed
                  rotation/translation, this translation is executed
                  B{after} the rotation.
        @rtype: L{Translation}
        """
        pass

    def screwMotion(self):
        """
        @returns: the four parameters
                  (reference, direction, angle, distance)
                  of a screw-like motion that is equivalent to the
                  transformation. The screw motion consists of a displacement
                  of distance (a C{float}) along direction (a normalized
                  L{Scientific.Geometry.Vector}) plus a rotation of
                  angle radians around an axis pointing along
                  direction and passing through the point reference
                  (a L{Scientific.Geometry.Vector}).
        """
        pass

#
# Pure translation
#
class Translation(RigidBodyTransformation):

    """
    Translational transformation
    """

    def __init__(self, vector):
        """
        @param vector: the displacement vector
        @type vector: L{Scientific.Geometry.Vector}
        """
        self.vector = vector

    is_translation = 1

    def asLinearTransformation(self):
        return LinearTransformation(Geometry.delta, self.vector)

    def __mul__(self, other):
        if hasattr(other, 'is_translation'):
            return Translation(self.vector + other.vector)
        elif hasattr(other, 'is_rotation'):
            return RotationTranslation(other.tensor, self.vector)
        elif hasattr(other, 'is_rotation_translation'):
            return RotationTranslation(other.tensor, other.vector+self.vector)
        else:
            return self.asLinearTransformation()*other.asLinearTransformation()

    def __call__(self, vector):
        return self.vector + vector

    def displacement(self):
        """
        @returns: the displacement vector
        """
        return self.vector

    def rotation(self):
        return Rotation(Geometry.ez, 0.)

    def translation(self):
        return self

    def inverse(self):
        return Translation(-self.vector)

    def screwMotion(self):
        l = self.vector.length()
        if l == 0.:
            return Geometry.Vector(0.,0.,0.), \
                   Geometry.Vector(0.,0.,1.), 0., 0.
        else:
            return Geometry.Vector(0.,0.,0.), self.vector/l, 0., l

#
# Pure rotation
#
class Rotation(RigidBodyTransformation):

    """
    Rotational transformation
    """

    def __init__(self, *args):
        """
        There are two calling patterns: 

         - Rotation(tensor), where tensor is a L{Scientific.Geometry.Tensor}
           of rank 2 containing the rotation matrix.

         - Rotation(axis, angle), where axis is a L{Scientific.Geometry.Vector}
           and angle a number (the angle in radians).       
        """
        if len(args) == 1:
            self.tensor = args[0]
            if not Geometry.isTensor(self.tensor):
                self.tensor = Geometry.Tensor(self.tensor)
        elif len(args) == 2:
            axis, angle = args
            axis = axis.normal()
            projector = axis.dyadicProduct(axis)
            self.tensor = projector - \
                          N.sin(angle)*Geometry.epsilon*axis + \
                          N.cos(angle)*(Geometry.delta-projector)
        else:
            raise TypeError('one or two arguments required')

    is_rotation = 1

    def asLinearTransformation(self):
        return LinearTransformation(self.tensor, Geometry.nullVector)

    def __mul__(self, other):
        if hasattr(other, 'is_rotation'):
            return Rotation(self.tensor.dot(other.tensor))
        elif hasattr(other, 'is_translation'):
            return RotationTranslation(self.tensor, self.tensor*other.vector)
        elif hasattr(other, 'is_rotation_translation'):
            return RotationTranslation(self.tensor.dot(other.tensor),
                                       self.tensor*other.vector)
        else:
            return self.asLinearTransformation()*other.asLinearTransformation()

    def __call__(self,other):
        if hasattr(other,'is_vector'):
            return self.tensor*other
        elif hasattr(other, 'is_tensor') and other.rank == 2:
            _rinv=self.tensor.inverse()
            return _rinv.dot(other.dot(self.tensor))
        elif hasattr(other, 'is_tensor') and other.rank == 1:
            return self.tensor.dot(other)
        else:
            raise ValueError('incompatible object')

    def axisAndAngle(self):
        """
        @returns: the axis (a normalized vector) and angle (in radians).
                  The angle is in the interval (-pi, pi]
        @rtype: (L{Scientific.Geometry.Vector}, C{float})
        """
        asym = -self.tensor.asymmetricalPart()
        axis = Geometry.Vector(asym[1,2], asym[2,0], asym[0,1])
        sine = axis.length()
        if abs(sine) > 1.e-10:
            axis = axis/sine
            projector = axis.dyadicProduct(axis)
            cosine = (self.tensor-projector).trace()/(3.-axis*axis)
            angle = angleFromSineAndCosine(sine, cosine)
        else:
            t = 0.5*(self.tensor+Geometry.delta)
            i = N.argmax(t.diagonal().array)
            axis = (t[i]/N.sqrt(t[i,i])).asVector()
            angle = 0.
            if t.trace() < 2.:
                angle = N.pi
        return axis, angle

    def threeAngles(self, e1, e2, e3, tolerance=1e-7):
        """
        Find three angles a1, a2, a3 such that
        Rotation(a1*e1)*Rotation(a2*e2)*Rotation(a3*e3)
        is equal to the rotation object. e1, e2, and
        e3 are non-zero vectors. There are two solutions, both of which
        are computed.

        @param e1: a rotation axis
        @type e1: L{Scientific.Geometry.Vector}
        @param e2: a rotation axis
        @type e2: L{Scientific.Geometry.Vector}
        @param e3: a rotation axis
        @type e3: L{Scientific.Geometry.Vector}
        @returns: a list containing two arrays of shape (3,),
                  each containing the three angles of one solution
        @rtype: C{list} of C{N.array}
        @raise ValueError: if two consecutive axes are parallel
        """

        # Written by Pierre Legrand (pierre.legrand@synchrotron-soleil.fr)
        #
        # Basically this is a reimplementation of the David
        # Thomas's algorithm [1] described by Gerard Bricogne in [2]:
        #
        # [1] "Modern Equations of Diffractometry. Goniometry." D.J. Thomas
        # Acta Cryst. (1990) A46 Page 321-343.
        #
        # [2] "The ECC Cooperative Programming Workshop on Position-Sensitive
        # Detector Software." G. Bricogne,
        # Computational aspect of Protein Crystal Data Analysis,
        # Proceedings of the Daresbury Study Weekend (23-24/01/1987)
        # Page 122-126

        e1 = e1.normal()
        e2 = e2.normal()
        e3 = e3.normal()

        # We are searching for the three angles a1, a2, a3
        # If 2 consecutive axes are parallel: decomposition is not meaningful
        if (e1.cross(e2)).length() < tolerance or \
           (e2.cross(e3)).length() < tolerance :
            raise ValueError('Consecutive parallel axes. Too many solutions')
        w = self(e3)
        
        # Solve the equation : _a.cosx + _b.sinx = _c
        _a = e1*e3 - (e1*e2)*(e2*e3)
        _b = e1*(e2.cross(e3))
        _c = e1*w - (e1*e2)*(e2*e3)
        _norm = (_a**2 + _b**2)**0.5
        
        # Checking for possible errors in initial Rot matrix
        if _norm == 0:
            raise ValueError('FAILURE 1, norm = 0')
        if abs(_c/_norm) > 1+tolerance:
            raise ValueError('FAILURE 2' +
                              'malformed rotation Tensor (non orthogonal?) %.8f'
                              % (_c/_norm))
        #if _c/_norm > 1: raise ValueError('Step1: No solution')
        _th = angleFromSineAndCosine(_b/_norm, _a/_norm)
        _xmth = N.arccos(_c/_norm)

        # a2a and a2b are the two possible solutions to the equation.
        a2a = mod_angle((_th + _xmth), 2*N.pi)
        a2b = mod_angle((_th - _xmth), 2*N.pi)
        
        solutions = []
        # for each solution, find the two other angles (a1, a3).
        for a2 in (a2a, a2b):
            R2 = Rotation(e2, a2)
            v =  R2(e3)
            v1 = v - (v*e1)*e1
            w1 = w - (w*e1)*e1
            norm = ((v1*v1)*(w1*w1))**0.5
            if norm == 0: 
                # in that case rotation 1 and 3 are about the same axis
                # so any solution for rotation 1 is OK
                a1 = 0.
            else:
                cosa1 = (v1*w1)/norm
                sina1 = v1*(w1.cross(e1))/norm
                a1 = mod_angle(angleFromSineAndCosine(sina1, cosa1),
                               2*N.pi)
                
            R3 = Rotation(e2, -1*a2)*Rotation(e1, -1*a1)*self
            # u = normalized test vector perpendicular to e3
            # if e2 and e3 are // we have an exception before.
            # if we take u = e1^e3 then it will not work for
            # Euler and Kappa axes.
            u = (e2.cross(e3)).normal()
            cosa3 = u*R3(u)
            sina3 = u*(R3(u).cross(e3))
            a3 =  mod_angle(angleFromSineAndCosine(sina3, cosa3),
                            2*N.pi)
            
            solutions.append(N.array([a1, a2, a3]))
            
        # Gives the closest solution to 0,0,0 first
        if N.add.reduce(solutions[0]**2) > \
               N.add.reduce(solutions[1]**2):
            solutions = [solutions[1], solutions[0]]
        return solutions

    def asQuaternion(self):
        """
        @returns: a quaternion representing the same rotation
        @rtype: L{Scientific.Geometry.Quaternion.Quaternion}
        """
        from Quaternion import Quaternion
        axis, angle = self.axisAndAngle()
        sin_angle_2 = N.sin(0.5*angle)
        cos_angle_2 = N.cos(0.5*angle)
        return Quaternion(cos_angle_2, sin_angle_2*axis[0],
                          sin_angle_2*axis[1], sin_angle_2*axis[2])

    def rotation(self):
        return self

    def translation(self):
        return Translation(Geometry.Vector(0.,0.,0.))

    def inverse(self):
        return Rotation(self.tensor.transpose())

    def screwMotion(self):
        axis, angle = self.axisAndAngle()
        return Geometry.Vector(0., 0., 0.), axis, angle, 0.

#
# Combined translation and rotation
#
class RotationTranslation(RigidBodyTransformation):

    """
    Combined translational and rotational transformation.

    Objects of this class are not created directly, but can be the
    result of a composition of rotations and translations.
    """

    def __init__(self, tensor, vector):
        self.tensor = tensor
        self.vector = vector

    is_rotation_translation = 1

    def asLinearTransformation(self):
        return LinearTransformation(self.tensor, self.vector)

    def __mul__(self, other):
        if hasattr(other, 'is_rotation'):
            return RotationTranslation(self.tensor.dot(other.tensor),
                                       self.vector)
        elif hasattr(other, 'is_translation'):
            return RotationTranslation(self.tensor,
                                       self.tensor*other.vector+self.vector)
        elif hasattr(other, 'is_rotation_translation'):
            return RotationTranslation(self.tensor.dot(other.tensor),
                                       self.tensor*other.vector+self.vector)
        else:
            return self.asLinearTransformation()*other.asLinearTransformation()

    def __call__(self, vector):
        return self.tensor*vector + self.vector

    def rotation(self):
        return Rotation(self.tensor)

    def translation(self):
        return Translation(self.vector)

    def inverse(self):
        return Rotation(self.tensor.transpose())*Translation(-self.vector)

#    def screwMotion1(self):
#        import Scientific.LA
#        axis, angle = self.rotation().axisAndAngle()
#        d = self.vector*axis
#        x = d*axis-self.vector
#        r0 = N.dot(Scientific.LA.generalized_inverse(
#                            self.tensor.array-N.identity(3)), x.array)
#        return Geometry.Vector(r0), axis, angle, d

    def screwMotion(self):
        axis, angle = self.rotation().axisAndAngle()
        d = self.vector*axis
        x = d*axis-self.vector
        if abs(angle) < 1.e-9:
            r0 = Geometry.Vector(0., 0., 0.)
            angle = 0.
        else:
            r0 = -0.5*((N.cos(0.5*angle)/N.sin(0.5*angle))*axis.cross(x)+x)
        return r0, axis, angle, d

#
# Scaling
#
class Scaling(Transformation):

    """
    Scaling
    """
    def __init__(self, scale_factor):
        """
        @param scale_factor: the scale factor
        @type scale_factor: C{float}
        """
        self.scale_factor = scale_factor

    is_scaling = 1

    def asLinearTransformation(self):
        return LinearTransformation(self.scale_factor*Geometry.delta,
                                    Geometry.nullVector)

    def __call__(self, vector):
        return self.scale_factor*vector

    def __mul__(self, other):
        if hasattr(other, 'is_scaling'):
            return Scaling(self.scale_factor*other.scale_factor)
        else:
            return self.asLinearTransformation()*other.asLinearTransformation()

    def inverse(self):
        return Scaling(1./self.scale_factor)

#
# Inversion is scaling by -1
#
class Inversion(Scaling):

    def __init__(self):
        Scaling.__init__(self, -1.)

#
# Shear
#
class Shear(Transformation):

    def __init__(self, *args):
        if len(args) == 1:
            if Geometry.isTensor(args[0]):
                self.tensor = args[0]
            else:
                self.tensor = Geometry.Tensor(args[0])
                assert self.tensor.rank == 2
        elif len(args) == 3 and Geometry.isVector(args[0]) \
                 and Geometry.isVector(args[1]) and Geometry.isVector(args[2]):
            self.tensor = Geometry.Tensor([args[0].array, args[1].array,
                                           args[2].array]).transpose()

    def asLinearTransformation(self):
        return LinearTransformation(self.tensor, Geometry.nullVector)

    def __mul__(self, other):
        return self.asLinearTransformation()*other

    def __call__(self, vector):
        return self.tensor*vector

    def inverse(self):
        return Shear(self.tensor.inverse())

#
# General linear transformation
#
class LinearTransformation(Transformation):

    """
    General linear transformation.

    Objects of this class are not created directly, but can be the
    result of a composition of transformations.
    """

    def __init__(self, tensor, vector):
        self.tensor = tensor
        self.vector = vector

    def asLinearTransformation(self):
        return self

    def __mul__(self, other):
        other = other.asLinearTransformation()
        return LinearTransformation(self.tensor.dot(other.tensor),
                                    self.tensor*other.vector+self.vector)

    def __call__(self, vector):
        return self.tensor*vector + self.vector

    def inverse(self):
        return LinearTransformation(self.tensor.inverse(), -self.vector)


# Utility functions

def angleFromSineAndCosine(y, x):
    return atan2(y, x)

def mod_angle(angle, mod):
    return (angle + mod/2.) % mod - mod/2


# Test code

if __name__ == '__main__':

    t = Translation(Geometry.Vector(1.,-2.,0))
    r = Rotation(Geometry.Vector(0.1, -2., 0.5), 1.e-10)
    q = r.asQuaternion()
    angles = r.threeAngles(Geometry.Vector(1., 0., 0.),
                           Geometry.Vector(0., 1., 0.),
                           Geometry.Vector(0., 0., 1.))
    c = t*r
    print c.screwMotion()
    s = Scaling(2.)
    all = s*t*r
    print all(Geometry.ex)
