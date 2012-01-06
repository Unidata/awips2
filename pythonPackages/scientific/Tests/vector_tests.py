#
# Tests for Scientific.Geometry.Vector
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-11-23
#

import unittest
import copy
from Scientific import N
from Scientific.Geometry import Tensor

class VectorTest(unittest.TestCase):

    """
    Test Vector
    """

    def testVectorPython(self):
        from Scientific.Geometry.VectorModule import Vector, isVector
        self.shouldPass(Vector, isVector)
        self.shouldFail(Vector, isVector)

    def testVectorPyrex(self):
        from Scientific_vector import Vector, isVector
        self.shouldPass(Vector, isVector)
        self.shouldFail(Vector, isVector)

    def shouldPass(self, Vector, isVector):
        # Create vector objects
        v1 = Vector(1., -2., 3.)
        v2 = Vector([-2., 1., 0.])
        # Check that vectors are not copied
        v1_copy = copy.copy(v1)
        self.assertTrue(v1 is v1_copy)
        v1_copy = copy.deepcopy(v1)
        self.assertTrue(v1 is v1_copy)
        # check len and element access
        self.assertEqual(len(v1), 3)
        self.assertEqual(v1[0], 1.)
        self.assertEqual(v1[1], -2.)
        self.assertEqual(v1[2], 3.)
        self.assertEqual(v1[-3], 1.)
        self.assertEqual(v1[-2], -2.)
        self.assertEqual(v1[-1], 3.)
        self.assertEqual(v1.x(), 1.)
        self.assertEqual(v1.y(), -2.)
        self.assertEqual(v1.z(), 3.)
        # Check arithmetic
        self.assertEqual(v1+v2, Vector(-1., -1., 3.))
        self.assertEqual(v1-v2, Vector(3., -3., 3.))
        self.assertEqual(-v1, Vector(-1., 2., -3.))
        self.assertEqual(v1*v2, -4.)
        self.assertEqual(2.*v1, Vector(2., -4., 6.))
        self.assertEqual(v1/0.5, Vector(2., -4., 6.))
        # Check comparisons
        self.assertTrue(v1 == v1)
        self.assertFalse(v1 == v2)
        self.assertFalse(v1 == None)
        # Check methods
        self.assertAlmostEqual(v1.length(), N.sqrt(14.), 12)
        self.assertAlmostEqual(v1.normal()[0], v1[0]/N.sqrt(14.), 12)
        self.assertAlmostEqual(v1.normal()[1], v1[1]/N.sqrt(14.), 12)
        self.assertAlmostEqual(v1.normal()[2], v1[2]/N.sqrt(14.), 12)
        self.assertAlmostEqual(v1.cross(v1).length(), 0., 12)
        self.assertEqual(v1.cross(v2), Vector(-3., -6., -3.))
        self.assertAlmostEqual(v1.angle(v1), 0., 12)
        self.assertAlmostEqual(v1.angle(v2),
                               N.arccos(v1.normal()*v2.normal()), 12)
        dp = v1.dyadicProduct(v2)
        for i in range(3):
            for j in range(3):
                self.assertEqual(dp[i, j], v1[i]*v2[j])
        self.assertTrue(N.logical_and.reduce(v1.asTensor().array == v1.array))
        # Check isVector
        self.assertTrue(isVector(v1))
        self.assertTrue(isVector(v2))
        self.assertFalse(isVector(0.))
        self.assertFalse(isVector("string"))
                               
    def shouldFail(self, Vector, isVector):
        # Create vector objects
        v1 = Vector(1., -2., 3.)
        v2 = Vector([-2., 1., 0.])
        # Check indices out of range
        for index in [3, 4, 100, -4, -5, -10]:
            self.assertRaises(IndexError, eval, "v1[index]", {}, locals())
        # Check arithmetic
        self.assertRaises(TypeError, eval, "v1/v2", {}, locals())
        self.assertRaises(TypeError, eval, "1./v2", {}, locals())
        # Check methods
        self.assertRaises(ZeroDivisionError, Vector(0., 0., 0.).normal)
        self.assertRaises(TypeError, v1.angle, 0.)
        self.assertRaises(TypeError, v1.cross, 0.)
        self.assertRaises(TypeError, v1.dyadicProduct, 0.)

if __name__ == '__main__':
    unittest.main()
