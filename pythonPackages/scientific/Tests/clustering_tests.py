#
# Tests for Scientific.Clustering.AffinityPropagation
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2007-3-13
#

import unittest
from Scientific.Clustering.AffinityPropagation import DataSet
from Scientific import N


class APTest(unittest.TestCase):

    """
    Test AffinityPropagation
    """

    def setUp(self):
        self.points = N.array([[-2.341500, 3.696800],
                               [-1.109200, 3.111700],
                               [-1.566900, 1.835100],
                               [-2.658500, 0.664900],
                               [-4.031700, 2.845700],
                               [-3.081000, 2.101100],
                               [2.588000, 1.781900],
                               [3.292300, 3.058500],
                               [4.031700, 1.622300],
                               [3.081000, -0.611700],
                               [0.264100, 0.398900],
                               [1.320400, 2.207400],
                               [0.193700, 3.643600],
                               [1.954200, -0.505300],
                               [1.637300, 1.409600],
                               [-0.123200, -1.516000],
                               [-1.355600, -3.058500],
                               [0.017600, -4.016000],
                               [1.003500, -3.590400],
                               [0.017600, -2.420200],
                               [-1.531700, -0.930900],
                               [-1.144400, 0.505300],
                               [0.616200, -1.516000],
                               [1.707700, -2.207400],
                               [2.095100, 3.430900]])
        self.results = [(-50., N.array([ 2,  2,  2,  2,  2,  2,  6,  6,  6,  6,
                                         2,  6,  2,  6,  6, 19, 19, 19, 19, 19,
                                         19,  2, 19, 19,  6])),
                        (-10., N.array([ 1,  1,  1,  5,  5,  5,  6,  6,  6, 13,
                                         21,  6,  1, 13,  6, 19, 19, 19, 19, 19,
                                         21, 21, 19, 19,  6])),
                        ( -5., N.array([ 1,  1,  1,  5,  5,  5,  6,  6,  6, 13,
                                         21,  6,  1, 13,  6, 22, 17, 17, 17,
                                         22, 21, 21, 22, 22,  6]))]

    def simfunc(self, p1, p2):
        return -N.sum((p1-p2)**2)

    def testPoints2D(self):
        data = DataSet(self.points, self.simfunc, symmetric=True)
        self.assertAlmostEqual(data.median_similarity, -16.159450725, 10)
        for threshold, result in self.results:
            data.findClusters(threshold)
            self.assert_(N.logical_and.reduce(data.exemplar == result))

if __name__ == '__main__':
    unittest.main()
