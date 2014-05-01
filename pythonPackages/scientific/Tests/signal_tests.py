#
# Tests for Scientific.Signals.Models
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-8-25
#

import unittest
from Scientific.Signals.Models import AutoRegressiveModel, \
                                      AveragedAutoRegressiveModel
from Scientific import N


class ARModelTest(unittest.TestCase):

    """
    Test AutoRegressiveModel
    """

    def setUp(self):
        self.constant = N.zeros((5,), N.Float) + 1.

    def testExponential(self):
        signal = N.exp(-N.arange(10))
        model = AutoRegressiveModel(5, signal)
        self.assertAlmostEqual(model.sigma, 0.248259203831, 10)
        self.assertAlmostEqual(model.coeff[0], 0.0134752822213, 10)
        self.assertAlmostEqual(N.sum(model.coeff), 0.589668004984, 10)
        spectrum =  model.spectrum(N.array([0., 1., 10.])).values
        self.assertAlmostEqual(spectrum[0], 0.183024806929, 10)
        self.assertAlmostEqual(spectrum[1], 0.0728546620522, 10)
        self.assertAlmostEqual(spectrum[2], 0.00741215804406, 10)
        poles = model.poles()
        self.assertAlmostEqual(N.sum(poles).real, 0.8508408685, 10)
        self.assertEqual(N.sum(poles).imag, 0.)
        correlation = model.correlation(3).values
        self.assertAlmostEqual(correlation[0], 0.115651764037, 10)
        self.assertAlmostEqual(correlation[2], 0.0307404966495, 10)
        self.assertAlmostEqual(model.frictionConstant(), 0.48018033306, 10)
        memory = model.memoryFunction(4).values
        self.assertAlmostEqual(memory[0], 0.703891452672, 10)
        self.assertAlmostEqual(memory[1], 0.15417211278, 10)
        self.assertAlmostEqual(memory[2], -0.026985553641, 10)
        self.assertAlmostEqual(memory[3], 0.00425970654789, 10)
        self.assertAlmostEqual(model.predictStep(), 7.85423644046e-05, 10)


if __name__ == '__main__':
    unittest.main()
