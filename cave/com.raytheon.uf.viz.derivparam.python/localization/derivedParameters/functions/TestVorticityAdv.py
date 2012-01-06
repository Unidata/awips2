##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##


import unittest
import numpy as np
import VorticityAdv

class TestVorticityAdv(unittest.TestCase):
    def setUp(self):
        """Set up arrays for calls to Vorticity.execute()."""
        self.U = np.ones((5,5), dtype=np.float32)
        self.V = np.ones_like(self.U);
        self.Wind = (self.U,self.V)
        self.Q = np.ones_like(self.U) * 0.5
        self.dx = np.ones_like(self.U)
        self.dy = np.ones_like(self.U)
        
    
    def testOuterEdge(self):
        """Confirm that the outer edge is set to the invalid placeholder (1e37). """
        vrtcta = VorticityAdv.execute(self.Wind, self.Q, self.dx, self.dy)
        self.assertEquals((5,5), vrtcta.shape, "vrtcta.shape")
        self.assertEquals(np.float32, vrtcta.dtype, "vrtcta.dtype")
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37, 0, 0, 0, 1e37],
                                  [1e37, 0, 0, 0, 1e37],
                                  [1e37, 0, 0, 0, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.all(correctAnswer==vrtcta))

    def testMiddleMath(self):
        """Test """
        self.U[1,2] = 5
        self.V[1,2] = 5
        self.Q[3,2] = 0.8
        
        vrtcta = VorticityAdv.execute(self.Wind, self.Q, self.dx, self.dy)
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37, 4.0, 0, 4.0, 1e37],
                                  [1e37, 0, -4.15, 0, 1e37],
                                  [1e37, -0.15, 0, 0.15, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.all(correctAnswer==vrtcta), "VorticityAdv is incorrect.\n" + repr(vrtcta))
        
    def testScalarDxDy(self):
        """Test Vorticity calculation when dx and dy are scalars instead of arrays."""
        self.U[1,2] = 5
        self.V[1,2] = 5
        self.Q[3,2] = 0.8
        
        vrtcta = VorticityAdv.execute(self.Wind, self.Q, 0.1, 0.5)
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37, 80, 0, 80, 1e37],
                                  [1e37, 0, -81.5, 0, 1e37],
                                  [1e37, -0.3, 0, 0.3, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.all(correctAnswer==vrtcta), "VorticityAdv is incorrect.\n" + repr(vrtcta))
        
if "__main__" == __name__:
    unittest.main()