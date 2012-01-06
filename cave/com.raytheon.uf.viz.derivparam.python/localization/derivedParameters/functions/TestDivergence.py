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
import Divergence

## Unit test for Divergence function.
class TestDivergence(unittest.TestCase):
    """Unit tests for the divergence function."""
    
    def testOuterEdge(self):        
        """Make sure Divergence.execute() fills the outer edge with
the "invalid" placeholder value (1e37)."""
         
        U = np.ones((4,4), dtype=np.float32)
        V = np.ones_like(U);
        Wind = (U,V)
        dx = np.ones_like(U)
        dy = np.ones_like(U)
        dvgnc = Divergence.execute(Wind,dx,dy)
        self.assertEquals((4,4), dvgnc.shape, "dvgnc.shape")
        self.assertEquals(np.float32, dvgnc.dtype, "dvgnc.dtype")
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37],
                                  [1e37, 0, 0, 1e37],
                                  [1e37, 0, 0, 1e37],
                                  [1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.all(correctAnswer==dvgnc), repr(dvgnc))

    def testMiddleMath(self):
        """Confirm that the inner cells are calculated correctly."""        
        U = np.array([[1,1,1,1,1],
                      [1,1,5,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1]], dtype=np.float32)
        V = np.array([[1,1,1,1,1],
                      [1,1,5,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1]], dtype=np.float32)
        Wind = (U,V)
        dx = np.ones_like(U);
        dy = np.ones_like(U);
        dvgnc = Divergence.execute(Wind,dx,dy)
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37, 2, 0, -2, 1e37],
                                  [1e37, 0, -2, 0, 1e37],
                                  [1e37, 0, 0, 0, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.all(correctAnswer==dvgnc), "Divergence is incorrect.\n" + repr(dvgnc))
        
    def testScalarDxDy(self):
        """Confirm that the inner cells are calculated correctly
when dx and dy are scalars instead of arrays."""        
        U = np.array([[1,1,1,1,1],
                      [1,1,5,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1]], dtype=np.float32)
        V = np.array([[1,1,1,1,1],
                      [1,1,5,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1]], dtype=np.float32)
        Wind = (U,V)
        dx = 1
        dy = 1
        dvgnc = Divergence.execute(Wind,dx,dy)
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37, 2, 0, -2, 1e37],
                                  [1e37, 0, -2, 0, 1e37],
                                  [1e37, 0, 0, 0, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.all(correctAnswer==dvgnc), "Divergence is incorrect.\n" + repr(dvgnc))

    def testWithQ(self):
        """Confirm that the inner cells are calculated correctly when quan is provided."""        
        U = np.array([[1,1,1,1,1],
                      [1,1,5,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1]], dtype=np.float32)
        V = np.array([[1,1,1,1,1],
                      [1,1,5,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1]], dtype=np.float32)
        Wind = (U,V)
        dx = np.ones_like(U);
        dy = np.ones_like(U);
        Q = np.array([[1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,1,1,1],
                      [1,1,4,1,1],
                      [1,1,1,1,1]])
        dvgnc = Divergence.execute(Wind,dx,dy,Q)
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37, 2, 0, -2, 1e37],
                                  [1e37, 0, -0.5, 0, 1e37],
                                  [1e37, 1.5, 0, -1.5, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.all(correctAnswer==dvgnc), "Divergence is incorrect.\n" + repr(dvgnc))
        
        
if "__main__" == __name__:
    unittest.main()