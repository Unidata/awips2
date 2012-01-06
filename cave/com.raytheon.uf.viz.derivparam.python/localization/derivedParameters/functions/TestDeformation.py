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
###


import unittest
import numpy as np
import Deformation

##
# Unit test class for Deformation
class TestDeformation(unittest.TestCase):
    def setUp(self):
        self.U = np.ones((5,5), dtype=np.float32)
        self.U += np.eye(5,5,0)
        self.V = np.ones((5,5), dtype=np.float32)
        
        self.V += np.eye(5,5,0).transpose()
        self.Vector = (self.U, self.V)
        self.dx = np.ones((5,5), dtype=np.float32)
        self.dy = np.ones((5,5), dtype=np.float32)
    
    def testExecute(self):
        "Test basic results of execute method."
        result = Deformation.execute(self.Vector, self.dx, self.dy)
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37,    0,    1,    0, 1e37],
                                  [1e37,    1,    0,    1, 1e37],
                                  [1e37,    0,    1,    0, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.allclose(result, correctAnswer), "Wrong answers: " + repr(result))
        
    def testMasked(self):
        "Check that invalid data results in cells with ourNaN (1e37)"
        self.Vector[1][1,1] = 1e37
        result = Deformation.execute(self.Vector, self.dx, self.dy)
        correctAnswer = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                                  [1e37,    0, 1e37,    0, 1e37],
                                  [1e37, 1e37,    0,    1, 1e37],
                                  [1e37,    0,    1,    0, 1e37],
                                  [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        self.failUnless(np.allclose(result, correctAnswer), "Wrong answers: " + repr(result))
