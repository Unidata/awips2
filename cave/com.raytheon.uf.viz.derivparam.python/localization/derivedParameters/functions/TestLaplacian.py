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
import Laplacian

class TestLaplacian(unittest.TestCase):
    def setUp(self):
        self.Q = np.ones((5,5), dtype=np.float32) * 1.5
        self.Q += np.eye(5, 5, 0, dtype=self.Q.dtype) * 0.25
        self.dx = np.ones(np.shape(self.Q), dtype=self.Q.dtype)
        self.dy = np.ones(np.shape(self.Q), dtype=self.Q.dtype)
    
    def testMath(self):
        "Test basic Laplacian math function."
        answer = Laplacian.execute(self.Q, self.dx, self.dy)
        correctAnswer = np.array([[1e37,1e37,1e37,1e37,1e37],
                                  [1e37,-1,0.5,0,1e37],
                                  [1e37,0.5,-1,0.5,1e37],
                                  [1e37,0, 0.5, -1,1e37],
                                  [1e37,1e37,1e37,1e37,1e37]], dtype=self.Q.dtype)
        self.failUnless(np.all(correctAnswer==answer), "Laplacian was incorrect. " + repr(answer))
    
    def testInvalidCell(self):
        "Test whether invalid cells are handled properly."
        self.Q[1,1] = 1e37
        answer = Laplacian.execute(self.Q, self.dx, self.dy)
        correctAnswer = np.array([[1e37,1e37,1e37,1e37,1e37],
                                  [1e37,1e37,1e37,0,1e37],
                                  [1e37,1e37,-1,0.5,1e37],
                                  [1e37,0, 0.5, -1,1e37],
                                  [1e37,1e37,1e37,1e37,1e37]], dtype=self.Q.dtype)
        self.failUnless(np.all(correctAnswer==answer), "Laplacian was incorrect. " + repr(answer))
        
