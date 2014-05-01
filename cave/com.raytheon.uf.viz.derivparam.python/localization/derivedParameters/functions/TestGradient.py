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
import Gradient

class TestGradient(unittest.TestCase):
    def setUp(self):
        self.data = np.ones((5,5), dtype=np.float32) * 2.0
        np.cumsum(self.data, axis=0, out=self.data)
        np.cumsum(self.data, axis=1, out=self.data)
        
        self.dx = np.ones((5,5), dtype=np.float32) * 0.5
        self.dy = np.ones((5,5), dtype=np.float32) * 0.5
    
    def testMath(self):
        "Check that simple gradients match expectations."
        grd_u, grd_v = Gradient.execute(self.data, self.dx, self.dy)
        correct_u = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                              [ 4.0,  8.0, 12.0, 16.0, 20.0],
                              [ 4.0,  8.0, 12.0, 16.0, 20.0],
                              [ 4.0,  8.0, 12.0, 16.0, 20.0],
                              [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        correct_v = correct_u.transpose()
        if not np.allclose(grd_u, correct_u):
            self.fail("" + repr(grd_u));

        if not np.allclose(grd_v, correct_v):
            self.fail("" + repr(grd_v))
            
    def testClean(self):
        "Check that input arrays are not modified."
        self.data[1,1] = 1e37
        self.dx[1,1] = 1e37
        self.dy[1,1] = 1e37
        dataCopy = np.copy(self.data)
        dxCopy = np.copy(self.dx)
        dyCopy = np.copy(self.dy)
        dontcare_u, dontcare_v = Gradient.execute(self.data, self.dx, self.dy)
        if not np.allclose(self.data, dataCopy):
            fail("Data was changed.")
        if not np.allclose(self.dx, dxCopy):
            fail("dx was changed.")
        if not np.allclose(self.dy, dyCopy):
            fail("dy was changed.")