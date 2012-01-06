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
import CompBy

class TestCompBy(unittest.TestCase):
    """Unit tests for CompBy function."""
    
    def setUp(self):
        """Create arrays to use in calls to CompBy.execute()."""
        self.U = np.ones((3,3), dtype=np.float32) * np.sqrt(2)
        self.U[1,0] = np.sqrt(3)
        self.V = np.ones((3,3), dtype=np.float32) * np.sqrt(2)
        self.V[0,2] = -self.V[0,2]
        self.Vector = (self.U, self.V)
        
        self.ThU = np.ones((3,3), dtype=np.float32) * 0.1
        self.ThU = np.cumsum(self.ThU, axis=0)
        self.ThV = np.zeros((3,3), dtype=np.float32)
        self.ThGrd = (self.ThU, self.ThV)
    
    def testAngleZero(self):
        """Test with no rotation."""
        comp = CompBy.execute(self.Vector, self.ThGrd, 0.0)
        correctAnswer_U = np.ones((3,3), dtype=np.float32) * np.sqrt(2)
        correctAnswer_U[1,0] = 1.73205078
        correctAnswer_V = np.array([[0,0,0], [0,0,0], [0,0,0]], dtype=np.float32)
        # check for correct answers.
        if not np.all(correctAnswer_U==comp[0]):
            self.fail(repr(comp))
        if not np.all(correctAnswer_V==comp[1]):
            self.fail(repr(comp))
    
    def testAngle45(self):
        """Test with 45 degree rotation."""
        comp = CompBy.execute(self.Vector, self.ThGrd, 45.0) 
        correctAnswer_U = np.zeros((3,3), dtype=np.float32)
        correctAnswer_U[0,2] = np.sqrt(2)
        correctAnswer_U[1,0] = 0.15891865
        correctAnswer_V = np.zeros((3,3), dtype=np.float32)
        correctAnswer_V[0,2] = -np.sqrt(2)
        correctAnswer_V[1,0] = -0.15891865
        # check for correct answers.
        if not np.all(correctAnswer_U==comp[0]):
            self.fail(repr(comp))
        if not np.all(correctAnswer_V==comp[1]):
            self.fail(repr(comp))
    
    def testAngle90(self):
        """Test with rotation of 90 degrees."""
        comp = CompBy.execute(self.Vector, self.ThGrd, 90.0) 
        correctAnswer_U = np.zeros((3,3), dtype=np.float32)
        correctAnswer_V = np.ones((3,3), dtype=np.float32) * np.sqrt(2)
        correctAnswer_V[0,2] = -np.sqrt(2)
        # check for correct answers.
        # note: We have to use allclose(a,b) instead of all(a==b) because of 
        # rounding errors in the degrees-to-radians conversion constant.
        # Since this is the same constant used in the original Fortran code,
        # the error should be identical. 
        if not np.allclose(correctAnswer_U,comp[0]):
            self.fail(repr(comp))
        if not np.all(correctAnswer_V==comp[1]):
            self.fail(repr(comp))
    
    def testAngle120(self):
        """Test angles above 90 but below 180."""
        comp = CompBy.execute(self.Vector, self.ThGrd, 120.0) 
        correctAnswer_U = np.ones((3,3), dtype=np.float32) * 0.96592587
        correctAnswer_U[0,2] = -0.25881901
        correctAnswer_U[1,0] = 1.04538524
        correctAnswer_U[2,:] = np.ones((1,3)) * 0.96592593
        correctAnswer_V = np.ones((3,3), dtype=np.float32) * 1.67303264
        correctAnswer_V[0,2] = -0.44828767
        correctAnswer_V[1,0] = 1.81066024
        correctAnswer_V[2,:] = np.ones((1,3)) * 1.67303276
        # check for correct answers.
        if not np.all(correctAnswer_U==comp[0]):
            self.fail(repr(comp))
        if not np.all(correctAnswer_V==comp[1]):
            self.fail(repr(comp))
    
    def testAngle225(self):
        """Test angles above 180 but below 1000."""        
        comp = CompBy.execute(self.Vector, self.ThGrd, 225.0) 
        correctAnswer_U = np.ones((3,3), dtype=np.float32) * np.sqrt(2)
        correctAnswer_U[1,0] = 1.73205078
        correctAnswer_V = np.array([[0,0,0], [0,0,0], [0,0,0]], dtype=np.float32)
        # check for correct answers.
        if not np.all(correctAnswer_U==comp[0]):
            self.fail(repr(comp))
        if not np.all(correctAnswer_V==comp[1]):
            self.fail(repr(comp))
    
    def testAngle2090(self):
        """Test with rotation of 2090 degrees."""
        # 2000 portion should be stripped off, so values should be the same as testAngle90.
        comp = CompBy.execute(self.Vector, self.ThGrd, 2090.0) 
        correctAnswer_U = np.zeros((3,3), dtype=np.float32)
        correctAnswer_V = np.ones((3,3), dtype=np.float32) * np.sqrt(2)
        correctAnswer_V[0,2] = -np.sqrt(2)
        # check for correct answers.
        # note: We have to use allclose(a,b) instead of all(a==b) because of 
        # rounding errors in the degrees-to-radians conversion constant.
        # Since this is the same constant used in the original Fortran code,
        # the error should be identical. 
        if not np.allclose(correctAnswer_U,comp[0]):
            self.fail(repr(comp))
        if not np.all(correctAnswer_V==comp[1]):
            self.fail(repr(comp))
    
    def testBigV(self):
        """Test with different ThGrd but no rotation."""
        self.V = self.V + 8
        comp = CompBy.execute(self.Vector, self.ThGrd, 0.0) 
        correctAnswer_U = np.ones((3,3), dtype=np.float32) * np.sqrt(2)
        correctAnswer_U[1,0] = 1.73205078
        correctAnswer_V = np.array([[0,0,0], [0,0,0], [0,0,0]], dtype=np.float32)
        # check for correct answers.
        if not np.all(correctAnswer_U==comp[0]):
            self.fail(repr(comp))
        if not np.all(correctAnswer_V==comp[1]):
            self.fail(repr(comp))
