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
import Derivative

bad = 1e36
class TestDerivative(unittest.TestCase):
    ""
    
    def setUp(self):
        ""
        self.A0 = np.ones((3,3), dtype=np.float32)
        self.A1 = np.array( range(9), dtype=np.float32).reshape((3,3))
        self.A1 *= self.A1
        self.A1 += self.A0
        self.A0[0,1] = 1e37
        self.A1[1,0] = 1e37
        self.B0 = np.ones((3,3), dtype=np.float32) * 0.5
        self.B1 = np.array( range(9), dtype=np.float32).reshape((3,3))
        self.B1 += self.B0
        self.B1[1,-1] = self.B0[1,-1]
        self.B1[-1, 1] = 1e37
         
    
    def testDerivative(self):
        "Test the derivative function."
        result = Derivative.execute(self.A0, self.A1, self.B0, self.B1)
        correct = np.array([[1e37, 1e37,    2],
                            [1e37,    4, 1e37],
                            [   6, 1e37,    8]], dtype=np.float32)
        if not np.allclose(result, correct):
            self.fail("Incorrect answer:" + repr(result))
            