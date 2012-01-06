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
import IsenStability

class TestIsenStability(unittest.TestCase):
    "Unit test for IsenStability function."
    def setUp(self):
        self.p_up = np.array(range(9), dtype=np.float32).reshape((3,3)) + 900
        self.p_lo = self.p_up + np.array(range(9), dtype=np.float32).reshape((3,3)) * 10
        self.o_up = 2.1
        self.o_lo = 0.1
    
    def testMath(self):
        "Test that correct answer is obtained."
        result = IsenStability.execute(self.p_up, self.p_lo, self.o_up, self.o_lo)
        correct = np.array(range(9), dtype=np.float32).reshape((3,3)) * 5
        correct[0,0] = 5.0 
        if not np.allclose(result, correct):
            self.fail("Incorrect result:" + repr(result))
            
    def testInvalid(self):
        "Test that invalid inputs result in invalid outputs."
        self.p_up[2,2] = 1e37
        result = IsenStability.execute(self.p_up, self.p_lo, self.o_up, self.o_lo)
        correct = np.array([[5, 5, 10],
                            [15, 20, 25],
                            [30, 35, 1e37]], dtype=np.float32)
        correct[0,0] = 5.0 
        if not np.allclose(result, correct):
            self.fail("Incorrect result:" + repr(result))            