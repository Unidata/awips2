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
import PotVortK

class TestPotVortK(unittest.TestCase):
    "Unit test class for PotVortK function."
    def setUp(self):
        self.p_lo = np.ones((5,5), dtype=np.float32) * 900.0
        self.p_up = self.p_lo + 100.0
        self.o_up = 285.0
        self.o_lo = 283.0
        self.u_lo = np.ones_like(self.p_lo) * 4.0
        self.v_lo = np.ones_like(self.p_lo) * 3.0
        self.Wind_lo = (self.u_lo, self.v_lo)
        self.u_up = self.u_lo + 1.0
        self.v_up = self.v_lo + 1.0
        self.Wind_up = (self.u_up, self.v_up)
        self.dx = np.ones_like(self.p_lo)
        self.dy = np.ones_like(self.p_lo)
        self.coriolis = np.ones_like(self.p_lo) * np.sqrt(2)
    
    def testMath(self):
        "Test basic operation."
        result = PotVortK.execute(self.p_up, self.p_lo, self.o_up, self.o_lo,
                                  self.Wind_up, self.Wind_lo, 
                                  self.dx, self.dy, self.coriolis)
        ansr = 2.82842696e-01 # obtained from early test run
        correct = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        if not np.allclose(correct, result):
            self.fail("Incorrect value:" + repr(result))
            
    def testMasked(self):
        "Send invalid input and make sure output reflects it."
        self.p_up[1,1] = 1e37
        result = PotVortK.execute(self.p_up, self.p_lo, self.o_up, self.o_lo,
                                  self.Wind_up, self.Wind_lo, 
                                  self.dx, self.dy, self.coriolis)
        ansr = 2.82842696e-01 # obtained from early test run
        correct = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                            [1e37, 1e37, ansr, ansr, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        if not np.allclose(correct, result):
            self.fail("Incorrect value:" + repr(result))            