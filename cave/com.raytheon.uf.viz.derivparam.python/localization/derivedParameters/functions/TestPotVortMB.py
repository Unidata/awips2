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
import PotVortMB

class TestPotVortMB(unittest.TestCase):
    def setUp(self):
        self.t_lo = np.ones((5,5), dtype=np.float32) * 273.15
        self.t_up = self.t_lo + 12.5
        self.p_lo = np.ones_like(self.t_lo) * 900.0
        self.p_up = self.p_lo + 100.0
        self.u_lo = np.ones_like(self.t_lo) * 4.0
        self.v_lo = np.ones_like(self.t_lo) * 3.0
        self.Wind_lo = (self.u_lo, self.v_lo)
        self.u_up = self.u_lo + 1.0
        self.v_up = self.v_lo + 1.0
        self.Wind_up = (self.u_up, self.v_up)
        self.dx = np.ones_like(self.t_lo)
        self.dy = np.ones_like(self.t_lo)
        self.coriolis = np.ones_like(self.t_lo) * np.sqrt(2)
    
    def testMath(self):
        "Test simplest operation."
        result = PotVortMB.execute(self.t_up, self.t_lo, self.p_up, self.p_lo,
                                   self.Wind_up, self.Wind_lo, 
                                   self.dx, self.dy, self.coriolis)
        av = 2.82842707634
        ansr = -0.5 * (av*12.5 + 1.0*0.0 - 1.0*0.0) / 100
        
        correct = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        if not np.allclose(correct, result):
            self.fail("Incorrect result:" + repr(result))
            
    def testMasked(self):
        "Test that function doesn't crash when an input cell is ourNaN."
        self.t_up[1,1] = 1e37
        result = PotVortMB.execute(self.t_up, self.t_lo, self.p_up, self.p_lo,
                                   self.Wind_up, self.Wind_lo, 
                                   self.dx, self.dy, self.coriolis)
        av = 2.82842707634
        ansr = -0.5 * (av*12.5 + 1.0*0.0 - 1.0*0.0) / 100
        
        correct = np.array([[1e37, 1e37, 1e37, 1e37, 1e37],
                            [1e37, 1e37, 1e37, ansr, 1e37],
                            [1e37, 1e37, ansr, ansr, 1e37],
                            [1e37, ansr, ansr, ansr, 1e37],
                            [1e37, 1e37, 1e37, 1e37, 1e37]], dtype=np.float32)
        if not np.allclose(correct, result):
            self.fail("Incorrect result:" + repr(result))
            
        
