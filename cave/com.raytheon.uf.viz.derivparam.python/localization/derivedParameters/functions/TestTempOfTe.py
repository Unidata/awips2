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
import TempOfTe

class TestTempOfTe(unittest.TestCase):
    def setUp(self):
        pass
    
    def testExecute(self):
        'Test operation for 2x1 and 1x2 inputs.'
        # This test's arrays are small enough to follow easily in the debugger.
        temp = np.array( range(260,315,50), dtype=np.float32)
        temp.resize((len(temp),1))
        pressure = np.array( range(700,800, 50), dtype=np.float32)
        result = TempOfTe.execute(temp, pressure)

        correct = np.array( [[256.05, 256.26],[281.49, 282.18]], dtype=np.float32)
        if not np.allclose(result, correct, 0.01, 0.01):
            self.fail("Wrong answer:" + repr(result))
            
    def testOuterLimits(self):
        ""
        temp = np.array([100.0, 192.0, 193.0, 194.0, 332.0, 333.0, 334.0], dtype=np.float32)
        pressure = np.array([730, 730, 730, 730, 730, 730, 730], dtype=np.float32)
        result = TempOfTe.execute(temp, pressure)
        correct = np.array([100.0, 192.0, 193.0, 194.0, 288.0, 288.0, 1e37], dtype=np.float32)
        if not np.allclose(result, correct, 0.01, 0.01):
            self.fail("Wrong answer:" + repr(result))
        