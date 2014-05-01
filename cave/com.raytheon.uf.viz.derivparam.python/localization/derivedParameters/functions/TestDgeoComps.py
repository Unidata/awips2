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
import DgeoComps
from constants import bad
import nan

class TestDgeoComps(unittest.TestCase):
    ""
    
    def failIfArraysDifferent(self, arr, arr_correct, arr_name):
        if not np.allclose(arr, arr_correct, 0.01, 0.01):
            failMsg = ""
            if not arr.shape == arr_correct.shape:
                failMsg +=  "%s.shape = " % (arr_name)
                failMsg += arr.shape + "\n"
            if not arr.dtype == arr_correct.dtype:
                failMsg += "%s.dtype = " % (arr_name)
                failMsg += arr.dtype + "\n"
            if arr.shape==arr_correct.shape and \
               arr.dtype==arr_correct.dtype:
                for xidx in range(5):
                    for yidx in range(5):
                        if np.abs(arr[xidx,yidx]-arr_correct[xidx,yidx])>0.01:
                            failMsg += "%s[%d,%d] = %g" % (arr_name,xidx,yidx, arr[xidx,yidx])
                            failMsg += ", expected %g\n" % arr_correct[xidx,yidx] 
            self.fail(failMsg)
    
    def setUp(self):
        ""
        self.height = np.array(range(25), dtype=np.float32).reshape((5,5))
        self.height[1,1] = 1e37
        self.height = nan.nan_greater(self.height, bad, copy=False)
        self.dx = np.ones((5,1)) * np.array([1,2,1,1,1], dtype=np.float32)
        self.dy = np.ones((5,1)) * np.array([1,2,1,1,1], dtype=np.float32)
        self.dy = self.dx.transpose()
        pi = 3.1415927
        self.coriolis = np.array(range(5), dtype=np.float32).reshape((5,1)) + 40
        self.coriolis *= pi/180
        self.coriolis = np.sin(self.coriolis) * 3.0 * np.ones((5,))
    
    def testDgeoComps(self):
        dugdx, dugdy, dvgdx, dvgdy = DgeoComps.execute(self.height, self.dx, self.dy, self.coriolis)
        dugdx = nan.nan_filled(dugdx, 1e37)
        dugdy = nan.nan_filled(dugdy, 1e37)
        dvgdx = nan.nan_filled(dvgdx, 1e37)
        dvgdy = nan.nan_filled(dvgdy, 1e37)
        dugdx_correct = np.array([[   1e37,   1e37,   1e37,    1e37,    1e37],
                                  [   1e37,  -0.00,  -0.00,   -0.00,    1e37],
                                  [   1e37,  -0.00,   1e37,   -0.00,    1e37],
                                  [   1e37,  -0.00,  -0.00,   -0.00,    1e37],
                                  [   1e37,   1e37,   1e37,    1e37,    1e37]], dtype=np.float32)
        
        dugdy_correct = np.array([[   1e37,   1e37,   1e37,    1e37,    1e37],
                                  [   1e37,   1e37,   1e37,   -0.00,    1e37],
                                  [   1e37,   0.00,   0.00,   -0.00,    1e37],
                                  [   1e37,  -0.00,  -0.00,   -0.00,    1e37],
                                  [   1e37,   1e37,   1e37,    1e37,    1e37]], dtype=np.float32)
        
        dvgdx_correct = np.array([[   1e37,   1e37,   1e37,    1e37,    1e37],
                                  [   1e37,   1e37,      0,       0,    1e37],
                                  [   1e37,   1e37,      0,       0,    1e37],
                                  [   1e37,      0,      0,       0,    1e37],
                                  [   1e37,   1e37,   1e37,    1e37,    1e37]], dtype=np.float32)
        
        dvgdy_correct = np.array([[   1e37,   1e37,   1e37,    1e37,    1e37],
                                  [   1e37,   0.00,   0.00,    0.00,    1e37],
                                  [   1e37,   0.00,   1e37,    0.00,    1e37],
                                  [   1e37,   0.00,   0.00,    0.00,    1e37],
                                  [   1e37,   1e37,   1e37,    1e37,    1e37]], dtype=np.float32)
        self.failIfArraysDifferent(dugdx, dugdx_correct, "dugdx")
        self.failIfArraysDifferent(dugdy, dugdy_correct, "dugdy")
        self.failIfArraysDifferent(dvgdx, dvgdx_correct, "dvgdx")
        self.failIfArraysDifferent(dvgdy, dvgdy_correct, "dvgdy")

if '__main__' == __name__:
    unittest.main()

