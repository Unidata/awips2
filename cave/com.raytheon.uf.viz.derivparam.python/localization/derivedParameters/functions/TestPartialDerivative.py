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
import PartialDerivative as Partial

class TestPartialDerivative(unittest.TestCase):
    ""
    
    def failIfArraysDifferent(self, arr, arr_correct, arr_name):
        failMsg = ""
        if not arr.shape == arr_correct.shape:
            failMsg +=  "%s.shape = " % (arr_name)
            failMsg += arr.shape + "\n"
        if not arr.dtype == arr_correct.dtype:
            failMsg += "%s.dtype = " % (arr_name)
            failMsg += arr.dtype + "\n"
        if arr.shape==arr_correct.shape and \
           arr.dtype==arr_correct.dtype:
            for xidx in range(arr.shape[0]):
                for yidx in range(arr.shape[1]):
                    if np.abs(arr[xidx,yidx]-arr_correct[xidx,yidx])<=0.01:
                        pass
                    elif np.isnan(arr[xidx,yidx]) and np.isnan(arr_correct[xidx,yidx]):
                        pass
                    else:
                        failMsg += "%s[%d,%d] = %g" % (arr_name,xidx,yidx, np.ma.filled(arr[xidx,yidx], np.NaN ) )
                        failMsg += ", expected %g\n" % np.ma.filled(arr_correct[xidx,yidx], np.NaN)
        if "" != failMsg: 
            self.fail(failMsg)

    
    def setUp(self):
        self.Qarr = np.array( range(25), dtype=np.float32).reshape((5,5))
        self.dx = np.ones((5,5), dtype=np.float32)
        self.dx[2,:] = 2
        self.dy = np.ones((5,5), dtype=np.float32)
        self.dy[:,3] = 2
    def testExecute(self):
        ""
        dQdx, dQdy = Partial.calculate(self.Qarr, self.dx, self.dy)
        dQdx_correct = np.array([[np.NaN, np.NaN, np.NaN, np.NaN, np.NaN],
                                    [np.NaN,      5,      5,      5, np.NaN],
                                    [np.NaN,   2.50,   2.50,   2.50, np.NaN],
                                    [np.NaN,      5,      5,      5, np.NaN],
                                    [np.NaN, np.NaN, np.NaN, np.NaN, np.NaN]], 
                                    dtype=np.float32)
        self.failIfArraysDifferent(dQdx, dQdx_correct, "dQdx")
        dQdy_correct = np.ma.array([[np.NaN, np.NaN, np.NaN, np.NaN, np.NaN],
                                    [np.NaN,      1,      1,    0.5, np.NaN],
                                    [np.NaN,      1,      1,    0.5, np.NaN],
                                    [np.NaN,      1,      1,    0.5, np.NaN],
                                    [np.NaN, np.NaN, np.NaN, np.NaN, np.NaN]], 
                                    dtype=np.float32)
        self.failIfArraysDifferent(dQdy, dQdy_correct, "dQdy")
