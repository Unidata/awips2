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
import GeoWind
import time

class testGeoWind(unittest.TestCase):
    def setUp(self):
        self.Height = np.array( range(64), dtype=np.float32).reshape((8,8))
        self.dx = np.ones((8,8), dtype=np.float32) * 0.5
        self.dy = np.ones((8,8), dtype=np.float32) * 0.5
        self.coriolis = np.cos( np.arange(0, 3.14159, 3.14159/8, dtype=np.float32))
        self.coriolis = np.ones((8,1)) * self.coriolis
        
    def testExecute(self):
        result = GeoWind.execute(self.Height, self.dx, self.dy, self.coriolis)
        correct_u = np.array([[  1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37],
                              [  1e37, -21.23, -27.74, -51.25,-1.54e7,  51.25,  27.74,   1e37],
                              [  1e37, -21.23, -27.74, -51.25,-1.54e7,  51.25,  27.74,   1e37],
                              [  1e37, -21.23, -27.74, -51.25,-1.54e7,  51.25,  27.74,   1e37],
                              [  1e37, -21.23, -27.74, -51.25,-1.54e7,  51.25,  27.74,   1e37],
                              [  1e37, -21.23, -27.74, -51.25,-1.54e7,  51.25,  27.74,   1e37],
                              [  1e37, -21.23, -27.74, -51.25,-1.54e7,  51.25,  27.74,   1e37],
                              [  1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37]],
                              dtype=np.float32)
        correct_v = np.array([[  1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37],
                              [  1e37, 169.82, 221.88, 409.98, 1.24e8,-409.99,-221.88,   1e37],
                              [  1e37, 169.82, 221.88, 409.98, 1.24e8,-409.99,-221.88,   1e37],
                              [  1e37, 169.82, 221.88, 409.98, 1.24e8,-409.99,-221.88,   1e37],
                              [  1e37, 169.82, 221.88, 409.98, 1.24e8,-409.99,-221.88,   1e37],
                              [  1e37, 169.82, 221.88, 409.98, 1.24e8,-409.99,-221.88,   1e37],
                              [  1e37, 169.82, 221.88, 409.98, 1.24e8,-409.99,-221.88,   1e37],
                              [  1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37,   1e37]],
                              dtype=np.float32)
        if not np.allclose(result[0], correct_u, 0.01, 0.01):
            self.fail("Wrong answer for u:" + repr(result[0]))

        if not np.allclose(result[1], correct_v, 0.01, 0.01):
            self.fail("Wrong answer for v:" + repr(result[1]))
