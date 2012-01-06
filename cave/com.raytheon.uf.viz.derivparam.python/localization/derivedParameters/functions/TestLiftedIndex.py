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
import LiftedIndex

class TestLiftedIndex(unittest.TestCase):
    def setUp(self):
        self.P = np.array([600, 700, 800, 900], dtype=np.float32)
        self.T = np.array([250, 275, 290, 300], dtype=np.float32)
        self.RH = np.array([30, 40, 50, 60], dtype=np.float32)
        self.P_500MB = 500.0
        self.T_500MB = np.array([290, 280, 270, 260], dtype=np.float32)
    
    def testLiftedIndex(self):
        "Test simple operation on 4 data points with P_500MB a scalar."
        result = LiftedIndex.execute(self.P, self.T, self.RH, self.T_500MB, self.P_500MB)
        correct = np.array([52.701, 27.634, 6.831, -12.093], dtype=np.float32)
        if not np.allclose(result, correct, .001, .001):
            self.fail("Wrong answer:"+ repr(result))
        
    def testMaskedPValue(self):
        self.P[2] = 1e37
        result = LiftedIndex.execute(self.P, self.T, self.RH, self.T_500MB, self.P_500MB)
        correct = np.array([52.701, 27.634, 1e37, -12.093], dtype=np.float32)
        if not np.allclose(result, correct, .001, .001):
            self.fail("Wrong answer:"+ repr(result))

    def testBadTValue(self):
        self.T[2] = 32.0
        result = LiftedIndex.execute(self.P, self.T, self.RH, self.T_500MB, self.P_500MB)
        correct = np.array([52.701, 27.634, 1e37, -12.093], dtype=np.float32)
        if not np.allclose(result, correct, .001, .001):
            self.fail("Wrong answer:"+ repr(result))
            
    def testBlackHoleValue(self):
        self.P = np.array([1008.18,1013.2], dtype=np.float32)
        self.T = np.array([302.02026,299.95776], dtype=np.float32)
        self.RH = np.array([88.98621,79.98621], dtype=np.float32)
        self.T_500MB = np.array([284.5984,284.3484], dtype=np.float32)
        self.P_500MB = 700
        result = LiftedIndex.execute(self.P, self.T, self.RH, self.T_500MB, self.P_500MB)
        correct = np.array([-3.70840454, 0.28149414], dtype=np.float32)
        if not np.allclose(result, correct, .001, .001):
            self.fail("Wrong answer:"+ repr(result))            
