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
import AdiabaticTemp

class TestAdiabaticTemp(unittest.TestCase):
    def setup(self):
        pass
    
    def testExecute(self):
        intemp = np.array([268.54, 270.5], dtype=np.float32)
        inpres = np.array([770, 770], dtype=np.float32)
        
        result = AdiabaticTemp.execute(intemp, inpres)
        correct = np.array([278.37, 281.93], dtype=np.float32)
        self.failUnless(np.allclose(result,correct, 0.01, 0.01), "result = " + repr(result))