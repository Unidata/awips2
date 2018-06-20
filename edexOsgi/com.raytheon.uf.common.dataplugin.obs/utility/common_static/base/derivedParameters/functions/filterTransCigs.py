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

import numpy as np
from numpy import *

def execute(cig, cigBase, index):
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])
    cOuterDim=len(cig)
    cInnerDim=len(cig[0])
    if cbOuterDim == cInnerDim:
        outerCount = 0
        innerCount = 0     
        for i in cig:
            for j in i:
                if ("BKN" in j) or ("OVC" in j) or ("VV" in j):
                    cigBase[outerCount][innerCount]=str(nan)                    
                innerCount = innerCount + 1
                if innerCount > (cbInnerDim-1):
                    innerCount = 0
                    outerCount = outerCount + 1
    else:
        target = open('/tmp/targetFile.txt','w')
        target.write('cig and cigBase dimensions dont match')
        target.close()
    return cigBase[:,index]
