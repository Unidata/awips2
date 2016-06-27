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

#MVFR
def execute(cig, index, cigBase):
    outerLen=len(cig)
    innerLen=len(cig[0])
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])    
    endElement=outerLen*innerLen
    if cbOuterDim == innerLen:
        outerCount = 0
        innerCount = 0     
        for counter1, i in enumerate(cig):
            for counter2, j in enumerate(i):
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cig[counter1][counter2]=''
                if (cigBase[outerCount][innerCount] > 3000) or (cigBase[outerCount][innerCount] < 1000):
                    cig[counter1][counter2]=''
                innerCount = innerCount + 1
                if innerCount > (cbInnerDim-1):
                    innerCount = 0
                    outerCount = outerCount + 1
    else:
        target = open('/tmp/targetFile.txt','w')
        target.write('cig and cigBase dimensions dont match')
        target.close()
    cig = cig.flatten() 
    return cig[index:endElement:6]

#IFR
def execute2(cig, index, cigBase):
    outerLen=len(cig)
    innerLen=len(cig[0])
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])    
    endElement=outerLen*innerLen
    if cbOuterDim == innerLen:
        outerCount = 0
        innerCount = 0     
        for counter1, i in enumerate(cig):
            for counter2, j in enumerate(i):
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cig[counter1][counter2]=''
                if (cigBase[outerCount][innerCount] >= 1000) or (cigBase[outerCount][innerCount] < 500):
                    cig[counter1][counter2]=''
                innerCount = innerCount + 1
                if innerCount > (cbInnerDim-1):
                    innerCount = 0
                    outerCount = outerCount + 1
    else:
        target = open('/tmp/targetFile.txt','w')
        target.write('cig and cigBase dimensions dont match')
        target.close()
    cig = cig.flatten() 
    return cig[index:endElement:6]

#LIFR
def execute3(cig, index, cigBase):
    outerLen=len(cig)
    innerLen=len(cig[0])
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])    
    endElement=outerLen*innerLen
    if cbOuterDim == innerLen:
        outerCount = 0
        innerCount = 0     
        for counter1, i in enumerate(cig):
            for counter2, j in enumerate(i):
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cig[counter1][counter2]=''
                if cigBase[outerCount][innerCount] >= 500:
                    cig[counter1][counter2]=''
                innerCount = innerCount + 1
                if innerCount > (cbInnerDim-1):
                    innerCount = 0
                    outerCount = outerCount + 1
    else:
        target = open('/tmp/targetFile.txt','w')
        target.write('cig and cigBase dimensions dont match')
        target.close()
    cig = cig.flatten() 
    return cig[index:endElement:6]

#VFR
def execute4(cig, index, cigBase):
    outerLen=len(cig)
    innerLen=len(cig[0])
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])    
    endElement=outerLen*innerLen
    if cbOuterDim == innerLen:
        outerCount = 0
        innerCount = 0     
        for counter1, i in enumerate(cig):
            for counter2, j in enumerate(i):
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cig[counter1][counter2]=''
                if cigBase[outerCount][innerCount] <= 3000:
                    cig[counter1][counter2]=''
                innerCount = innerCount + 1
                if innerCount > (cbInnerDim-1):
                    innerCount = 0
                    outerCount = outerCount + 1
    else:
        target = open('/tmp/targetFile.txt','w')
        target.write('cig and cigBase dimensions dont match')
        target.close()
    cig = cig.flatten() 
    return cig[index:endElement:6]