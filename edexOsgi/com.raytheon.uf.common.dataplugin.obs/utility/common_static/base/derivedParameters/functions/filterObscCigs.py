##
##

import numpy as np
from numpy import *

#MVFR
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
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cigBase[outerCount][innerCount]=str(nan) 
                if (cigBase[outerCount][innerCount] > 3000) or (cigBase[outerCount][innerCount] < 1000):
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

#IFR
def execute2(cig, cigBase, index):
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])
    cOuterDim=len(cig)
    cInnerDim=len(cig[0])
    if cbOuterDim == cInnerDim:
        outerCount = 0
        innerCount = 0     
        for i in cig:
            for j in i:
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cigBase[outerCount][innerCount]=str(nan)
                if (cigBase[outerCount][innerCount] >= 1000) or (cigBase[outerCount][innerCount] < 500):
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

#LIFR
def execute3(cig, cigBase, index):
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])
    cOuterDim=len(cig)
    cInnerDim=len(cig[0])
    if cbOuterDim == cInnerDim:
        outerCount = 0
        innerCount = 0     
        for i in cig:
            for j in i:
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cigBase[outerCount][innerCount]=str(nan)
                if cigBase[outerCount][innerCount] >= 500:
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

#VFR
def execute4(cig, cigBase, index):
    cbOuterDim=len(cigBase)
    cbInnerDim=len(cigBase[0])
    cOuterDim=len(cig)
    cInnerDim=len(cig[0])
    if cbOuterDim == cInnerDim:
        outerCount = 0
        innerCount = 0     
        for i in cig:
            for j in i:
                if ("BKN" not in j) and ("OVC" not in j) and ("VV" not in j):
                    cigBase[outerCount][innerCount]=str(nan)
                if cigBase[outerCount][innerCount] <= 3000:
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
