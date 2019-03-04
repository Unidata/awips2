##
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
