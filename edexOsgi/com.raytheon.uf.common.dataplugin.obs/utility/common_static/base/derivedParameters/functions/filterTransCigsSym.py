##
##

#cig[:,index::6]

import numpy as np
from numpy import *

def execute(cig, index):
    outerLen=len(cig)
    innerLen=len(cig[0])
    endElement=outerLen*innerLen
    for counter1, i in enumerate(cig):
        for counter2, j in enumerate(i):
            if ("BKN" in j) or ("OVC" in j) or ("VV" in j):
                cig[counter1][counter2]=''
    cig = cig.flatten() 
    return cig[index:endElement:6]
