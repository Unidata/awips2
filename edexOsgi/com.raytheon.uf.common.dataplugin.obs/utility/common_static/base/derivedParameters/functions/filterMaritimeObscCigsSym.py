##
##

import numpy as np
from numpy import *

#VFR
def execute(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cig[count]=-9999
#        if (cigBase[count] < "33-49") and (cigBase[count] != "50-65") and (cigBase[count] != "70-80") and (cigBase[count] != ">85"):
        if cigBase[count] < 6:
            cig[count]=-9999 
    return cig

#MVFR
def execute2(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cig[count]=-9999
#        if (cigBase[count] != "10-19") and (cigBase[count] != "20-32"):
        if (cigBase[count] < 4) or (cigBase[count] > 5):
            cig[count]=-9999 
    return cig

#IFR
def execute3(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cig[count]=-9999
#        if (cigBase[count] != "7-9") and (cigBase[count] != "4-6"):
        if (cigBase[count] < 2) or (cigBase[count] > 3):
            cig[count]=-9999 
    return cig

#LIFR
def execute4(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cig[count]=-9999
#        if (cigBase[count] != "2-3") and (cigBase[count] != "0-1"):
        if cigBase[count] > 1:
            cig[count]=-9999 
    return cig

