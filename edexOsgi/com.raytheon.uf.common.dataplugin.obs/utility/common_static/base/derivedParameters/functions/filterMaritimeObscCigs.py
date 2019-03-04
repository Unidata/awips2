##
##

import numpy as np
from numpy import *

#VFR
def execute(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cigBase[count]=-9999
        if cigBase[count] < 6:
            cigBase[count]=-9999   
    return cigBase

#MVFR
def execute2(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cigBase[count]=-9999 
        if (cigBase[count] < 4) or (cigBase[count] > 5):
            cigBase[count]=-9999                    
    return cigBase

#IFR
def execute3(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cigBase[count]=-9999
        if (cigBase[count] < 2) or (cigBase[count] > 3):
            cigBase[count]=-9999   
    return cigBase

#LIFR
def execute4(cig, cigBase):
    for count, i in enumerate(cig):
        if (i < 5):
            cigBase[count]=-9999
        if cigBase[count] > 1:
            cigBase[count]=-9999
    return cigBase
