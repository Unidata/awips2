##
##

#cig[:,index::6]

import numpy as np
from numpy import *

def execute(cig):
    for count, i in enumerate(cig):
        if (i >= 5):
            cig[count]=-9999 
    return cig
