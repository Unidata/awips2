##
##

import numpy as np
from numpy import *

def execute(cig, cigBase):
    for count, i in enumerate(cig):
        if (i >= 5):
            cigBase[count]=-9999
    return cigBase
