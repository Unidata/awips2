##
##

import numpy as np
from numpy import *
np.set_printoptions(suppress=True)

def execute(vis):
    for count, i in enumerate(vis):
        if (i < 3) or (i > 5):
            vis[count]=-9999
    return vis

def execute2(vis):
    for count, i in enumerate(vis):
        if (i < 1) or (i > 3):
            vis[count]=-9999
    return vis

def execute3(vis):
    for count, i in enumerate(vis):
        if i >= 1:
            vis[count]=-9999
    return vis

def execute4(vis):
    for count, i in enumerate(vis):
        if i <= 5:
            vis[count]=-9999
    return vis
