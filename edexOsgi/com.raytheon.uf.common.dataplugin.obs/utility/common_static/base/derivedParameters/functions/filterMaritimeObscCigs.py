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
