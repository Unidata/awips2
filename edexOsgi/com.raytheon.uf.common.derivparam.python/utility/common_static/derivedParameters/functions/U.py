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

# ----------------------------------------------------------------
# Calculate U component from wind speed and direction
# 
# ----------------------------------------------------------------

import numpy
from numpy import power
from numpy import sin

const1 = 0.0174533

def execute(wSp, WD):
    return calculate(wSp, WD)
    
def calculate(wSp, wD):
    wD = numpy.where(wD < 0, -9999.0, numpy.where(wD > 360, -9999.0,wD))
    wSp = numpy.where(wSp < 0, -9999.0, numpy.where(wSp > 250, -9999.0,wSp))
    theta = wD * const1
    U = (-1 * wSp) * numpy.sin(theta)
    return U