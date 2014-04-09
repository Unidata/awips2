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
# Calculates the Wind Chill from temperature(C) and windSpeed(km/h)
# ----------------------------------------------------------------

import numpy
from numpy import power
    
def calculate(T,wSpd):

    # T and DpT have to be in C for the below calculation
    badValue = numpy.where(T > 16,1,0)
    noChill = numpy.where(wSpd < 6.4,1,0)
    wSpd = numpy.where(wSpd > 128.75, 128.75, wSpd)
    wSpd = numpy.power(wSpd,0.16)
    Wc = 13.12 + 0.6215 * T - 11.37 * wSpd + 0.3965 * T * wSpd
    #Returned in Celsius
    return numpy.where(badValue == 1, -9999.0, numpy.where(noChill == 1, T,Wc))