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
# Calculates the heatIndex from temperature(C) and dewpoint temperatures(C)
# ----------------------------------------------------------------

import numpy
from numpy import power

const1 = 0.0091379024
const2 = 6106.396
    
def calculate(T, DpT):
    # T and DpT have to be in C for the below calculation
    badValue = numpy.where(T < 26.5,1,numpy.where(DpT > T,1,0))
    
    TK = T + 273.15 
    DpTK = DpT + 273.15
# Legacy src/meteoLib/calcrh.f
    exponent = numpy.where(T < 80.0, const1 * (TK - DpTK) + const2/TK - const2/DpTK,const1 * (T - DpT) + const2/T - const2/DpT)
    RH = 100 * numpy.exp(exponent)

    
    T1 = (T * 1.8) + 32
    T1_sq = T1 * T1
    RH_sq = RH * RH        
    
    # the Lans Rothfusz formula
    H1 = -42.379 + (2.04901523 * T1) + (10.14333127 * RH)
    H2 = (-0.22475541 * T1 * RH) - (0.00683783 * T1_sq) - (0.05481717 * RH_sq)
    H3 = (0.00122874 * T1_sq * RH) + (0.00085282 * T1  * RH_sq)
    H4 = (-0.00000199 * T1_sq * RH_sq)
    Hi = H1 + H2 + H3 + H4
    
    HiC = (Hi - 32) / 1.8 #convert F to C
    #Returned in Celsius
    return numpy.where(badValue == 1, -9999.0, HiC)