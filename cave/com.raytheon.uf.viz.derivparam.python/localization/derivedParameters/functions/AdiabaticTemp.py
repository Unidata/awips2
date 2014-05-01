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
###

## @file AdiabaticTemp.py
from numpy import exp

# Constants from the old C code.
const1 = 26.660820
const2 = 0.0091379024
const3 = 6106.396
const4 = 0.622
const5 = 2740.0

##
# The method that does the actual calculation.
# Adapted from the old adiabatic_te.c function.
# This method skips conversion of temp and pressure to NaN arrays
# and back again. It can be called directly if temp and pressure are
# guaranteed to be valid, or if they are already NaN arrays.
#
# @param temp: Temperature (K)
# @param pressure: Pressure (mb)
# @return: Adiabatic temperature (K)
# @rtype: numpy masked array
def calculate(temp, pressure):
    ee = exp(const1 - const2*temp - const3/temp)
    pme = pressure-ee
    ee *= const4
    ee = ee / pme
    
    result = temp * exp(const5*ee/temp)
    return result
    
##
# Calculate adaibatic temperature.
#
# @param temp: Temperature (K)
# @param pressure: Pressure (mb)
# @return: Adiabatic temperature    
def execute(temp, pressure):    
    result = calculate(temp, pressure)
    return result
