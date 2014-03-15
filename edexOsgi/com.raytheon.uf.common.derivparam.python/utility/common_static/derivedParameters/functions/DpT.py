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
# Calculate Dewpoint Temperature (K) from Temperature (K) and Relative
# Humdity(0 to 100).
# ----------------------------------------------------------------
import numpy
from numpy import clip
from numpy import log
from numpy import sqrt
from numpy import zeros
from numpy import where
from numpy import equal
from numpy import isnan, NaN
from unit import celciusToKelvin

# build an array that contains the higher resolution dpFromTenths where possible, otherwise use the lower resolution dewpoint
def combineTemps(dewpoint,dpFromTenths):
    return numpy.where(numpy.isnan(dpFromTenths), dewpoint, dpFromTenths)

##
# Calculate Dewpoint Temperature (K) from Temperature (K) and Relative
# Humdity(0 to 100).
# This function can operate on numpy arrays of the appropriate values.
#
# @param T: Temperature in degrees K
# @param RH: Relative humidity from 0 to 100
# @return: Dewpoint temperature in degrees K
# @rtype: numpy array of Python floats or Python float
def execute1(T,RH):
   "Calculate dewpoint temperature(K) from temperature(K) and relative \
   humidity(0 to 100)."
   rhqc=clip(RH,1.0,100.0)
   b=0.0091379024*T
   b += 6106.396/T
   b -= log(rhqc/100)
   val = b*b
   val -= 223.1986
   val = sqrt(val)
   DpT = b-val
   DpT /= 0.0182758048
   return DpT

def execute3(P,T,SHx):
    eee=P*SHx/(622.0+0.378*SHx)
    b=26.66082-log(eee)
    result = (b-sqrt(b*b-223.1986))/0.0182758048
    result[isnan(T)] = NaN
    result[eee>980.5386] = NaN
    result[eee<3.777647E-05] = NaN
    return result

# @param dewpoint: dewpoint in degrees C
# @param dpFromTenths: dewpoint from thenths in degrees C
# @return: Dewpoint temperature in degrees K
# @rtype: numpy array of Python floats or Python float
def execute4(dewpoint,dpFromTenths):
    return celciusToKelvin(combineTemps(dewpoint, dpFromTenths))

# @param dewpoint: dewpoint in degrees K
# @param temperature: Temperature in degrees K
# @param relHumidity: Relative humidity from 0 to 100
# @return: Dewpoint temperature in degrees K
# @rtype: numpy array of Python floats or Python float
def execute5(dewpoint,temperature,relHumidity):
    TdRH = execute1(temperature, relHumidity) 
    return combineTemps(TdRH,dewpoint)
