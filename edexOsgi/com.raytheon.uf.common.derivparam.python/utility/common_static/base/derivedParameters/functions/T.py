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
# Returns tempFromTenths if present, otherwise returns temperature
# 
# ----------------------------------------------------------------
import numpy
from numpy import concatenate
from numpy import isnan
from unit import celciusToKelvin 

# build an array that contains the higher resolution tempFromTenths where possible, otherwise use the lower resolution temperature
def combineTemps(temperature,tempFromTenths):
    return numpy.where(isnan(tempFromTenths), temperature, tempFromTenths)
    
##
# Returns tempFromTenths if present, otherwise returns temperature
#
# @param temperature : temperature
# @param tempFromTenths : tempFromTenths
# @return:
# @rtype:
# 
def execute1(temperature,tempFromTenths):
    return celciusToKelvin(combineTemps(temperature,tempFromTenths))

def execute2(temperature, temp2):
    temp2 = temp2.reshape(-1, 1);
    result = concatenate((temp2, temperature), 1)
    return result

def execute3(temperatureStation,tempFromTenthsStation):
    return celciusToKelvin(combineTemps(temperatureStation, tempFromTenthsStation))