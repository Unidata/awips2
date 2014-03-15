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

from numpy import where
from numpy import equal

# Provide unit constants and conversions

def celciusToKelvin(celcius):
    return _add(celcius, 273.15)

def pascalToMilliBar(pascal):
    return _multiply(pascal, 0.01)

def knotToMetersPS(knot):
    return _multiply(knot, 0.514)

# water at 4 degrees celcius
def kgPerSquareMeterToInchOfWater(kgPm2):
    return _multiply(kgPm2, 0.03937117)

def mileToMeter(mile):
    return _multiply(mile, 1609.344)


def inchToMillimeter(inch):
    return _multiply(inch, 25.4)

def _multiply(array,scalar,dataMissing=-9999):
    return where(equal(array, dataMissing), dataMissing, array * scalar)

def _add(array,offset,dataMissing=-9999):
    return where(equal(array, dataMissing), dataMissing, array + offset)
