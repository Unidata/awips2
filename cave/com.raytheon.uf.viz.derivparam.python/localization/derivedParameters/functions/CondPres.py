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

from numpy import zeros
from numpy import log
from numpy import sqrt

CONST_A = 0.0091379024
CONST_B = 6106.396
CONST_C = 223.1986
CONST_D = 0.0182758048
CONST_E = 0.37329638
CONST_F = 41.178204
CONST_G = 0.0015945203
CONST_H = 3.498257

##
# Calculate condensation pressure from pressure, temperature, and relative
# humidity
# @attention: Result may contain NaN
#
# @param p: Pressure in millibars
# @type p: scalar or numpy array
# @param t: Temperature in degrees Kelvin
# @type t: scalar or numpy array
# @param rh: Relative humidity from 0.0 to 100.0
# @type rh: scalar or numpy array
#
# @return: Condensation pressure in millibars 
# @rtype: numpy array of float
def execute(p, t, rh):
    rhqc = rh.clip(1.0,100.0)
    b = CONST_A * t + CONST_B / t - log(rhqc/100.0)
    tdp = (b - sqrt(b**2.0 - CONST_C)) / CONST_D
    tcp = tdp - (t-tdp) * (-CONST_E + CONST_F / t + CONST_G * tdp)
    q = p * (tcp/t) ** CONST_H
    return q
