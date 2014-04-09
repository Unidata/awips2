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
# Calculate mixing ratio from pressure, temperature and RH
# 
# ----------------------------------------------------------------
from numpy import exp
from numpy import zeros

##
# Calculate mixing ratio from pressure, temperature, and RH
#
# @param P: pressure in millibars
# @param T: temperature in degrees C or K
# @param RH: relative humidity from 0.0 to 100.0 percent
# @return mixing ratio 
# @rtype: numpy array or Python float
def execute(P,T,RH):
   "Calculate mixing ratio from pressure, temperature, and RH"
   
   eee = -0.0091379024 * T
   eee -= 6106.396 / T
   eee += 28.48859
   eee = exp(eee)
   eee = RH * eee
   denom = P - 0.001607717 * eee 
   mixRat = eee / denom
   return mixRat
