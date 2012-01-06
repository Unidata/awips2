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
# calculate virtual temperature from the pressure, 
#    temperature, and relative humidity.
# ----------------------------------------------------------------

from numpy import clip
from numpy import exp

def execute(P,T,RH):
    "Calculate virtual temperature from temperature(K), relative \
    humidity(0 to 100) and Pressure."

    rhqc=clip(RH,1.0,100.0)
    k = T
    eee = exp(21.0827887-0.0091379024*k-6106.396/k)
    tv = T * P / (P - RH * eee)
    return tv
