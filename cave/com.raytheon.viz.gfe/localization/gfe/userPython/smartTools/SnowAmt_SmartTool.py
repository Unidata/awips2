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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Numeric_SnowAmt_SmartTool
#
# Author: billingsley
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "SnowAmt"
from numpy import *

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
  
    def execute(self, Topo, T, FzLevel, QPF):        
        "Determines SnowAmt from QPF, T, FzLevel and Topo information"
##        if T < 9:   # This way 8.5 will be treated as 8
##            SnowRatio = 20
##        elif T >= 30:
##            SnowRatio = 7
##        else:
##            SnowRatio = SnowRatioDict[int(T)]
        SnowRatio = zeros(T.shape)        
        SnowRatio = where(less(T, 9), 20,
                        where(greater_equal(T, 30), 7,
                              self.linear(9,29,18,8,T)))
                    
##        # Determine new value
##        if (FzLevel-1000) <= Topo:
##            SnowAmt = SnowRatio * QPF
##        else:
##            SnowAmt = 0
        SnowAmt = zeros(T.shape)
        SnowAmt = where(less_equal(FzLevel-1000, Topo), SnowRatio*QPF,0)

        # Return the new value
        return SnowAmt

    def linear(self, xmin, xmax, ymin, ymax, we):
        m = (ymax - ymin) / (xmax - xmin + .0000001)
        b = ymin - m * xmin
        return m * we + b
## POINT-BASED VERSION
# This dictionary maps temperature to snow ratio.
# The boundaries are handled in the code below.
##SnowRatioDict = {
##    9:18,
##    10:18,
##    11:17,
##    12:17,
##    13:16,
##    14:15,
##    15:14,
##    16:13,
##    17:13,
##    18:12,
##    19:12,
##    20:11,
##    21:11,
##    22:11,
##    23:10,
##    24:10,
##    25:9,
##    26:9,
##    27:8,
##    28:8,
##    29:8
##    }
    
######################
##def SnowAmt_SmartTool(QPF, T, FzLevel, Topo):
##    "Determines SnowAmt from QPF, T, FzLevel and Topo information"
    
##    if T < 9:   # This way 8.5 will be treated as 8
##        SnowRatio = 20
##    elif T >= 30:
##        SnowRatio = 7
##    else:
##        SnowRatio = SnowRatioDict[int(T)]
        

##    # Determine new value
##    if (FzLevel-1000) <= Topo:
##        SnowAmt = SnowRatio * QPF
##    else:
##        SnowAmt = 0

##    # Return the new value
##    return SnowAmt

