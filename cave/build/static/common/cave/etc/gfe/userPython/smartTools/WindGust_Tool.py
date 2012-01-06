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
# TC_WindGust_Tool
#
# Author: G. RADER
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "WindGust"
from numpy import *

import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, Wind):
        "Calculates Wind Gusts with "

        magnitude = Wind[0]

        WindGust = zeros(magnitude.shape, magnitude.dtype)
        WindGust = where(greater_equal(magnitude,  0.0), magnitude*1.40, WindGust)
        WindGust = where(greater_equal(magnitude, 46.0), magnitude*1.38, WindGust)
        WindGust = where(greater_equal(magnitude, 47.0), magnitude*1.36, WindGust)
        WindGust = where(greater_equal(magnitude, 48.0), magnitude*1.34, WindGust)
        WindGust = where(greater_equal(magnitude, 49.0), magnitude*1.32, WindGust)
        WindGust = where(greater_equal(magnitude, 50.0), magnitude*1.30, WindGust)
        WindGust = where(greater_equal(magnitude, 62.0), magnitude*1.28, WindGust)
        WindGust = where(greater_equal(magnitude, 63.0), magnitude*1.26, WindGust)
        WindGust = where(greater_equal(magnitude, 64.0), magnitude*1.24, WindGust)
        WindGust = where(greater_equal(magnitude, 65.0), magnitude*1.23, WindGust)
        
        return WindGust

