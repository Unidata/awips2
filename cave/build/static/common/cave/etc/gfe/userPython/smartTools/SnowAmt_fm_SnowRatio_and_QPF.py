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
# SnowAmt_fm_SnowRatio_and_QPF
#
# Author: Brian Meade, GRR, 12/07
# ----------------------------------------------------------------------------
# short tool which simply computes SnowAmt by multiplying SnowRatio times QPF.
# SnowRatio is a local grid which is created by a smartInit which applies
# the Cobb Snow Algoritm
#-----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "SnowAmt"
from numpy import *

# Set up Class
import SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)


    def execute(self, QPF, T, SnowRatio, SnowAmt, varDict, GridTimeRange):
 
        snowfall = QPF * SnowRatio
        snowfall = where(greater(T, 32.0), pow(36.0 - T,2)/16.0 * snowfall , snowfall) 
        snowfall[greater(T, 35.0)] = 0.0

        # Return the new value
        return snowfall

       
