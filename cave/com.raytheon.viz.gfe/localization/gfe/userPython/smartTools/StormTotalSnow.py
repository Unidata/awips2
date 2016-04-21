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
# StormTotalSnow
#
# This very simple tool will assign the sum of the SnowAmt grids over the time range of 
# the StormTotalSnow grid. For example, if the user creates/stretches a StormTotalSnow
# grid which runs from 00z to 18z and runs this tool, the result will be the addition
# of the 00-06z, 06z-12z, and 12z-18z SnowAmt grids.
#
# Author: Brian Meade, GRR, 12/7/06
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "StormTotalSnow"
from numpy import *
HideTool = 0

## Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  Called once for each grid
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, GridTimeRange, StormTotalSnow, SnowAmt):
        "Put your tool description here"

        StormTotalSnow=self.getGrids("Fcst","SnowAmt","SFC",GridTimeRange,"Sum")

        # Return the new value
        return StormTotalSnow
    
