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
# MaxRH_Tool
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "MaxRH"
from numpy import *

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange):
        "Returns the maximum RH over the selected timeRange"
        
        # Fetch the maximum over the GridTimeRange
        MaxRH = self.getGrids("Fcst", "RH", "SFC", GridTimeRange, 'Max', 0)

        # Return the new value
        return MaxRH

