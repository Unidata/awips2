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
# getSumGrids - generic smart tool to sum into the active element the grids
#            defined by varDict["Element"]. This tool is designed to be
#            called by a procedure and generally only for a snow or qpf grid
#
# Author: Paul Jendrowski WFO Blacksburg, VA (RNK)
#         paul.jendrowski@noaa.gov
# Version: 1.0    Date: 02/21/2003
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "variableElement"
from numpy import *

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
# This tool is normally run from a procedure so hide it!
ScreenList = [""]

# Set up Class
import SmartScript
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, varDict):
        "Sums a grid into current element. This tool should be run from a procedure and not interactively."

        if varDict is None or "Element" not in varDict:
            msg="getSumGrids - No element defined."
            msg += " This tool should not be run interactively!"
            self.noData(msg)
        we = varDict["Element"]
        grid = self.getGrids("Fcst",we,"SFC",GridTimeRange,mode="Sum")

        # Return the new value
        return grid
