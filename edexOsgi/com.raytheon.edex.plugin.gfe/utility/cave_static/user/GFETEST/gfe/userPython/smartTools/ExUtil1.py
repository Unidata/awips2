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
# ExUtil1
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "T"
from numpy import *

import SmartScript
import Common

VariableList = [("Model:" , "", "D2D_model")]

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss = dbss
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, Topo, varDict):
        "This tool accesses T grids directly"
        self._common = Common.Common(self._dbss)

        model = varDict["Model:"]

        # Convert Topo to meters
        topo_M = self._common._convertFtToM(Topo)

        # Make a sounding cubes for T
        # Height will increase in the sounding and be the
        # first dimension
        levels = ["MB1000","MB850", "MB700","MB500"]
        gh_Cube, t_Cube = self.makeNumericSounding(
            model, "t", levels, GridTimeRange)

        print "Cube shapes ", gh_Cube.shape, t_Cube.shape

        # Make an initial T grid with values of -200
        # This is an out-of-range value to help us identify values that
        # have already been set.
        T = (Topo * 0) - 200

        # Work "upward" in the cubes to assign T
        # We will only set the value once, i.e. the first time the
        # gh height is greater than the Topo
        # For each level
        for i in xrange(gh_Cube.shape[0]):
                # where ( gh > topo and T == -200),
                #       set to t_Cube value, otherwise keep value already set))
                T = where(logical_and(greater(gh_Cube[i], topo_M), equal(T,-200)), t_Cube[i], T)

        # Convert from K to F
        T_F = self.convertKtoF(T)

        return T_F    
