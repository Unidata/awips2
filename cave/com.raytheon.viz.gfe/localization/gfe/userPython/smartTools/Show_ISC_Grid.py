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
# Show_ISC_Grid - Version 3.04 (Tim Barker - SOO Boise, ID)
#
#   Creates a discrepancy grid with the gridpoints inside your CWA set to
#   the difference between it and its neighbor.  When their are two
#   neighbors (i.e., when your border has a corner) it sets the difference
#   to the one with the largest magnitude.  For vector elements only the
#   magnitude differences are shown - not the direction differences.
#
#   Positive discrepancies mean your gridpoint is above the neighbor,
#   negative discrepancies mean your gridpoint is below the neighbor
#
#   Configuration of maskNonViolators allows the grid to only display
#   values in gridpoints that violate the NDFD criteria for that pair.
#
# Author: hansen
#  2006-01-23 - Barker - Version 3.04. Added thresholds for more parameters
#  2006-01-19 - Barker - Version 3.03. Fixed another problm in ISC_Utility
#               for non-square GFE domains.
#  2006-01-17 - Barker - Version 3.02. Fixed problem in ISC_Utility for
#               non-square GFE domains.
#  2006-01-13 - Barker - Version 3.01. Changed for new NDFD algorithm.
#               Thresholds now vary at each gridpoint - overall average
#               difference along border must be less than average threshold
#               along that border (a much better algorithm!). All
#               calculations done in ISC Utility routine.
#  2005-03-30   Barker - added code to remove values where they are smaller
#               than the threshold.
#  2004-11-17   Mathewson - baselined by FSL
#  2004-10-31 - Version 2.4.  Remove by-length calculations. Fix error in
#               sky threshold.  Fix Status Bar messages for IFPS 16. Fix
#               accumulative elements.
#  2004-10-12 - Version 2.3.  Remove restriction that ISC grids must be
#               displayed (not needed in IFPS 15 or 16).  Cuts down on number
#               of cached grids it stores and increases time between
#               recomputes of cached grids
#  2004-09-30 - Version 2.2. Changes to ISC_Utility_Local for handling
#               specified edit areas (which can include marine) rather than
#               edit areas based only on CWA name.  Thresholds changed
#               extensively to add thresholds based on grid values.  Code
#               to eliminate border pairs with large elevation differences
#               changed to more reasonable code, since NDFD fixed their code.
#  2004-09-05 - Version 2.1. Changes to ISC_Utility_Local for handling
#               areas where no neighbor exists.
#  2004-09-05 - Version 2.01 - clip the values so that it is guaranteed to
#               fit within the discrepancy grid limits.  This shouldn't
#               be necessary but did it to be sure.
#  2004-08-15 - Version 2.0 - Just gives the grid of the max difference
#               for any point inside the CWA that has neighbors outside the
#               CWA.
#  2004-06-20 - Version 1.1 - Modified to use modified ISC_Utility_Local
#               routines to give discrepancies identical (close) to NDFD.
#  
# ----------------------------------------------------------------------------
#  C O N F I G U R A T I O N   S E C T I O N
#
maskNonViolators = 0 # 0 to show all differences, 1 to show only 'violators'
#
#  Rest of Configuration in ISC_Utility
#  
#  E N D   C O N F I G U R A T I O N   S E C T I O N
# ----------------------------------------------------------------------------
ToolType = "numeric"
WeatherElementEdited = "None"
ScreenList = ["SCALAR", "VECTOR"]
import numpy

import ISC_Utility_Local, GridInfo
import SmartScript

GridType = ISC_Utility_Local.ISC_Utility.GridType

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss = dbss
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessTool(self, WEname):
        self._utility = ISC_Utility_Local.ISC_Utility_Local(self._dbss, None)

    def execute(self, WEname, variableElement_GridInfo, GridTimeRange):
        "Make a grid of ISC discrepancies"
        variableElement_GridInfo = GridInfo.GridInfo(gridParmInfo=variableElement_GridInfo)
        wxtype = variableElement_GridInfo.type()
        #
        #  Get grid with the maximum discrepancy with neighboring points
        #
        diffGrid = self._utility._getDiffGrid(WEname, GridTimeRange,
                                            vectordir=0, maskNonViolators=maskNonViolators)
        if diffGrid is None:
            self.statusBarMsg("No ISC data for %s" % WEname, "A")
            self.cancel()
        #
        #  Find max absolute discrepancy on grid
        #
        minVal = numpy.minimum.reduce(numpy.minimum.reduce(diffGrid))
        maxVal = numpy.maximum.reduce(numpy.maximum.reduce(diffGrid))
        maxVal = max(abs(minVal), abs(maxVal))
        minVal = - maxVal
        if maxVal - minVal == 0:
            minVal -= 1
            maxVal += 1
        minVal = float(minVal)
        maxVal = float(maxVal)
        #
        # get the parms max/min allowable values
        #
        parmRange = variableElement_GridInfo.maxLimit() - \
          variableElement_GridInfo.minLimit()
        #
        # clip the values
        #
        diffGrid = numpy.clip(diffGrid, - parmRange, parmRange)
        #
        # create the discrepancy grid
        #
        newname = WEname
        if GridType.VECTOR.equals(wxtype):
            newname = WEname + "Spd"
        self.createGrid("ISCDisc", newname, "SCALAR", diffGrid,
                GridTimeRange, descriptiveName=WEname + "Disc",
                timeConstraints=variableElement_GridInfo.tc(),
                rateParm=0, precision=variableElement_GridInfo.precision(),
                minAllowedValue= - parmRange,
                maxAllowedValue=parmRange,
                units=variableElement_GridInfo.units())

        #    
        # Set up the active grid with Discrepancy color table
        #
        self.setActiveElement("ISCDisc", newname, "SFC", GridTimeRange,
                              colorTable="GFE/Discrepancy",
                              minMax=(- maxVal, maxVal))
        #
        #  If a vector - then create another grid with the vector difference
        #
        if GridType.SCALAR.equals(wxtype):
            return
        #
        #  Get grid with the maximum discrepancy with neighboring points
        #
        diffGrid = self._utility._getDiffGrid(WEname, GridTimeRange,
                                            vectordir=1, maskNonViolators=maskNonViolators)
        #
        #  Find max absolute discrepancy on grid
        #
        minVal = numpy.minimum.reduce(numpy.minimum.reduce(diffGrid))
        maxVal = numpy.maximum.reduce(numpy.maximum.reduce(diffGrid))
        maxVal = max(abs(minVal), abs(maxVal))
        minVal = - maxVal
        if ((maxVal - minVal) == 0):
            minVal -= 1
            maxVal += 1
        minVal = float(minVal)
        maxVal = float(maxVal)
        #
        # get the parms max/min allowable values
        #
        parmRange = variableElement_GridInfo.maxLimit() - \
          variableElement_GridInfo.minLimit()
        #
        # clip the values
        #
        diffGrid = numpy.clip(diffGrid, - 180.0, 180.0)
        #
        # create the discrepancy grid
        #
        newname = WEname + "Direc"
        self.createGrid("ISCDisc", newname, "SCALAR", diffGrid,
                GridTimeRange, descriptiveName=WEname + "Disc",
                timeConstraints=variableElement_GridInfo.tc(),
                rateParm=0, precision=variableElement_GridInfo.precision(),
                minAllowedValue= - 180.0,
                maxAllowedValue=180.0,
                units=variableElement_GridInfo.units())
        #    
        # Set up the active grid with Discrepancy color table
        #
        self.setActiveElement("ISCDisc", newname, "SFC", GridTimeRange,
                              colorTable="GFE/Discrepancy",
                              minMax=(- maxVal, maxVal))
