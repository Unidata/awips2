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
# Show_ISC_Area - Version 3.04 (Tim Barker - SOO Boise, ID)
#
#   Creates an edit area containing points in your CWA that when compared
#   to a neighbors ISC grids, violate the discrepancy criteria for this
#   parameter and point.
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
#  2004-11-17   Mathewson - Baselined
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
#  2004-09-05 - Version 2.1. Changes to ISC_Utility_Local for
#               handling areas where no neighbor exists.
#  2004-08-15 - Version 2.0. Threshold is now different for each neighbor CWA
#               and based on the average elevation difference of the points
#               inside our CWA that border this neighbor CWA - and for which
#               ISC data is available and at least one of the border points
#               meet any criteria-based test (like wind speed), and do not
#               have too great an average elevation difference to be eliminated.
#               Thgis is what NDFD does. (Clear as mud...huh?)
#  2004-06-20 - Version 1.1. Modified to use modified ISC_Utility_Local
#               routines so that discrepancies are identical (close) to NDFD
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "None"
ScreenList = ["SCALAR","VECTOR"]
import numpy

import ISC_Utility_Local,GridInfo
import SmartScript

GridType = ISC_Utility_Local.ISC_Utility.GridType

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss = dbss
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessTool(self, WEname):
        self._utility = ISC_Utility_Local.ISC_Utility_Local(self._dbss, None)

    def execute(self, WEname, variableElement_GridInfo, GridTimeRange):
        "Make an edit area of discrepant gridpoints"
        #
        #  Get grid of 0/1 where violating threshold
        #
        variableElement_GridInfo = GridInfo.GridInfo(gridParmInfo=variableElement_GridInfo)
        wxtype=variableElement_GridInfo.type()
        if GridType.SCALAR.equals(wxtype):
            diffGrid=self._utility._getDiffGrid(WEname,GridTimeRange,vectordir=0,maskNonViolators=1)
            if diffGrid is None:
                self.statusBarMsg("No ISC data for %s"%WEname,"A")
                self.cancel() 
            violated=numpy.not_equal(diffGrid,0.0)
        else:
            diffSpd=self._utility._getDiffGrid(WEname,GridTimeRange,vectordir=0,maskNonViolators=1)
            diffDir=self._utility._getDiffGrid(WEname,GridTimeRange,vectordir=1,maskNonViolators=1)
            if ((diffSpd is None)or(diffDir is None)):
                self.statusBarMsg("No ISC data for %s"%WEname,"A")
                self.cancel() 
            violated=numpy.logical_or(numpy.not_equal(diffSpd,0.0),numpy.not_equal(diffDir,0.0))
        #
        #  Set edit area equal to points that violated any threshold
        #
        area = self.decodeEditArea(violated)
        self.setActiveEditArea(area)
        #
        #  Status Bar message so it is clear the tool ended
        #
        num=numpy.add.reduce(numpy.add.reduce(violated))
        if num==0:
            msg="No border pairs violated discrepancy criteria for %s"%WEname
            self.statusBarMsg(msg,"A")
        else:
            msg="EditArea set to %d border pairs that violate criteria for %s"%(num,WEname)
            self.statusBarMsg(msg,"R")
