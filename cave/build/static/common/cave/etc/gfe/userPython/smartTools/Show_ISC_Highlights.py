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
# Show_ISC_Highlights - Version 3.04 (Tim Barker - SOO Boise ID)
#
#   Highlight grid if it has a significant discrepancy with any of the ISC
#   neighbors.
#
# Author: hansen
# 2006-01-23 - Barker - Version 3.04. Added thresholds for more parameters
# 2006-01-19 - Barker - Version 3.03. Fixed another problm in ISC_Utility
#              for non-square GFE domains.
# 2006-01-17 - Barker - Version 3.02. Fixed problem in ISC_Utility for
#              non-square GFE domains.
# 2006-01-13 - Barker - Version 3.01. Changed for new NDFD algorithm.
#              Thresholds now vary at each gridpoint - overall average
#              difference along border must be less than average threshold
#              along that border (a much better algorithm!). All
#              calculations done in ISC Utility routine.
# 2004-11-17 - Mathewson - baselined by FSL
# 2004-10-31 - Version 2.4.  Remove by-length calculations. Fix error in
#              sky threshold.  Fix Status Bar messages for IFPS 16. Fix
#              accumulative elements.
# 2004-10-12 - Version 2.3.  Remove restriction that ISC grids must be
#              displayed (not needed in IFPS 15 or 16).  Cuts down on number
#              of cached grids it stores and increases time between
#              recomputes of cached grids
# 2004-09-30 - Version 2.2. Changes to ISC_Utility_Local for handling
#              specified edit areas (which can include marine) rather than
#              edit areas based only on CWA name.  Thresholds changed
#              extensively to add thresholds based on grid values.  Code
#              to eliminate border pairs with large elevation differences
#              changed to more reasonable code, since NDFD fixed their code.
#              Changed to make RED if the average discrepancy would violate
#              the 'by length' weighted average threshold (as NDFD does).
#              Turns ORANGE if it violates the threshold on ANY segment.
# 2004-09-05 - Version 2.1. Changes to ISC_Utility_Local for
#              handling areas where no neighbor exists.
# 2004-08-15 - Version 2.0. Thresholds are now hard-coded and derived by
#              average elevation difference for points considered (like NDFD)
#              but still WRONG (in my opinion).
# 2004-06-20 - Version 1.1. Various changes to vector checks to be somewhat
#              closer to NDFD checks - though they are in a state of flux.
# 2004-06-08 - Added support for multi-parms and vector parms
# 2004-05-19 - Tim Barker - SOO - Boise, ID
#              Changed to use new ISC_Utility_Local routines that make
#              discrepancy calculations identical to NDFD - and also to
#              color the gridblock red if ANY of the NDFD faces would be
#              "frowns".
#
#              To use this modified tool, you need to configure the
#              modified ISC_Utility_Local with a list of neighboring
#              sites (actually a list of edit areas for those sites),
#              and to set the algorithm to Neighboring Points
#
# ============================================================================
#
#  C O N F I G U R A T I O N   S E C T I O N
#
#  See ISC_Utility.
#
#  E N D   O F   C O N F I G U R A T I O N   S E C T I O N
#
#----------------------------------------------------------------------------
ToolType = "numeric"
WeatherElementEdited = "None"
ScreenList = ["SCALAR","VECTOR"]

import ISC_Utility_Local
import SmartScript
import time

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss = dbss
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessTool(self, WEname):
        pass
        self._utility = ISC_Utility_Local.ISC_Utility_Local(self._dbss, None)

    def execute(self, WEname, GridTimeRange):
        "Highlight gridboxes with ISC discrepancies"
        timetext=self._utility.makeTimeMsg(GridTimeRange)
        (numchecked,violate,warning)=self._utility._checkParmBorders(WEname, GridTimeRange)
        #
        #  If any of the associated grids have any borders that violate
        #  the threshold, color that grid 'red' in the grid manager.
        #  Warnings, or Partial failures, are for grids where only SHORT
        #  borders violate the threshold - and they are colored 'orange'
        #
        if (violate>0):
            self.highlightGrids(self.mutableID().modelName(), WEname, "SFC", 
              GridTimeRange, "red")
            msg="Failed: %s Discrepancies for %s"%(WEname,timetext)
            self.statusBarMsg(msg,"A")
        else:
            if (warning>0):
               self.highlightGrids(self.mutableID().modelName(), WEname, "SFC", 
                 GridTimeRange, "orange")
               msg="Partial: %s Discrepancies for %s"%(WEname,timetext)
               self.statusBarMsg(msg,"A")
            else:                
               self.highlightGrids(self.mutableID().modelName(), WEname, "SFC", 
                 GridTimeRange,"red",0)
               msg="Passed: %s Discrepancies for %s"%(WEname,timetext)
               self.statusBarMsg(msg,"R")
        return
