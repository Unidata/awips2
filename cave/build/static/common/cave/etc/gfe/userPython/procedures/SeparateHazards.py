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
# SeparateHazards
#
# Author: lefebvre
#
# This procedure reads the hazards grids and create new temporary hazard-type
# Weather Elements each of which contains a single hazard.  The format of
# the weather element name is hazPPS, where PP is the phenomena and S is the
# significance.
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Hazards"]


import SmartScript
import HazardUtils

# from Numeric import *

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss): 
        SmartScript.SmartScript.__init__(self, dbss) 
        self._dbss = dbss

    def execute(self):

        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)

        #see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in "+\
              "the GFE before running SeparateHazards.", "S")
            self.cancel()

        self.setToolType("numeric")

        sepStatus = self._hazUtils._separateHazardGrids()
        if sepStatus == HazardUtils.FAIL_REDUNDANT:
            self.statusBarMsg("Hazards Weather Element already separated." +\
              " No action taken. Hazards grid ignored.", "S")
        if sepStatus != HazardUtils.SUCCESS:
            self.cancel()

        return


