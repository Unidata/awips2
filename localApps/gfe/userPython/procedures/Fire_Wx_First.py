# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Fire_Wx_First.py
#
# Author: dtomalak
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]
import LogStream, time
from math import *

# The ToolList is optional, but recommended, if you are calling
# Smart Tools from your Script.
# If present, it can be used to show which grids will be
# modified by the Script.


##ToolList = [("T_Tool", "T"),
##            ("PoP_Tool", "PoP"),
##            ("Wind_Tool", "Wind"),
##          ]

### If desired, Set up variables to be solicited from the user:
##  If your script calls Smart Tools, this VariableList should
##  cover all the variables necessary for the tools.

VariableList = [
         ("Model" , "NAM12", "radio", ["NAM12","NAM40", "GFS40"]), 
        ]
#VariableList.append(("Extrapolate:", "Forward in Time", "radio", ["Forward in Time", "Backward in Time"]))
#VariableList.append(("Movement Speed (Kts):", "15", "numeric"))
#VariableList.append(("This is just a label", "", "label"))
#VariableList.append(("5% Sky Cover threshold at RH percentage:", 60., "scale", [44., 74.],2.0))

import time
import AbsTime
import SmartScript
## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        self.loadWEGroup("FireWx")
        fwModel = varDict["Model"]
        self.saveObject("FireModel", fwModel, "ModelType")
      
        hour = int(time.strftime('%H', time.gmtime(time.time())))
        if hour > 3 and hour < 16:
            starttime = 12
            endtime = 61
        elif hour < 3:
            startime = 0
            endtime = 61
        else:
            starttime = 12
            endtime = 73
        
        # Clean up old data
        print 'Create_Fire_With_Smoke: Deleting old grids.'
        tr = self.createTimeRange(starttime, endtime, mode="Zulu")
        self.deleteCmd(['MaxRH', 'MinRH', 'Haines', 'MixHgt', 'TransWind', 'VentRate'],tr)
      
        if fwModel == "NAM12" or fwModel == "NAM40":
            print 'Create_Fire_With_Smoke: Creating new scratch grids.'
            self.createFromScratchCmd(['MaxRH', 'MinRH'], tr)
            self.createFromScratchCmd(['MixHgt'], tr, 3, 1)
            self.createFromScratchCmd(['TransWind'], tr, 3, 1)
            self.createFromScratchCmd(['VentRate'], tr, 3, 1)
            self.createFromScratchCmd(['MixHgt'], tr, 3, 1)
            self.createFromScratchCmd(['Haines'], tr, 3, 1)
#           self.createFromScratchCmd(['TransWindAve'], tr, 3, 1)
#            self.createFromScratchCmd(['VentRateAve'], tr, 3, 1)
#            self.createFromScratchCmd(['HainesAve'], tr, 3, 1)
#            self.createFromScratchCmd(['HrsOfSun'], tr, 12, 12)
            self.createFromScratchCmd(['LAL'], tr, 6, 6)
###            self.createFromScratchCmd(['RFD'], tr, 3, 1)
            
        else:
            print 'Create_Fire_With_Smoke: Creating new scratch grids.'
            self.createFromScratchCmd(['MaxRH', 'MinRH'], tr)
            self.createFromScratchCmd(['MixHgt'], tr, 6, 1)
            self.createFromScratchCmd(['TransWind'], tr, 6, 1)
            self.createFromScratchCmd(['VentRate'], tr, 6, 1)
#            self.createFromScratchCmd(['MixHgtAve'], tr, 6, 1)
#            self.createFromScratchCmd(['TransWindAve'], tr, 6, 1)
#            self.createFromScratchCmd(['VentRateAve'], tr, 6, 1)
            self.createFromScratchCmd(['Haines'], tr, 6, 1)
#            self.createFromScratchCmd(['HrsOfSun'], tr, 12, 12)
            self.createFromScratchCmd(['LAL'], tr, 6, 6)
###            self.createFromScratchCmd(['RFD'], tr, 3, 1)
            
        # QC the dewpoint, then populate the RH, then MinRH MaxRH
        print 'Create_Fire_With_Smoke: Running Td and RH tools.'
##        self.callSmartTool("Td_SmartTool", "Td", None, tr, missingDataMode="skip")
##        self.callSmartTool("RHTool","RH", None, tr, missingDataMode="create")
        self.callSmartTool("MaxRH_Tool","MaxRH", None, tr, missingDataMode="skip")
        self.callSmartTool("MinRH_Tool","MinRH", None, tr, missingDataMode="skip")
        
        
        # Populate the smoke parameters
        print 'Create_Fire_With_Smoke: Starting Haines.'
        self.callSmartTool("Haines", "Haines", None, tr, missingDataMode="skip")
        print 'Create_Fire_With_Smoke: Starting Mixing Hgt.'
        self.callSmartTool("MixHgt_FWF", "MixHgt", None, tr, missingDataMode="skip")
        print 'Create_Fire_With_Smoke: Starting Transport Winds.'
        self.callSmartTool("TransWind_NoVar", "TransWind", None, tr, missingDataMode="skip")
        #print 'Create_Fire_With_Smoke: Starting Vent Rate.'
        self.callSmartTool("VentRate", "VentRate", None, tr, missingDataMode="skip")
        print 'Create_Fire_With_Smoke: Starting LAL.'
        self.callSmartTool("LAL_Tool", "LAL", None, tr, missingDataMode="create")
#        self.callSmartTool("CalculateRFD", "RFD", None, tr, missingDataMode="create")

        # Interpolate the smoke parameters to hourly grids
        print 'Create_Fire_With_Smoke: Interpolating Grids.'
        self.interpolateCmd(["MixHgt", "VentRate", "TransWind", "Haines"], tr, "GAPS", "SYNC", interval = 1)

        # Populate the afternoon average smoke parameters for the ZFP
#        print 'Create_Fire_With_Smoke: Starting Average Mixing Hgt.'
#        self.callSmartTool("MixHgtAve", "MixHgtAve", None, tr, missingDataMode="skip")
#        print 'Create_Fire_With_Smoke: Starting Average Transport Winds.'
#        self.callSmartTool("TransWindAve", "TransWindAve", None, tr, missingDataMode="skip")
#        print 'Create_Fire_With_Smoke: Starting Average Vent Rate.'
#        self.callSmartTool("VentRateAve", "VentRateAve", None, tr, missingDataMode="skip")
        

        # Calculate the hours of sun
#        print 'Create_Fire_With_Smoke: Starting hours of sun.'
#    def execute(self, editArea, timeRange, varDict):
#        new_timeRange = self.createTimeRange(06, 78, mode="Zulu")
#        self.createFromScratchCmd(['HrsOfSun'], new_timeRange, 24, 24)
#        self.callSmartTool("Calc_Hours_of_Sun","HrsOfSun", None, tr, missingDataMode="skip")
##############
###   Added by Dergan for calculation of RFD (9/14/07)  ###
##############
##        dur = 10
##        startt = 14
##        timelength = dur + startt
##        rfd_tr = self.createTimeRange(startt, timelength, "Zulu")
##        self.createFromScratchCmd(['RFD'], rfd_tr, 1, 1)
##        self.callSmartTool("CalculateRFD", "RFD", None, rfd_tr, varDict)
##        self.createFromScratchCmd(['RFDmax'], rfd_tr, 0, timelength)
##        self.callSmartTool("RFDmax", "RFDmax", None, rfd_tr, varDict)
