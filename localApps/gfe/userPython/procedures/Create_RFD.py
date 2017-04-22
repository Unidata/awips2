# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Create_RFD.py
#
# Author: dtomalak
# Optimized by njensen
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]
import LogStream, time
from math import *

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
        ####CONFIGURABLE SECTION
        ####

        ### added to use 14Z if morning issuance, 18Z if update
        dur = 12
        startt = 12
        timelength = dur + startt
        timeRange = self.createTimeRange(startt, timelength, "Zulu")
        print "Delete timerange:", timeRange
        self.deleteCmd(['RFDmax'], timeRange)
        self.deleteCmd(['PERCENTGREEN'], timeRange)
        self.deleteCmd(['RFD'], timeRange)
        self.deleteCmd(['Curing'], timeRange)
        self.deleteCmd(['GFDI'], timeRange)
        
        curTime = time.gmtime()
        hour = curTime[3]
        if hour < 15:
            dur = 12 #HOW MANY HOURS OF DATA ARE NEEDED
            startt = 12 #WHEN TO START (UTC)
        else:
            dur = 6
            startt = 18
        ####end of added section    
        
        ####END CONFIGURATIONS!!!!!!!!!!!!!!
        ############################################################
        timelength = dur + startt
        timeRange = self.createTimeRange(startt,timelength ,"Zulu")
##        deltimerange = self.createTimeRange(27,startt+24 ,"Zulu")
##        RFDarea = self.getEditArea("Surrounding_Offices")
##        self.callSmartTool("RFD_isc_to_fcst", "T", RFDarea, timeRange)
##        self.callSmartTool("RFDisc_T", "T", None, timeRange)
##        self.callSmartTool("RFDisc_Td", "Td", None, timeRange)
##        self.callSmartTool("RFDisc_Wind", "Wind", None, timeRange)
##        self.callSmartTool("RFDisc_Sky", "Sky", None, timeRange)
##        self.callSmartTool("RFDisc_PoP", "PoP", None, timeRange)
##        self.callSmartTool("RHTool", "RH", None, timeRange)

############ &&&&&&&&&&&&&&&&&&& ######################       
        
        DB = self.findDatabase("ISC")
        self.copyCmd(['T', 'Td', 'Wind', 'Sky', 'PoP', 'RH'], DB, timeRange)
        self.createFromScratchCmd(['PERCENTGREEN'], timeRange)
        self.callSmartTool("PERCENTGREEN", "PERCENTGREEN", None, timeRange)
        self.createFromScratchCmd(['RFD'], timeRange, repeat=1, duration=1)
        self.callSmartTool("CalculateRFD", "RFD", None, timeRange)
        self.createFromScratchCmd(['Curing'], timeRange, repeat=1, duration=1)
        self.callSmartTool("Curing_from_Green", "Curing", None, timeRange)
        self.createFromScratchCmd(['GFDI'], timeRange, repeat=1, duration=1)
        self.callSmartTool("Calc_GFDI", "GFDI", None, timeRange)
##        self.callSmartTool("RFDmax", "RFDmax", None, timeRange)
        DB = self.findDatabase("ISC")
##        self.copyCmd(['T', 'Td', 'Wind', 'Sky', 'PoP'], DB, timeRange)
        self.createFromScratchCmd(['RFDmax'], timeRange)
        self.callSmartTool("RFDmax", "RFDmax", None, timeRange)
        DB = self.findDatabase("Official")
        self.copyCmd(['T', 'Td', 'Wind', 'Sky', 'PoP', 'RH'], DB, timeRange)
####Begin day 2 stuff for planning purposes
        dur = 12 #HOW MANY HOURS OF DATA ARE NEEDED
        startt = 12 #WHEN TO START (UTC)
        timelength = dur + startt
        timeRange = self.createTimeRange(startt+24,timelength+24,"Zulu")
##        deltimerange = self.createTimeRange(startt+24, timelength+24,"Zulu")
        DB = self.findDatabase("Official")
        self.createFromScratchCmd(['PERCENTGREEN'], timeRange)
        self.callSmartTool("PERCENTGREEN", "PERCENTGREEN", None, timeRange)
        self.createFromScratchCmd(['RFD'], timeRange, repeat=1, duration=1)
        self.callSmartTool("CalculateRFD", "RFD", None, timeRange)
        self.createFromScratchCmd(['Curing'], timeRange, repeat=1, duration=1)
        self.callSmartTool("Curing_from_Green", "Curing", None, timeRange)
        self.createFromScratchCmd(['GFDI'], timeRange, repeat=1, duration=1)
        self.callSmartTool("Calc_GFDI", "GFDI", None, timeRange)
        self.createFromScratchCmd(['RFDmax'], timeRange)
        self.callSmartTool("RFDmax", "RFDmax", None, timeRange)


