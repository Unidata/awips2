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
# Show_ISC_Info - Version 3.04 (Tim Barker - SOO Boise, ID)
#
#   creates a Tk window where it shows information on the ISC discrepancy
#   stats for each of the surrounding CWAs (the list of CWAs is controlled
#   by the config information in the ISC_Utility_Local).  For each CWA it
#   gives the average discrepancy, the average threshold, the number of
#   points considered (some border pairs ignored because of topography
#   differences, ocean/land difference, or failure to meet conditional
#   criteria like wind speeds above 12 kts, etc.), number of individual
#   points that failed, and a colored highlight for whether that border
#   passed or failed.
#
#   Optionally can have it check multiple parameters when run on certain
#   grids.  By default, when you run it on RH grids, it checks T and Td
#   grids too.
#
# 2006-01-23 - Barker - Version 3.04. Added thresholds for more parameters
# 2006-01-19 - Barker - Version 3.03. Fixed another problm in ISC_Utility
#              for non-square GFE domains.
# 2006-01-17 - Barker - Version 3.02. Fixed problem in ISC_Utility for
#              non-square GFE domains.
# 2006-01-13 - Barker - Version 3.01. Changed for new NDFD algorithm.
#              Thresholds now vary at each gridpoint - overall average
#              difference along border must be less than average threshold
#              along that border (a much better algorithm!). All
#              calculations done in ISC Utility routine. Text summary of
#              borders is displayed in format similar to old tool - but
#              different because of new agorithm.
# 2004-11-17 - Mathewson - baselined at FSL
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
#              Changed to show if the average discrepancy would violate
#              the 'by length' weighted average threshold (as NDFD does).
# 2004-09-05 - Version 2.1. Changes to ISC_Utility_Local for
#              handling areas where no neighbor exists.
# 2004-08-15 - Version 2.0 - Thresholds are not hard-coded and derived from
#              average elevation difference - like in NDFD (but still
#              WRONG (in my opinion).
# 2004-06-20 - version 1.1 - Various changes to vector checks to be somewhat
#              closer to NDFD checks - though NDFD is in a state of flux
# 2004-06-08 - version 1.0 - Added multi-parameter tests, and the
#              handling of vector parms
# 2004-02-11 - Cleaned up old tool used in BOI for quite some time.
# ----------------------------------------------------------------------------
#
#  C O N F I G U R A T I O N   S E C T I O N
#
# See ISC_Utility.
#
#  E N D   O F   C O N F I G U R A T I O N   S E C T I O N
#
#----------------------------------------------------------------------------
ToolType = "numeric"
WeatherElementEdited = "None"
ScreenList = ["SCALAR","VECTOR"]

import numpy
from Tkinter import *

import ISC_Utility_Local
import SmartScript
import time

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss = dbss
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessTool(self, WEname):
        self._utility = ISC_Utility_Local.ISC_Utility_Local(self._dbss, None)

    def preProcessGrid(self, WEname, GridTimeRange):
        #
        #  Run the check - and get the text listing
        #
        (numchecked,violate,warning)=self._utility._checkParmBorders(WEname,GridTimeRange,listing=1)
        outtext=self._utility._getListing()
        #
        #  Setup window to display results
        #
        self.chk=Tk()
        self.chk.title("ISC Discrepancy Info")
        self.frame=Frame(self.chk)
        self.button=Button(self.frame,text="Close",fg="red",
          command=self.chk.quit)
        self.button.pack(side=BOTTOM)
        self.frame.pack(side=BOTTOM,fill=X)
        self.scrollbar=Scrollbar(self.chk)
        self.scrollbar.pack(side=RIGHT,fill=Y)
        self.text=Text(self.chk,width=100,height=25,
          yscrollcommand=self.scrollbar.set)
        self.text.tag_config("extreme",foreground="purple4")
        self.text.tag_config("high",foreground="red")
        self.text.tag_config("medium",foreground="DarkOrange")
        self.text.tag_config("low",foreground="DarkGreen")
        self.text.tag_config("none",foreground="black")
        self.text.pack(fill=BOTH,expand=1)
        #
        #  Display lines - coloring the last word if it is recognized.
        #
        try:
            lines=outtext.split("\n")
            for line in lines:
               if line[-2:]=="OK":
                   newline=line[:-2]
                   self.text.insert(END,newline,"none")
                   self.text.insert(END,"OK\n","low")
               elif line[-7:]=="IGNORED":
                   newline=line[:-7]
                   self.text.insert(END,newline,"none")
                   self.text.insert(END,"IGNORED\n","medium")
               elif line[-6:]=="FAILED":
                   newline=line[:-6]
                   self.text.insert(END,newline,"none")
                   self.text.insert(END,"FAILED\n","high")
               else:
                   self.text.insert(END,line+"\n","none")
        except:
            self.chk.destroy()
            raise
        else:   
            self.text.configure(state=DISABLED)
            self.scrollbar.config(command=self.text.yview)
            self.chk.mainloop()
            try:
                self.chk.destroy()
            except:
                pass
            self.cancel()

    #======================================================================
    #
    #  dummy execute routine
    #
    def execute(self):
        "Show ISC discrepancies for each neighbor"
        return
    
