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
# RemoveWx - version 1.2 - Remove mention of any particular weather from any
#            combinations leaving everything else untouched.
#
# Author: Tim Barker - SOO Boise, ID
#    06/29/02 - Original Implementation
#    04/30/03 - version 1.1. Save weather Combine mode, force it to be
#               "Replace" while tool runs, then change it back to original
#               mode.  Otherwise - if mode is 'combine' when tool starts
#               nothing would happen.
#    03/20/05 - version 1.2. Remove weather codes that are no longer
#               available.  Better coding of string functions.
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "Wx"
ScreenList = ["Wx"]

from numpy import *
from WxMethods import *

VariableList = [
    ("Weather code to remove:","T (Thunder)","radio",["T (Thunder)",
                                            "R (Rain)",
                                            "RW (Rain Showers)",
                                            "S (Snow)",
                                            "SW (Snow Showers)",
                                            "L (Drizzle)",
                                            "ZR (Freezing Rain)",
                                            "ZL (Freezing Drizzle)",
                                            "IP (Sleet)",
                                            "F (Fog)",
                                            "ZF (Freezing Fog)",
                                            "IF (Ice Fog)",
                                            "IC (Ice Crystals)",
                                            "BS (Blowing Snow)",
                                            "BN (Blowing Sand)",
                                            "BD (Blowing Dust)",
                                            "K (Smoke)",
                                            "H (Haze)",
                                            "FR (Frost)",
                                            "ZY (Freezing Spray)",
                                            "WP (Waterspouts)",
                                            "VA (Volcanic Ash)"])
    ]

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessTool(self):
        self.savemode=self.combineMode()
        self.setCombineMode("Replace")
    def postProcessTool(self):
        if self.savemode==0:
           self.setCombineMode("Replace")
        else:
           self.setCombineMode("Combine")
        
    def execute(self, Wx, varDict):
        "Remove any mention of Specified Weather Code"
        wxValues,keys = Wx
        removeStr=varDict["Weather code to remove:"]
        remove=removeStr[0:2].strip()
#
#       make a table of all keys that need changing...and what they
#       will change to - using any current keys if we can.
#
        numkeys=len(keys)
        trans=zeros(numkeys)
        for i in range(numkeys):
            oldkey=keys[i]
            newkey=""
            codes=oldkey.split('^')
            for code in codes:
                (cover,type,inten,vis,attr)=code.split(":")
                if (type==remove):
                    pass
                else:
                    newkey+=code+"^"
            newkey=newkey[:-1]
            if len(newkey)<1:
                newkey="<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
            trans[i]=self.getIndex(newkey,keys)
                
        newwxValues=wxValues
        for i in range(numkeys):
           newwxValues[equal(wxValues,i)] = trans[i]
        return((newwxValues.astype('int8')),keys)

