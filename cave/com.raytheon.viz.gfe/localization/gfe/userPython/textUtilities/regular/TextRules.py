##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TextRules.py
# Methods for producing text forecast from Analysis statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

import ConfigurableIssuance
import Header
import SimpleTableUtils
import TableBuilder
import CombinedPhrases
import MarinePhrases
import FirePhrases
import CommonUtils

import string, types, time, sys, re
import math 
import ModuleAccessor 

class TextRules(ConfigurableIssuance.ConfigurableIssuance,
                Header.Header, TableBuilder.TableBuilder, SimpleTableUtils.SimpleTableUtils,
                CombinedPhrases.CombinedPhrases,
                MarinePhrases.MarinePhrases, FirePhrases.FirePhrases,
                CommonUtils.CommonUtils):
    def __init__(self):    
        ConfigurableIssuance.ConfigurableIssuance.__init__(self)
        Header.Header.__init__(self)
        SimpleTableUtils.SimpleTableUtils.__init__(self)
        TableBuilder.TableBuilder.__init__(self)
        CombinedPhrases.CombinedPhrases.__init__(self)
        MarinePhrases.MarinePhrases.__init__(self)
        FirePhrases.FirePhrases.__init__(self)
        CommonUtils.CommonUtils.__init__(self)
        
    ############################################      
    ### GLOBAL THRESHOLDS AND VARIABLES
    ### To override, override the associated method in your text product class.

    def IFP(self):
        return 0#AFPS

    def getSiteID(self, argDict):
        ifpClient = argDict["ifpClient"]
        return str(ifpClient.getSiteID().get(0))

    def getGFESuiteVersion(self):
        return 0#AFPS.DBSubsystem.getBuildVersion()[9:]

    def fillSpecial(self, fcst, argDict):
        # Substitute appropriate strings for special variables 
        fcstDef = argDict["forecastDef"]
        ut = argDict["utility"]
        trMethod = ut.set(fcstDef, "timePeriodMethod", self.timeRangeLabel)
        if type(trMethod) == types.StringType:
            exec "trMethod = self."+trMethod
        labelFormat = ut.set(fcstDef,"timePeriodFormat", None)
        tr = argDict["timeRange"]
        if labelFormat is not None:
            LTorZulu, durFmt, startFmt, endFmt = labelFormat
            timeperiod = self.timeDisplay(
                tr, LTorZulu, durFmt, startFmt, endFmt)
        else:
            timeperiod = trMethod(tr)

        try:
            trName = argDict["timeRangeName"]
        except:
            trName = ""
        try:
            eaName, eaLabel = argDict["editArea"]
        except:
            eaName, eaLabel = "",""
        try:
            elementName = argDict["element"]
        except:
            elementName = ""
        fcst = string.replace(fcst, "%TimePeriod",timeperiod)
        fcst = string.replace(fcst, "%EditArea", eaLabel)
        fcst = string.replace(fcst, "%WeatherElement", elementName)
        fcst = string.replace(fcst, "%TimeRange", trName)
        return fcst


