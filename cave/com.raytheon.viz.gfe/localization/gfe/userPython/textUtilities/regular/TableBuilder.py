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
# TableBuilder.py
# Methods for Smart Table products.
#
# Author: hansen
# ----------------------------------------------------------------------------

import TextUtils
from WxMethods import *
import types
import TimeRange, AbsTime
from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteKey

class TableBuilder(TextUtils.TextUtils):
    def __init__(self):
        pass

    def columnLabels(self, statDict, argDict, weList):
        # Return the time labels and the column length for
        # the elements in weList

        # Could be made more general, but currently gets
        # grid time labels for each we listed
        hours = []
        maxLen = 0
        for we in weList:
            stats = statDict[we.name]
            #print stats
            for value, start in stats:
                hourStr = `start.hour`+"Z/"+`start.day`
                hours.append(hourStr)
                if len(hourStr) > maxLen:
                    maxLen = len(hourStr)

        colLen = maxLen + 4
        colLabels = ""
        for hour in hours:
            colLabels = colLabels + string.center(hour,colLen)
        colLabels = colLabels + "\n"
        return colLabels,colLen

    def columnValues(self, statDict, argDict, weList, label):
        # Return the column values as text strings
        values = []
        for we in weList:
            stats = statDict[we.name()]
            for value, hour in stats:
                values.append(fformat(value, we.roundVal()))
        return label, values

    def makeRow(self, rowLabel, colWidth, timeRangeList,
                statList, method, argList=None, rowLabelWidth=None,
                firstValWidth=None, justify = "r"):
        # Produce a row beginning with the label followed by a value
        #  for each period. Note that colWidth can be a number, for a
        #  fixed column width, or a list, for a variable column width. If
        #  a list, then it is a parallel list with the timeRangeList.
        #  If provided, firstValWidth overrides the colWidth (list or value)
        # Justify is "r" for right justify values, "l" for left justify values
        # Justify may also be a list, which then is specified for each 
        # column entry.
        # Each value is obtained via the given method which is given arguments:
        #   (statDict, timeRange, argList)
        # and must return a text string value (i.e. numerical values
        # must be converted to text strings before being returned).
        if type(colWidth) is types.ListType:
            fixedColWidth = colWidth[0]
        else:
            fixedColWidth = colWidth
        if rowLabelWidth is None:
            rowLabelWidth = fixedColWidth
        if firstValWidth is None:
            firstValWidth = fixedColWidth
        values = []
        index = 0
        for timeRange, label in timeRangeList:
            statDict = statList[index]
            if statDict is None or method is None:
                value = ""
            else:
                value = method(statDict, timeRange, argList)
            values.append(value)
            index = index + 1
        row = string.ljust(rowLabel, rowLabelWidth)
        for x in xrange(len(values)):
            if x == 0:
                width = firstValWidth
            elif type(colWidth) is types.ListType:
                width = colWidth[x]
            else:
                width = fixedColWidth
            if type(justify) is types.ListType:
                curJustify = justify[x]
            else:
                curJustify = justify
            if curJustify == 'r':
                row = row + string.rjust(string.strip(values[x]), width)
            else:
                row = row + string.ljust(string.strip(values[x]), width)
            width = fixedColWidth
        return row + "\n"

    def addColValue(self, fcst, str, width):
        return fcst + string.rjust(string.strip(str), width)

    def addRowLabel(self, fcst, str, width):
        return fcst + string.ljust(str, width)

    def maxVal(self, stats, timeRange, argList):
        # Return a scalar text string value representing the max value
        # The desired element name must be the first element of argList
        element = argList[0]
        value = self.getStats(stats, element)
        if value is None:
            return ""
        min, max = value
        return self.getScalarVal(max)
    
    def minVal(self, stats, timeRange, argList):
        # Return a scalar text string value representing the min value
        # The desired element name must be the first element of argList
        element = argList[0]
        value = self.getStats(stats, element)
        if value is None:
            return ""
        min, max = value
        return self.getScalarVal(min)

    def scalarVal(self, stats, timeRange, argList):
        # Return a scalar text string value
        # The desired element name must be the first element of argList
        element = argList[0]
        value = self.getStats(stats, element)
        if value is None:
            return ""
        return self.getScalarVal(value)
    
    def vectorVal(self, stats, timeRange, argList):
        # Return a vector text string value
        # The desired element name must be the first element of argList
        # E.g.  SW 19
        element = argList[0]
        value = self.getStats(stats, element)
        if value is None:
            return ""
        return self.getVectorVal(value)
                    
    def wxVal(self, stats, timeRange, argList):
        # Return a weather text string value
        # The desired element name must be the first element of argList
        # E.g.  SNOW
        element = argList[0]
        wxStats = self.getStats(stats, element)
        if wxStats is None:
            return ""
        value = ""
        #print "\nIn wxVal"
        for wxValue, timeRange in wxStats:
            #print wxValue, timeRange
            val = self.short_weather_phrase(
                element,wxValue)
            val = string.replace(val,"|"," ")
            val = string.replace(val,"THUNDER STORMS","THUNDERSTORMS")
            val = string.replace(val, "THUNDERSTORMS", "TSTMS")
            if self.wxOrder(val) < self.wxOrder(value) or value == "":
                value = val
            #print "value", value
        if value == "":
            value = "NONE"

        #print "Returning ", value
        return value
        
        
    def dayOrNightVal(self, statDict, timeRange, argList):
        # Return a min or max value based on the timeRange
        #   as Day or Night 
        # The argList contains the weather element for the
        #  daytime value followed by the element for the nighttime
        #  value
        # Try to report a trend as well
                    
        day = self.getPeriod(timeRange,1)        
        dayElement = argList[0]
        nightElement = argList[1]
        dayMinMax = argList[2]
        nightMinMax = argList[3]
        trendElement = argList[4]
        priorStatDict = argList[5]
        statList = argList[6]
        timeRangeList = argList[7]
        
        if day == self.DAYTIME():
            element = dayElement
            minMax = dayMinMax
        else:
            element = nightElement
            minMax = nightMinMax
        curVal = self.getStats(statDict, element)
        if curVal is not None:
            curVal = self.getValue(curVal, minMax)
            value = self.getScalarVal(curVal)
        else:
            return ""

        # Try to get trend
        if trendElement is None:
            return value
        
        # Get trend 1st or 2nd period
        index = 0
        for i in range(len(timeRangeList)):
            tr, label = timeRangeList[i]
            if timeRange == tr:
                index = i
                break
        if index >= 2:
            return value
        
        # Try the trend element first
        stats = self.getStats(statDict, trendElement)
        rawDiff = None
        if stats is not None:
            rawDiff = int(self.getValue(stats)) 
        else:
            # Get data from prior day
            # Since we want 24 hours prior AND we are only
            # looking at the first 2 periods for trends,
            # we always look at the priorStatDict
            val = None
            val = self.getStats(priorStatDict, element)
            if val is not None:
                rawDiff = int(curVal) - int(self.getValue(val, minMax))                
        if rawDiff is None:
            return value
        else:
            if rawDiff > 0:
                sign = "+"
            else:
                sign = ""                    
            return value + " (" + sign + `rawDiff` + ")"

####################################################################
#  Weather Codes
####################################################################
                  
    def getCode(self, stats, timeRange):
        # Return the weather code
        # Assumes analysis list:
        # "analysisList": [
        #               ("MinT", AnalysisMethods.avg),
        #               ("MaxT", AnalysisMethods.avg),
        #               ("PoP", AnalysisMethods.stdDevMaxAvg),
        #               ("Wx", AnalysisMethods.dominantWx),
        #               ("Sky", AnalysisMethods.avg),
        #               ("Wind", AnalysisMethods.vectorTextAvg)
        #              ],
        #
          
        # Get the statistics
        popMax = self.getStats(stats, "PoP__stdDevMaxAvg")
        maxT = self.getStats(stats,"MaxT__avg")
        wxKey = self.getStats(stats,"Wx__dominantWx")
        sky = self.getStats(stats,"Sky__avg")
        wind = self.getStats(stats,"Wind__vectorAvg")
        if wind is not None:
            windMag, windDir = wind
        else:
            windMag = None
        if wxKey is not None:
            wxSize = len(wxKey)
            wxStr = ""
            for x in range(wxSize):             
                wxStr += str(wxKey[x])
                if x < wxSize - 1:
                    wxStr += '^'
            wxKey = wxStr
        else:
            return "?"

        # the first if code statement satisfied is used
        # the order of the if statements prioritizes the code returned
        # eg. if fzrain code = Y is higher priority than snowrain code=O
        # then move block of code
        # code = self.getCode_Y(popMax, maxT, wxKey, sky, windMag)
        # if code is not None:
        #     return code
        ##ahead of
        # code = self.getCode_O(popMax, maxT, wxKey, sky, windMag)
        # if code is not None:
        #     return code


        code = self.getCode_P(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_T(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_O(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_R(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_S(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_W(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_J(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_L(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_X(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_Y(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_Z(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_M(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_Q(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_N(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_F(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_G(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_I(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_D(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_H(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_K(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        code = self.getCode_Sky(popMax, maxT, wxKey, sky, windMag, timeRange)
        if code is not None:
            return code
        code = self.getCode_A(popMax, maxT, wxKey, sky, windMag)
        if code is not None:
            return code
        return None
        

    def getCode_P(self, popMax, maxT, wxKey, sky, windMag):
        if windMag:
            # P -- BLZZRD
            # edit conditions for Blizzard below
            if (WxContains(wxKey, "Wide S + 1/4SM") or \
                WxContains(wxKey, "Wide S + 0SM"))   and \
                windMag > 35/1.15:    # 35 = wind speed in mph 
                    return "P"
        return None

    def getCode_T(self, popMax, maxT, wxKey, sky, windMag):
        #RW and T needed
        #Sct,Num,Wide,Chc,Lkly,Def needed

        if popMax is not None:
            # T -- TSTRMS
            # ->first block (commented out) requires RW and T Chc or 
            # greater be in wx to satisfy.  Second block requires 
            # only T Chc or greater to satisfy
            # both blocks require pop >= 45 to satisfy
#            if (WxContains(wxKey,"Lkly RW") or WxContains(wxKey,"Wide RW") or \
#                WxContains(wxKey,"Num RW") or WxContains(wxKey,"Sct RW") or \
#                WxContains(wxKey,"Ocnl RW") or \
#                WxContains(wxKey,"Chc RW") or WxContains(wxKey,"Def RW")) and \
#                (WxContains(wxKey,"Lkly T") or WxContains(wxKey,"Wide T") or \
#                WxContains(wxKey,"Num T") or WxContains(wxKey,"Sct T") or \
#                WxContains(wxKey,"Chc T") or WxContains(wxKey,"Def T")) and \
#                popMax >= 45:
            if (WxContains(wxKey,"Lkly T") or WxContains(wxKey,"Wide T") or \
                WxContains(wxKey,"Num T") or WxContains(wxKey,"Sct T") or \
                WxContains(wxKey,"Chc T") or WxContains(wxKey,"Def T") or \
                WxContains(wxKey,"Ocnl T")) and \
                popMax >= 45:


                   return "T"

        return None

    def getCode_O(self, popMax, maxT, wxKey, sky, windMag):
        if popMax is not None:
            # O -- RNSNOW
            if WxContains(wxKey, "* R") and WxContains(wxKey, "* S") \
               and popMax >= 45:
                   return "O"
        return None

    def getCode_R(self, popMax, maxT, wxKey, sky, windMag):
        if popMax is not None:
            # R -- RAIN
            if WxContains(wxKey, "* R") and popMax >= 45:
                   return "R"
        return None

    def getCode_S(self, popMax, maxT, wxKey, sky, windMag):
        if popMax is not None:
            # S -- SNOW
            if WxContains(wxKey, "* S") and popMax >= 45:
                   return "S"
        return None

    def getCode_W(self, popMax, maxT, wxKey, sky, windMag):
        if popMax is not None:
            # W -- SHWRS
            if WxContains(wxKey, "* RW") and popMax >= 45:
                   return "W"
        return None

    def getCode_J(self, popMax, maxT, wxKey, sky, windMag):
        if popMax is not None:
            # J -- SNOWSHWR
            if WxContains(wxKey, "* SW") and popMax >= 45:
                   return "J"
        return None

    def getCode_L(self, popMax, maxT, wxKey, sky, windMag):
        # L -- DRZL
        if WxContains(wxKey, "* L"):
               return "L"
        return None

    def getCode_X(self, popMax, maxT, wxKey, sky, windMag):
        if popMax is not None:
            # X -- SLEET
            if WxContains(wxKey, "* IP") and popMax >= 45:
                   return "X"
        return None

    def getCode_Y(self, popMax, maxT, wxKey, sky, windMag):
        if popMax is not None:
            # Y -- FZRAIN
            if WxContains(wxKey, "* ZR") and popMax >= 45:
                   return "Y"
        return None

    def getCode_Z(self, popMax, maxT, wxKey, sky, windMag):
        # Z -- FZDRZL
        if WxContains(wxKey, "* ZL"):
               return "Z"
        return None

    def getCode_M(self, popMax, maxT, wxKey, sky, windMag):
        # M -- FLURRIES
        # SW-- or S-- will return flurries as defined below
        # if you want S-- to be returned as light snow then use
        #if WxContains(wxKey, "* SW --"):   
        if WxContains(wxKey, "* SW --") or WxContains(wxKey, "* S --"):
               return "M"
        return None

    def getCode_Q(self, popMax, maxT, wxKey, sky, windMag):
        # Q -- BLGSNO
        if WxContains(wxKey, "* BS"):
               return "Q"
        return None

    def getCode_N(self, popMax, maxT, wxKey, sky, windMag):
        # edit windthresh for threshhold for getting N windy code returned
        windthresh = 25.0  # wind threshold in mph
        #print "wind ",windthresh,windMag,"\n"
        if windMag is not None:
            # N -- WINDY
            # the uncommented assumes all your wind speeds in gfe are in knts
            # if you have converted them to mph then use
            #if windMag > windthresh :       #
            if windMag > windthresh/1.15:    #mph from kts
                   return "N"
        return None

    def getCode_F(self, popMax, maxT, wxKey, sky, windMag):
        # F -- FOGGY
        # if wx contains F+ any coverage return F
        # you may want to change this to include F only if Areas of F 
        # are fcst any intensity
        # if WxContains(wxKey, "Areas F *"):
        if WxContains(wxKey, "* F +"):
               return "F"
        return None

    def getCode_G(self, popMax, maxT, wxKey, sky, windMag):
        if maxT is not None:
            # G -- VRYHOT  edit  your threshhold in degs F
            threshhold = 105
            if maxT > threshhold:
                   return "G"
        return None

    def getCode_I(self, popMax, maxT, wxKey, sky, windMag):
        if maxT is not None:
            # I -- VRYCOLD edit  your threshhold in degs F
            threshhold = 20
            if maxT < threshhold:
                   return "I"
        return None

    def getCode_D(self, popMax, maxT, wxKey, sky, windMag):
        # D -- DUST
        if WxContains(wxKey, "* BD"):
               return "D"
        return None

    def getCode_H(self, popMax, maxT, wxKey, sky, windMag):
        # H -- HAZE
        if WxContains(wxKey, "* H"):
               return "H"
        return None

    def getCode_K(self, popMax, maxT, wxKey, sky, windMag):
        # K -- SMOKE
        if WxContains(wxKey, "* K"):
               return "K"
        return None

    def getCode_Sky(self, popMax, maxT, wxKey, sky, windMag, timeRange):
        # C, E, B, U, V,  depending upon sky and time of day
        if sky is not None:
            # C -- CLDY
            if sky > 94:
                   return "C"

            # E -- MCLDY
            elif sky > 69:
                   return "E"

            # B -- PTCLDY
            elif sky > 31:
                   return "B"

            # U -- SUNNY
            else:
                localTimeRange = self.shiftedTimeRange(timeRange)
                dayNight = self.getPeriod(localTimeRange)
                if dayNight == self.DAYTIME():
                    return "U"
                else:
                    return "V"
        return None

    def getCode_A(self, popMax, maxT, wxKey, sky, windMag):
     # A -- FAIR
        return "A"

    def getScalarVal(self, value):
        # Return 4-digit right-justified text representation of value

        # Check for no data
        if value == () or value is None:
            return "    "
        else:
            # Convert to integer string
            return string.rjust(`int(value)`,4)

    def getVectorVal(self, value):
        # Return  text representation of vector value
        # Value is a tuple of magnitude and direction
        # E.g. returned value:  SW 19

        # Check for no data
        if value == () or value is None:
            return "    "
        else:
            mag = value[0]
            dir = value[1]
            magStr = string.rjust(`int(mag)`,3)
            if type(dir) is not types.StringType:
                dir = self.dirToText(dir)
            dirStr = string.rjust(dir,2)
            return dirStr + magStr

    def getWxVal(self, value):
        # Return text representation of value
        return self.wx_phrase({}, {}, value)

    def wxOrder(self, value):
        value = string.lower(value)
        if value == "thunderstorms":
            return 0
        elif value == "rain showers":
           return 1
        elif value == "rain":
            return 2
        elif value == "snow":
           return 3
        else:
            return 4

    def short_weather_phrase(self, element, stats):
        " Develop short phrase for weather in a table"
        # Weather Stats:
        #      SubKey List : list of all subkeys mentioned in time period

        if stats is None:
            return ""
        subkeyList = self.makeSubkeyList(stats)
        if len(subkeyList) == 0:
            return ""
        value = ""
        #print "In short_weather_phrase"
        for subKey in subkeyList:
            val, cov =  self.weather_value(None, None, subKey, typeOnly=1)
            #print "subKey", val
            if self.wxOrder(val) < self.wxOrder(value) or value == "":
                value = val
            #print "value", value
        value = string.replace(value, " ", "|")
        value = string.replace(value, "thunderstorm","thunder|storm")
        #print "returning", value
        return string.upper(value)

    def long_weather_phrase(self, element, stats): 
        # Stats from SampleAnalysis method: weather_percentages 
        words = ""
        index = 0
        prevCoverage = None
        conjunction = "|"
        if stats is None:
            return "None"
        length = len(stats)
        for subkey, percentage in stats:
            #print "subkey, percent", subkey, percentage
            if subkey is None or subkey.wxType() == "<NoWx>":
                index += 1
                continue

            # If not last one, determine nextCoverage
            if index < length-1:
                nextSubkey, percentage = stats[index+1]
            else:
                nextSubkey = None

            value, prevCoverage = self.weather_value(
                None, None, subkey, prevCoverage, nextSubkey)
            percentage = int(self.round(percentage,"Nearest", 1)) 
            words = words + value + " (" + `percentage` + "%) " 
            #prevSubkey = subkey 

            # if last one, do not add conjunction
            if index == length - 1: break
            words = words + conjunction
            index = index + 1        
            
        if words == "": 
            words = "None" 
        return words.upper() 

    def discrete_value(self, element, stats):
        " Return string of hazards"
        # Weather Stats:
        #      SubKey List : list of all subkeys mentioned in time period

        if stats is None:
            return ""
        subkeyList = self.makeSubkeyList(stats)
        if len(subkeyList) == 0:
            return ""

        from com.raytheon.uf.viz.core.localization import LocalizationManager
        siteId = LocalizationManager.getInstance().getSite()
        value = ""
        #print "In discrete_value"
        for subKey in subkeyList:
            str = subKey.split(":")[0]
            discreteWords = DiscreteKey.discreteDefinition(siteId).keyDesc(
                "Hazards" + "_SFC", str)
            value = value + discreteWords + " "
        #print "returning", value
        return value.upper()
    
    def long_discrete_phrase(self, element, stats): 
        # Stats from SampleAnalysis method: discrete_percentages 
        words = ""
        index = 0
        prevCoverage = None
        conjunction = "|"
        if stats is None:
            return "None"
        length = len(stats)
        from com.raytheon.uf.viz.core.localization import LocalizationManager
        siteId = LocalizationManager.getInstance().getSite()
        for subkey, percentage in stats:
            #print "subkey, percent", subkey, percentage
            if subkey is None or subkey == "<None>":
                index += 1
                continue

            percentage = int(self.round(percentage,"Nearest", 1))
            str = subkey.split(":")[0]
            discreteWords = DiscreteKey.discreteDefinition(siteId).keyDesc(
                "Hazards" + "_SFC", str)
            words = words + discreteWords + " (" + `percentage` + "%) " 

            # if last one, do not add conjunction
            if index == length - 1: break
            words = words + conjunction
            index = index + 1        
            
        if words == "": 
            words = "None" 
        return words.upper() 

    def cloudCover(self, element, stats):
        # Return a text cloud cover given Sky average value
        valStr = ""
        if stats is None:
            return valStr

        val = self.callMethod(stats, element.conversion())
        value = self.round(val, "Nearest", element.roundVal())
        if value < 20:
            shift = self.determineShift()
            period = element.getPeriod()
            tr = TimeRange.TimeRange(period.startTime() + shift, period.endTime() + shift)
            dayNight = self.getPeriod(tr)
            if dayNight == self.NIGHTTIME():
                valStr = "CLEAR"
            else:
                valStr = "SUNNY"
        elif value < 55:
            valStr = "PARTLY|CLOUDY"
        elif value < 85:
            valStr = "MOSTLY|CLOUDY"
        else:
            valStr = "CLOUDY"

        return valStr

    def wxPrecipSubkey(self, subkey):
        # List of wxTypes that should be counted as precipitation
        # for the calculation of Wx Duration
        if subkey.wxType() in  ["R", "RW", "S", "SW", "ZR", "IP", "SA"]:
            return 1
        else:
            return 0

    def wxCoveragePercent(self, coverage):
        percents = [
         ("Def", 100),
         ("Wide", 100),
         ("Ocnl", 80),
         ("Lkly", 75),
         ("Num", 60),
         ("Sct", 40),
         ("Chc", 20),
         ("SChc", 10),
         ("Iso", 10),
        ]
        for cov, percent in percents:
             if cov == coverage:
                 return percent
        return 0

    def wxDuration(self, statsByRange, timeRange):
        # Used in the FWM and FWFTable for weather duration
        # Weather duration is determined as follows:
        # Coverages are weighted according to the
        # values in wxCoveragePercent.  For example, "Wide"
        # gets 100% weighting while "Sct" on get 40%.
        # Only precip weather types (according to the wxPrecipSubkey)
        # are counted toward the duration.
        #
        # Create a total duration by adding a contribution from each grid:
        # For each grid in the time range:
        #   Among the precip subkeys, find the maximum Coverage percent.
        #   Weight it's contibution according to the amount of time
        #   it overlaps the given time range and the maximum coverage percent.
        #
        total = 0.0
        for subkeyList, subRange in statsByRange:
            subkeyList = self.makeSubkeyList(subkeyList)
            maxPercent = 0
            for subkey in subkeyList:
                if self.wxPrecipSubkey(subkey):
                    percent = self.wxCoveragePercent(subkey.coverage())
                    #print subkey.coverage(), percent
                    if percent > maxPercent:
                        maxPercent = percent
            subRange = timeRange.intersection(subRange)            
            value = maxPercent/100.0 * subRange.duration()/3600
            #print value
            total = total + value
        total = self.round(total, "Nearest", 1)
        #print "wxDur", total, timeRange
        return `int(total)`

    # Special interface to Multiple Element Table
    def makeMultipleElementTable(self, areaLabel, timeRange, argDict,
                                 byTimeRange=0, product="MultipleElementTable_Aux_Local"):
        # For each area in the areaLabel group of Combinations,
        #   For each city
        #     Generate a MultipleElementTable
        comboList = self.getCurrentAreaNames(argDict)
        exec "import " + self._cityDictionary
        exec "cityDict = " + self._cityDictionary + ".CityDictionary"
        table = ""

        if type(argDict) is not types.DictType:
            # "argDict" is really "tree"
            argDict = argDict.get("argDict")
        argDict["elementList"] = self._elementList
        argDict["singleValueFormat"] = self._singleValueFormat
        argDict["includeTitle"] = 1
        argDict["byTimeRange"] = byTimeRange
        
        for area in comboList:
            try:
                cities = cityDict[area]
            except:
                continue
            for city, cityLabel in cities:
                table = table + self.generateProduct(
                    product, argDict, city,
                    timeRange=timeRange, areaLabel=cityLabel)
                # ensure table has valid data 
                if table == "": 
                    continue 
                argDict["includeTitle"] = 0
            table = "\n" + table + "\n\n"
        return table

    def getMultipleElementTableRanges(self, productIssuance, singleValueFormat,
                                      timeRange=None):
        if productIssuance in [
            "Morning", "Morning Update", "Afternoon Update",
            "Morning with Pre-1st Period"]:
            self._productIssuance = "Morning"
            startHour = self.DAY()
            numPeriods = 3
        else: # "Afternoon", "Afternoon with Pre-1st Period",
              # "Evening Update", "Early Morning Update"
            self._productIssuance = "Afternoon"
            startHour = self.NIGHT()
            numPeriods = 4
        labelMethod = self.getLocalWeekday

        #print "\nMET Ranges", productIssuance, timeRange
        if timeRange is None:
            currentLocalTime, self._shift = self.determineTimeShift()
            day = currentLocalTime.day
            month = currentLocalTime.month
            year = currentLocalTime.year
                
            # Use getPeriods to set up a list of
            # time periods to be sampled
            # Returns a list of tuples: (timeRange, timeRangeLabel)

            # Convert to GMT time before making time range
            if productIssuance == "Early Morning Update":
                    self._shift = self._shift + 24*3600
                    labelMethod = self.getLocalWeekdayName
                    
            startTime = AbsTime.absTimeYMD(year,month,day,startHour)
            startTime = startTime - self._shift
            timeRange = TimeRange.TimeRange(startTime, startTime + 12*3600)
            
        if singleValueFormat == 1:
            numPeriods = 1            
        timeRangeList = self.getPeriods(timeRange, 12, 12, numPeriods,
                               labelMethod=labelMethod)
        
        # Adjust the first time range if an update issuance
        if productIssuance not in ["Morning", "Afternoon"]:
            updateTime = AbsTime.absTimeYMD(year, month, day, currentLocalTime.hour)
            updateTime = updateTime - self._shift
            tr, label = timeRangeList[0]
            updateTR = TimeRange.TimeRange(updateTime, tr.endTime())
            timeRangeList[0] = (updateTR, labelMethod(updateTR))
            
        #print "Returning", timeRangeList
        return timeRangeList
