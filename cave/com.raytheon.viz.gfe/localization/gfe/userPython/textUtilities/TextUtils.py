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
# TextUtils.py
# Utility methods for Text Products.
#
# Author: hansen
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

from math import *
import types
import os, re, time
import WxMethods
import SiteInfo
from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID

class  TextUtils:
    def __init__(self):
        self.__percentCompleted = 0

    def DAY(self):
        return 6
    def NIGHT(self):
        return 18

    def DAYTIME(self):
        return 1
    def NIGHTTIME(self):
        return 0
    def DAYNIGHT(self):
        return -1

    def dirList(self):
        dirSpan = 45 # 45 degrees per direction
        base = 22.5 # start with N
        return [
            ('N', 360-base,          361),
            ('N', 0,                 base),
            ('NE',base          ,    base + 1*dirSpan),
            ('E', base + 1*dirSpan,  base + 2*dirSpan),
            ('SE',base + 2*dirSpan,  base + 3*dirSpan),
            ('S', base + 3*dirSpan,  base + 4*dirSpan),
            ('SW',base + 4*dirSpan,  base + 5*dirSpan),
            ('W', base + 5*dirSpan,  base + 6*dirSpan),
            ('NW',base + 6*dirSpan,  base + 7*dirSpan)
            ]

    def dir16PtList(self):
        dirSpan = 22.5 # 22.5 degrees per direction
        base = 11.25 # start with N
        return [
            ('N',   360-base,          361),
            ('N',   0,                 base),
            ('NNE', base            ,  base + 1*dirSpan),
            ('NE',  base + 1*dirSpan,  base + 2*dirSpan),
            ('ENE', base + 2*dirSpan,  base + 3*dirSpan),
            ('E',   base + 3*dirSpan,  base + 4*dirSpan),
            ('ESE', base + 4*dirSpan,  base + 5*dirSpan),
            ('SE',  base + 5*dirSpan,  base + 6*dirSpan),
            ('SSE', base + 6*dirSpan,  base + 7*dirSpan),
            ('S',   base + 7*dirSpan,  base + 8*dirSpan),
            ('SSW', base + 8*dirSpan,  base + 9*dirSpan),
            ('SW',  base + 9*dirSpan,  base + 10*dirSpan),
            ('WSW', base + 10*dirSpan,  base + 11*dirSpan),
            ('W',   base + 11*dirSpan,  base + 12*dirSpan),
            ('WNW', base + 12*dirSpan,  base + 13*dirSpan),
            ('NW',  base + 13*dirSpan,  base + 14*dirSpan),
            ('NNW', base + 14*dirSpan,  base + 15*dirSpan),
            ]

    # Dictionary for converting Wind Direction from letters to degrees
    def dirList2(self):
        return {
            'N' : 0,
            'NE':45,
            'E' :90,
            'SE':135,
            'S' :180,
            'SW':225,
            'W' :270,
            'NW':315,
            }

    def dirToText(self, numDir):
        "Convert the numerical direction to a string: N, NE, E, ..."
        for dirInfo in self.dirList():
            if numDir >= dirInfo[1] and numDir < dirInfo[2]:
                return dirInfo[0]
        print("WARNING -- illegal direction for conversion: ", numDir)
        return None

    def dirTo16PtText(self, numDir):
        "Convert the numerical direction to a string: N, NE, E, ..."
        for dirInfo in self.dir16PtList():
            if numDir >= dirInfo[1] and numDir < dirInfo[2]:
                return dirInfo[0]
        print("WARNING -- illegal direction for conversion: ", numDir)
        return None

    def vector_dir(self, direction):
        if not isinstance(direction, str):
            direction = self.dirToText(direction)
        direction = direction.replace("N", "north")
        direction = direction.replace("S", "south")
        direction = direction.replace("E", "east")
        direction = direction.replace("W", "west")
        return direction


    def getVis(self, subkeyList, outputFormat="NM"):
        # Find the "lowest" visibility specified in the subkeys
        conversionDict = self.visibilityConversionDict()
        resultVisNM = None
        resultVis = "<NoVis>"
        for subkey in subkeyList:
            vis = subkey.visibility()
            if vis == "<NoVis>":
                continue
            if resultVisNM is None:
                resultVisNM = conversionDict[vis]
                resultVis = vis
            else:
                # Find lowest visibility
                visNM = conversionDict[vis]
                if visNM < resultVisNM:
                    resultVisNM = visNM
                    resultVis = vis
        if outputFormat == "NM":
            return resultVisNM
        else:
            return resultVis

    def visibilityConversionDict(self):
        # Conversion from text to nautical miles
        return {
            "0SM": 0,
            "1/4SM": 0.2174,
            "1/2SM": 0.4348,
            "3/4SM": 0.6522,
            "1SM": 0.8696,
            "11/2SM": 1.304,
            "2SM": 1.739,
            "21/2SM": 2.174,
            "3SM": 2.609,
            "4SM": 3.478,
            "5SM": 4.348,
            "6SM": 5.217,
            "P6SM": 6.087,
            }

    def findSubkeys(self, subkeys, searchKeys):
        # Return 1 if any of the searchKeys are found in subkeys
        wxSize = len(subkeys)
        wxStr = ""
        for x in range(wxSize):
            wxStr += str(subkeys[x])
            if x < wxSize - 1:
                wxStr += '^'
        wx = wxStr
        for searchKey in searchKeys:
            if WxMethods.WxContains(wx, searchKey):
                return 1
        return 0

    ########################################################################
    # Methods for accessing customizable dictionaries and tables

    # Dictionary access
    def access_dictionary(self, tree, node, key, value, dictName, execMethods=1):
        # Access the dictionary with the given name for the given key value
        # The value for a key may be :
        #    a text string
        #    a method
        #    a dictionary.  The dictionary may be of several forms:
        #       a non-linear value dictionary (nlValue). This
        #         dictionary has entries that are tuples with values
        #         or the keyword "default" with a value. (See the
        #         Text Product User Guide section on Non-linear Thresholds).
        #       a dictionary by weather element.  In this case,
        #         the dictionary can have the optional entry "otherwise"
        #         to be used if the given element has not entry.
        #
        # If a method, it will be called with arguments:
        #    tree, node
        # If there is no entry found, an empty string will be returned
        #
        dictionary = getattr(self, dictName)(tree, node)
        #print dictionary
        if key in dictionary:
            entry = dictionary[key]
            #print type(entry), entry
            if execMethods and type(entry) is types.MethodType:
                return entry(tree, node, key, value)
            # For some reason, if a method is assigned within
            # the Local class, it appears as a tuple instead of a
            # method
            if execMethods and type(entry) is tuple:
                try:
                    return entry[0](tree, node, key, value)
                except:
                    # In case it's really a tuple
                    return entry
            elif type(entry) is dict:
                # Check for nlValue dictionary
                for key in entry:
                    if key == "default" or type(key) is tuple:
                        return entry
                # Otherwise, look for value in dictionary
                try:
                    return entry[value]
                except:
                    # See if there is an "otherwise" entry
                    try:
                        return entry["otherwise"]
                    except:
                        return ""
            else:
                return entry
        else:
            if "otherwise" in dictionary:
                return dictionary["otherwise"]
        return ""

    def calcTopoPercentage(self, tree, node, areaLabel, value):
        # Calculate the percentage of topo points in the current edit area that are above
        # the given value
        parmHisto = tree.getTopoHisto(areaLabel)
        totalPoints = parmHisto.numberOfGridPoints()
        if totalPoints == 0:
            return 0.0
        countAbove = 0

        for histSample in parmHisto.histoSamples():
            for histPair in histSample.histogram():
                if histPair.value().scalar() > value:
                    countAbove = countAbove + histPair.count()
        return float(countAbove)/totalPoints * 100.0

    def callMethod(self, value, method):
        "Call the given method with the value"

        if method is not None:
            value = method(value)
        return value

    def fformat(self, value, roundVal):
        # Return a string for the floating point value
        # truncated to the resolution given by roundVal
        if roundVal > 1.0:
            return repr(int(value))
        else:
            # Determine how many decimal points
            #  e.g. if roundVal is .01, dec will be 2
            val = roundVal
            dec = 0
            while val < 1:
                val = val * 10
                dec = dec + 1
            dec = repr(dec)

        numFormat = "%10."+dec+"f"
        value = numFormat %value
        value = value.strip()
        return value

    def convertDirection(self, numDir):
        "Convert the numerical direction to a string: N, NE, E, ..."
        for dirInfo in self.dirList():
            if numDir >= dirInfo[1] and numDir < dirInfo[2]:
                return dirInfo[0]
        return "N" # this line for pychecker

    def direction_movement(self, dir1, dir2):
        # Returns -1, 0, or 1 if the change from dir1 to dir2 is
        # counterclockwise, no change, or clockwise, respectively.
        # Note differences of 180 degrees can return -1 or 1.
        dirList2 = self.dirList2()
        if type(dir1) is str:
            dir1 = dirList2[dir1]
        if type(dir2) is str:
            dir2 = dirList2[dir2]
        diff = dir2 - dir1
        absDiff = abs(diff)
        if diff == 0:
            return 0
        elif absDiff <= 180:
            return diff / absDiff
        else:
            return -diff / absDiff

    def direction_difference(self, dir1, dir2):
        # Returns the difference dir2 - dir2.  Values <0 or more than
        # 180 are normalized so that this function always return values
        # between 0 and 180.
        dirList2 = self.dirList2()
        if type(dir1) is str:
            dir1 = dirList2[dir1]
        if type(dir2) is str:
            dir2 = dirList2[dir2]
        diff = dir2 - dir1
        absDiff = abs(diff)
        if absDiff <= 180:
            return absDiff
        else:
            return abs(absDiff - 360)

    def direction_between(self, testdir, dir1, dir2):
        # Returns 1 if dir is between dir1 and dir2, 0 otherwise
        # Note if dir1 - dir2 == 180 this function always returns 1
        if isinstance(testdir, str):
            testdir = self.dirList2()[testdir]
        totalDiff = self.direction_difference(dir1, dir2)
        diff1 = self.direction_difference(testdir, dir1)
        diff2 = self.direction_difference(testdir, dir2)
        # if dir is inbetween the sum of the differences will be the same
        if abs(diff1 + diff2 - totalDiff) < 0.1:
            return 1
        else:
            return 0

    def handleError(self, errorMsg, argDict):
        ut = argDict["utility"]
        ut.handleError(errorMsg)
        #tkMessageBox.showwarning("Warning", errorMsg)

    def round(self, val, mode, increment):
        value = float(val)
        if type(increment) is str:
            return value
        if not (mode == "RoundUp" or mode == "RoundDown" or mode == "Nearest"):
            print(mode, "is an invalid mode.")
            return value
        
        # check for the case where no work is needed.
        if value % increment == 0:
            return value

        sign = abs(value) / value
        delta = 0
        if mode == "RoundUp" and sign > 0:
            delta = sign * increment
        elif mode == "RoundDown" and sign < 0:
            delta = sign * increment

        if mode == "RoundUp":
            value = (int(value / increment) * increment) + delta
        elif mode == "RoundDown":
            value = (int(value / increment) * increment) + delta
        elif mode == "Nearest":
            value = int((value + (sign * increment / 2.0)) / increment) * increment
        return float(value)

    def average(self,v1,v2):
        return (v1+v2)/2.0

    def vectorAverage(self, v1, v2):
        # v1, v2 are (mag,dir) tuples
        uw1, vw1 = self.MagDirToUV(v1[0], v1[1])
        uw2, vw2 = self.MagDirToUV(v2[0], v2[1])
        u = (uw1 + uw2) / 2.0
        v = (vw1 + vw2) / 2.0
        return self.UVToMagDir(u, v)

    def MagDirToUV(self, mag, direction):
        #Converts magnitude, direction to u, v
        DEG_TO_RAD = 0.017453292
        uw = sin(direction * DEG_TO_RAD) * mag
        vw = cos(direction * DEG_TO_RAD) * mag
        return (uw, vw)

    def UVToMagDir(self, u, v):
        # Converts u, v to magnitude, direction
        RAD_TO_DEG = 57.296083
        speed = sqrt(u * u + v * v)
        direction = atan2(u, v) * RAD_TO_DEG
        while direction < 0.0:
            direction += 360.0
        while direction >= 360.0:
            direction -= 360.0
        #print "Speed, dir ", speed, direction
        return (speed, direction)

    def setProgressPercentage(self, percentage):
        self.__percentCompleted = percentage

    def progressMessage(self, fraction, percent, message):
        percent = int(fraction * percent)
        self.__percentCompleted = int(self.__percentCompleted + percent)
        print("Progress: " + repr(self.__percentCompleted) + "% " + message)

    def getParmID(self, parmNameAndLevel, databaseID):
        index = parmNameAndLevel.find("_")
        if index == -1:
            name = parmNameAndLevel
            level = "SFC"
            parm = ParmID(name,databaseID,level)
        else:
            name = parmNameAndLevel[0:index]
            level = parmNameAndLevel[index+1:]
            parm = ParmID(name,databaseID,level)
        return parm

    def nlValue(self, nlValue, lookupValue):
        # Apply a non-linear value to the given value
        # nlValue might be a dictionary to be applied to value
        # OR it could be a simple constant
        if isinstance(nlValue, dict):
            for (key, value) in nlValue.items():
                if not isinstance(key, str):
                    if lookupValue >= key[0] and lookupValue < key[1]:
                        return value
            
            if 'default' in nlValue:
                return nlValue['default']

            msgString = """ILLEGAL NON-LINEAR THRESHOLD dictionary.
            No dictionary entry for value: """ + repr(lookupValue) + """
            Make sure your non-linear threshold dictionaries do not
            have "gaps" in the ranges. For example, your dictionary
            should look like this:

               def maximum_range_nlValue_dict(self, tree, node):
                    ### ConfigVariables
                    # Maximum range to be reported within a phrase
                    #   e.g. 5 to 10 mph
                    # Units depend on the product
                    dict = TextRules.TextRules.maximum_range_nlValue_dict(self,tree, node)
                    dict["Wind"] = {
                        (0, 5) : 0,
                        (5, 13) : 5,
                        (13, 28) : 10,
                        "default" : 15,
                        }
                    return dict

            NOT this:

               def maximum_range_nlValue_dict(self, tree, node):
                    ### ConfigVariables
                    # Maximum range to be reported within a phrase
                    #   e.g. 5 to 10 mph
                    # Units depend on the product
                    dict = TextRules.TextRules.maximum_range_nlValue_dict(self,tree, node)
                    dict["Wind"] = {
                        (0, 4) : 0,
                        (5, 12) : 5,
                        (13, 27) : 10,
                        "default" : 15,
                        }
                    return dict
                """
            raise ValueError(msgString)
        elif type(nlValue) is types.MethodType:
            return nlValue(lookupValue)
        else:
            # Constant value
            return nlValue

    def roundValue(self, value, roundingMethod, mode, increment_nlValue, maxFlag=0):
        nlIncrement = self.nlValue(increment_nlValue, value)
        if type(roundingMethod) is types.MethodType:
            return  roundingMethod(value, mode, nlIncrement, maxFlag)
        else:
            return self.round(value, mode, nlIncrement)


    def getRangeInfo(self, tree, node, elementName):
        rangeThreshold_nlValue = self.range_nlValue(tree, node, elementName, elementName)
        rangeBias_nlValue = self.range_bias_nlValue(tree, node, elementName, elementName)
        minRange_nlValue = self.minimum_range_nlValue(tree, node, elementName, elementName)
        minBias_nlValue = self.minimum_range_bias_nlValue(tree, node, elementName, elementName)
        maxRange_nlValue = self.maximum_range_nlValue(tree, node, elementName, elementName)
        maxBias_nlValue = self.maximum_range_bias_nlValue(tree, node, elementName, elementName)
        increment_nlValue = self.increment_nlValue(tree, node, elementName, elementName)
        null_nlValue = self.null_nlValue(tree, node, elementName, elementName)
        return self.RangeInfo(rangeThreshold_nlValue, rangeBias_nlValue, minRange_nlValue, minBias_nlValue,
                         maxRange_nlValue, maxBias_nlValue, increment_nlValue, null_nlValue)

    def applyRanges(self, tree, node, minVal, maxVal, elementName):
        rangeInfo = self.getRangeInfo(tree, node, elementName)
        return self.applyRangeValues(tree, node, minVal, maxVal, elementName, rangeInfo)

    def applyRangeValues(self, tree, node, minVal, maxVal, elementName, rangeInfo):
        avg = self.average(float(minVal), maxVal)
        diff = abs(maxVal - minVal)
        # If the range is not great enough, return as a single value
        if rangeInfo.rangeThreshold_nlValue != "":
            threshold = self.nlValue(rangeInfo.rangeThreshold_nlValue, avg)
            if diff < threshold:
                bias = self.nlValue(rangeInfo.rangeBias_nlValue, avg)
                if bias == "Average":
                    avg = self.roundStatistic(tree, node, avg, elementName)
                    return avg, avg
                elif bias == "Max":
                    return maxVal, maxVal
                else:
                    return minVal, minVal
        # Apply minimum range
        if rangeInfo.minRange_nlValue != "":
            minRange = self.nlValue(rangeInfo.minRange_nlValue, avg)
            if diff < minRange:
                minVal, maxVal =  self.applyBias(
                    tree, node, elementName, minVal, maxVal,
                    rangeInfo.minBias_nlValue, avg, minRange,
                    rangeInfo.increment_nlValue)
        # Apply maximum range
        if rangeInfo.maxRange_nlValue != "":
            maxRange = self.nlValue(rangeInfo.maxRange_nlValue, avg)
            if diff > maxRange:
                minVal, maxVal = self.applyBias(
                    tree, node, elementName, minVal, maxVal,
                    rangeInfo.maxBias_nlValue, avg, maxRange,
                    rangeInfo.increment_nlValue)
        # Cut-off at null_nlValue if max > null_nlValue and min < null_nlValue
        threshold = self.nlValue(rangeInfo.null_nlValue, maxVal)
        if minVal > 0 and maxVal >= threshold and minVal < threshold:
            #print "cut-off", minVal, maxVal, threshold, elementName
            roundingMethod = self.rounding_method(tree, node, elementName, elementName)
            nlIncrement = self.nlValue(self.increment_nlValue(
                tree, node, elementName, elementName), threshold)
            minVal = self.roundValue(threshold, roundingMethod, "RoundUp", nlIncrement)
            if minVal < threshold:
                minVal = threshold
            #print "   new min", minVal
        return minVal, maxVal

    def applyBias(self, tree, node, elementName, minVal, maxVal, bias_nlValue,
                  avg, rangeValue, increment_nlValue):
        bias = self.nlValue(bias_nlValue, avg)
        #print "applying bias", minVal, maxVal, elementName, bias, rangeValue
        if bias == "Average":
            inc = rangeValue/2.0
            minVal = self.roundStatistic(tree, node, avg - inc, elementName)
            maxVal = self.roundStatistic(tree, node, avg + inc, elementName)
            ## If ranges are being applied to values that
            ## span zero, you can end up with a max-min being greater than
            ## rangeValue because Python rounds away from zero. (-0.5 rounds to
            ## -1.0, not 0.0.) The test below checks for this and adds
            ## 1 back to the min.
            if maxVal-minVal > rangeValue:
                minVal += 1
        else:
            increment = self.nlValue(increment_nlValue, avg)
            rangeValue = self.round(rangeValue, "Nearest", increment)
            minAllowedValue, maxAllowedValue = tree.library.getLimits(elementName)
            if bias == "Max":
                minVal = maxVal - rangeValue
            else:
                maxVal = minVal + rangeValue
            if maxVal > maxAllowedValue:
                maxVal = maxAllowedValue
            if minVal < minAllowedValue:
                minVal = minAllowedValue
        #print "     returning new", minVal, maxVal
        return minVal, maxVal

    #######################################################
    # Statistics manipulation

    def SCALAR(self):
        return 0
    def MAGNITUDE(self):
        return 1
    def DIRECTION(self):
        return 2
    def VECTOR(self):
        return 3
    def VECTOR_TEXT(self):
        return 4
    def VECTOR_NUM(self):
        return 5
    def WEATHER(self):
        return 6
    def DISCRETE(self):
        return 7

    def getValue(self, stats, method="Average", dataType=None):
        # "stats" is either a single value or a tuple of 2 values
        # method is any mergeMethod

        if dataType == self.VECTOR():
            mag, direction = stats
            mag = self.getValue(mag, method)
            return (mag, direction)

        if isinstance(stats, tuple):
            if method == "Max":
                return stats[1]
            elif method == "Min":
                return stats[0]
            elif method == "Sum":
                return stats[0] + stats[1]
            elif method == "MinMax":
                return stats
            else:
                if stats[0] is None or stats[1] is None:
                    return None
                return self.average(stats[0], stats[1])
        else:
            if method == "MinMax":
                return (stats, stats)
            else:
                return stats

    def makeSubkeyList(self, weatherKey):
        # Make sure subkeyList is a true list
        length = len(weatherKey)
        newList = []
        index = 0
        for subkey in weatherKey:
            newList.append(subkey)
            index = index + 1
            if index >= length:
                break
        return newList

    def storeAWIPS(self, product, AWIPSkey="", host=None):
        # Stores text in string "product" into
        # the AWIPS text database via the given host if host is defined using
        # ssh technique. Otherwise uses the AWIPS textdb command directly.
        # Note: for the ssh mode, you need to have an entry in the .rhosts
        # file of your home directory on lx1
        #

        if AWIPSkey == "":
            return    # do nothing

        # use the command directly - assumes FXA environment setup
        if host is None:
            # (code adopted from Paul Jendrowski 9/18/03)
            # set path to textdb command
            cmd = "gfetextdb -w " + AWIPSkey
            # issue the command
            db = os.popen(cmd, 'w')
            db.write(product)
            db.close()

        # use ssh (or rsh) to communicate with the textdb
        else:
            try:
                command= "ssh " + host + " 'textdb -w " + AWIPSkey +"'"
            except:
                command= "rsh " + host + " 'textdb -w " + AWIPSkey +"'"
            saveProduct = os.popen(command,'w')
            saveProduct.write(product)
            saveProduct.close()

    def getPreviousProduct(self, productID, searchString="", version=0):
        # gets a previous product from the AWIPS database

        from com.raytheon.viz.gfe.core import DataManagerUIFactory
        from com.raytheon.viz.gfe.product import TextDBUtil

        # Redmine #17120 - return to pre-DR 15703 behavior.
        opMode = DataManagerUIFactory.getCurrentInstance().getOpMode().name() == "OPERATIONAL"       
        previousProduct = TextDBUtil.retrieveProduct(productID, version, opMode)
        previousProduct = previousProduct.strip()

        if searchString != "":
            # extract the specified section
            section = re.sub(r'^[=, A-Za-z0-9\-\n\./]+' +
                             searchString + r'[=, A-Za-z0-9\-\n\/]*\.\.\.\n' +
                             r'*([=, A-Za-z0-9\-\n\./]+)\$\$[=, A-Za-z0-9\-\n'
                             + r'\.\$@/]+', r'\1', previousProduct)
            return section
        else:
            return previousProduct

    def formatTimeString(self, gmTime, dateFormat, newTimeZone=None):
        # converts the specified time (in seconds) to the specified time zone
        # the time returned is in local time in units of seconds.
        # newTimeZone must be an accepted time zone identifier such as
        # "EST5EDT", CST6CDT", "MST7MDT", "PST8PST", "AST9ADT" "HST10"
        myTimeZone = os.environ["TZ"]  # save the defined time zone
        if newTimeZone is None:
            newTimeZone = myTimeZone
        os.environ["TZ"] = newTimeZone  # set the new time zone
        time.tzset()
        timeZoneStr = time.strftime(dateFormat, time.localtime(gmTime))
        os.environ["TZ"] = myTimeZone # set the time zone back
        time.tzset()
        return timeZoneStr  # return the time as a string

    # Adopted from ER  8/04
    def debug_print(self, msg="", trace=0, limit=10):
        """ZFP_ER_Overrides addition of ZFP_ER_Overrides._debug_print.

        ER method for generic debug prints switched on/off by self._debug.
        Automatically prints calling method's name, file, class and line
        number plus an optional message string.

        (e.g. self.debug_print('Debug message')

        If the 'trace' flag is set to 1, the Python traceback info will not be
        displayed.  This is useful for displaying multiple formatted DEBUG
        messages.

        (e.g. self.debug_print('2nd Debug message', 1)
        """

        # This method requires: import traceback
        import traceback

        # If debug is set up as a dictionary, then you could turn on
        # method specific printing.
        # Definition["debug"] = {"_myMeth1":1, "_myMeth2":0}

        #  Try to get traceback info - if debug flag is defined and not off
        try:
            if self._debug:

                #  Get debug info
                file, lineno, name, text = traceback.extract_stack()[-2]
            else:
                return          #  bail now - turned off
        except:
            return              #  bail now - not defined

        #  Define counter to track number of times this method has been printed
        count = 0

        #  See if debug counter dictionary has been defined
        try:
            if type(self._debugDict) == type({}):
                pass
        except:
            self._debugDict = {}

        #  See if method-specific printing is being used
        if type(self._debug) == type({}):

            #  If this method has a method specific flag - get it
            if name in self._debug:
                flag = self._debug[name]
            else:
                flag = 0            #  not specified - don't display
        else:
            flag = self._debug

        #  If debug flag is turned off
        if not flag:
            return                  #  bail out now

        #  Track number of times this method has been displayed
        if name in self._debugDict:
            count = self._debugDict[name] + 1
        else:
            count = 1

        #  If debug message limit is not reached
        if count <= limit:

            #  If this is not a continuation debug message
            if trace == 0:

                #  Record the printing of this message
                self._debugDict[name] = count

                #  Print the traceback message
                print("DEBUG: %s in %s at line %d" % (name, file, lineno))
                print("DEBUG: Class = %s %d\n\n" % (self.__class__, count))
                #print "Super classes:",self.__class__.__bases__

            #  If there is a message, print that too
            if msg != "":
                print('\t%s' % (msg))

    class RangeInfo:
        def __init__(self,rangeThreshold_nlValue, rangeBias_nlValue,
                     minRange_nlValue, minBias_nlValue,
                     maxRange_nlValue, maxBias_nlValue, increment_nlValue,
                     null_nlValue):
            self.rangeThreshold_nlValue = rangeThreshold_nlValue
            self.rangeBias_nlValue = rangeBias_nlValue
            self.minRange_nlValue =  minRange_nlValue
            self.minBias_nlValue =  minBias_nlValue
            self.maxRange_nlValue = maxRange_nlValue
            self.maxBias_nlValue =  maxBias_nlValue
            self.increment_nlValue = increment_nlValue
            self.null_nlValue = null_nlValue

    def getSiteInfo(self, infoType, siteID):
        # Get information about an NWS site given the 3-letter site id
        # infoType can be: "region", "wfoCity", "wfoCityState", "fullStationID
        return SiteInfo.SiteInfo().getInfo(infoType, siteID)
