# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCMWindTool
#
# Version 2.7.1   2 Sept 2010  Modified to Fix RCL error
# Version 2.7.2   01 Feb 2011  Fixed Pie Slice Algorithm/Added Backgroun Options
# Version Last    14 Apr 2014  Added User-editable max winds
# Modified On   22 May 2014  Introduced option for handling asymetry
# in inner core (RMW), option to use 85th wind radii reduction based on
# 2009 paper by DeMaria or the NCST Bias Correction scheme outside radius
# of MaxWind, corrected problem with ring of lower wind value introduced 
# at times in the transition between 34 knots wind radii and background 
# field, and introduced option to use preliminary TCM message being 
# pushed to all offices text databases beginning with 2014 season.
#
# Modified: 1 Jun 2014 to fix bugs with Lat grids and add option
# to use WindReductionFactor grids for Mid Atlantic offices.
# Modified: 6 June 2014 to fix bugs with large reduction factors over land.
# Modified: 9 June 2014 to fix GUI option to run or not over Selected Time Range.
# 
# Modified: 2 July to fix decoding of PRE TCM files
# Whatever options are needed should be carefully coordinated among 
# offices.
#
# Last Modified: October 28, 2016 to add mis to maxwindswath == "Yes" and adjust
# shift variable in interpolateQuadrants method.
# Submitted for 17.1.1
#
# Author:  Tom LeFebvre
# Contributor: Pablo Santos
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Populate"]

VariableList = [("Product to\ndecode:", [], "check",
                 ["preTCM","WRKTCM","TCMAT1", "TCMAT2", "TCMAT3", "TCMAT4", "TCMAT5",
                  "TCMEP1", "TCMEP2", "TCMEP3", "TCMEP4", "TCMEP5"]),
                ("Product to\n decode:", [], "check",
                 ["PREAT1", "PREAT2", "PREAT3", "PREAT4", "PREAT5",
                  "PREEP1", "PREEP2", "PREEP3", "PREEP4", "PREEP5"]),
#                ("Background\nModel:", "Fcst", "radio", ["GFS0p5degGbl", "UKMET", "ECMWFHiRes", "Fcst"]),
                ("Number of Pie Slices?", "16", "radio", ["4", "8", "12", "16", "24"]),
                ("Eye Diameter:", 0, "scale", [0, 100], 1),
                ("34 knot radius at 3 days (NM):", 100, "scale", [0, 1000], 10),
                ("34 knot radius at 4 days (NM):", 100, "scale", [0, 1000], 10),
                ("34 knot radius at 5 days (NM):", 100, "scale", [0, 1000], 10),
                ("Decrease Wind over Land by (%):", 15, "scale", [-20, 50], 1),
                ("Make Grids over \nSelected Time Only:", "No", "radio", ["Yes", "No"]),
                ("MaxWind Swath for \nTCWindThreat?", "No", "radio", ["Yes", "No"]),
                ("Define Asymmetrical \nMax Winds?", "No", "radio", ["Yes", "No"]),
                ("Reduce Radii by 15% or \n NC State Bias Correction", "Reduce by 15%",
                 "radio", ["Reduce by 15%", "NC State Bias Correction"]),
                ("Constant Land\nReduction (Slider Bar)\nor Wind Reduction\nFactor Grid?",
                 "Constant", "radio", ["Constant", "Grid"]),               
                ]

try:  # See if this is the AWIPS I environment
    from Numeric import *
    import AFPS
    AWIPS_ENVIRON = "AWIPS1"
except:  # Must be the AWIPS II environment
    from numpy import *
    import AbsTime
    import TimeRange
    AWIPS_ENVIRON = "AWIPS2"

import SmartScript
import DefineMaxWindGUI
import MetLib

import popen2, string, time, os, cPickle
import Exceptions, types, copy

class TCMDecoder:
    def __init__(self):
        self.pos = 0
                            # key words in TCM products from NCEP
        self.keyWordDict = {"HURRICANE CENTER" : self.decodeProductTime,
                            "FORECAST VALID" : self.decodeWindForecast,
                            "CENTER LOCATED NEAR" : self.decodeCenterLocation,
                            "CENTER LOCATED INLAND NEAR" : self.decodeCenterLocation,
                            "MINIMUM CENTRAL PRESSURE" : self.decodeCentralPressure,
                            "MAX SUSTAINED WINDS" : self.decodeMaxSustainedWinds,
                            "MAX WIND" : self.decodeMaxWind,
                            "OUTLOOK VALID" : self.decodeWindForecast,
                            "EYE DIAMETER" : self.decodeEyeDiameter,
                            "64 KT..." : self.decodeRadii,
                            "50 KT..." : self.decodeRadii,
                            "34 KT..." : self.decodeRadii,
                            # key words for JTWC products
                            "WTPN" : self.decodeJTWCProductTime,
                            "WARNING POSITION:" : self.decodeJTWCTimeCenter,
                            "VALID AT:" : self.decodeJTWCWindForecast,
                            "RADIUS OF 034 KT WINDS" : self.decodeJTWCRadii,
                            "RADIUS OF 050 KT WINDS" : self.decodeJTWCRadii,
                            "RADIUS OF 064 KT WINDS" : self.decodeJTWCRadii,
                            "RADIUS OF 100 KT WINDS" : self.decodeJTWCRadii,
                            "    ---" : self.endJTWCWindForecast, 
                            "REMARKS:" : self.stopDecodingJTWC,
                            }

        self.fcstList = []  # a place to store all of the forecasts

        self.text = []  #  the text product
        
        self.currentFcst = {}  # the current forecast we are docoding

        self.baseProductTime = 0

        self.foundEyeDiameter = 0

        self.AltFileName = ""
        
    def calcEyeDiameter(self, center, maxWind):
        lat = center[0]   # latitude in degrees
        maxWind = maxWind / 1.944  # convert to meters per second
        rmw = 46.29 * exp(-0.0153 * maxWind + 0.0166 * lat)

        # convert to diameter and convert from km to nm
        ed = rmw * 2.0 / 1.852
        return ed

    def stripText(self):
        endStr = chr(13) + chr(13) + chr(10)
        for i in range(len(self.text)):
            self.text[i] = string.replace(self.text[i], endStr, "")
        return

    def getFcstList(self):
        return self.fcstList

    def getBaseProductTime(self):
        return self.baseProductTime

    def getAltInfoFileName(self):
        return self.AltFileName    
    
    def currentLine(self):
        return self.text[self.pos]

    def nextLine(self):
        self.pos = self.pos + 1
        if self.pos < len(self.text):
            return self.text[self.pos]
        else:
            return ""

    def monthNum(self, monthStr):
        monthList = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                     "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]

        try:
            return monthList.index(monthStr) + 1
        except ValueError:
            return 0 

    def convertBaseTime(self, timeStr):
        # timeStr format: "HHMM UTC DAY MON DD YYYY"

        # extract time parts from the str
        hour = int(timeStr[0:2])
        minute = int(timeStr[2:4])
        strList = string.split(timeStr)
        monthStr = strList[3]
        month = self.monthNum(monthStr)
        day = int(strList[4])
        year = int(strList[5])

        # time.mktime returns time in seconds but in local time
        baseTime = time.mktime((year, month, day, hour, minute, 0, 0, 0, 0))

        # Adjustment to UTC
        diffTime = time.mktime(time.gmtime()) - time.mktime(time.localtime())

        # subtract timeZone and round to the nearest hour
        roundedTime = int((baseTime - diffTime) / 3600) * 3600

        return roundedTime

    def convert_ddhhmm(self, ddhhmmStr, baseTime):

        # remove the slash if present
        ddhhmmStr = string.replace(ddhhmmStr, "/", "")

        if baseTime == 0:
            baseTime = time.time()

        # extract the time parts
        dayStr = ddhhmmStr[0:2]
        hourStr = ddhhmmStr[2:4]
        minStr = ddhhmmStr[4:6]
        day = int(dayStr)
        hour = int(hourStr)
        minute = int(minStr)
        tupleTime = time.gmtime(baseTime)
        year = tupleTime[0]
        month = tupleTime[1]
        # see if we crossed over to a new month
        if tupleTime[2] > day:
            month = month + 1
            if month > 12:
                month = 1
                year = year + 1

        newTuple = (year, month, day, hour, minute, tupleTime[5],
                    tupleTime[6], tupleTime[7], tupleTime[8])

        secondsTime = time.mktime(newTuple)
        # Adjustment to UTC
        diffTime = time.mktime(time.gmtime()) - time.mktime(time.localtime())
        return secondsTime - diffTime  # subtract timeZone

    def decodeProductTime(self):
        # extract the alt filename
        self.decodeAltFileName()
        # Time of the product found on the next line
        timeStr = self.nextLine()
        # sanity check for the time string
        hhmm = timeStr[0:4]
        for c in hhmm:
            if not c in string.digits:
                return
            
        baseTime = self.convertBaseTime(timeStr)
        self.baseProductTime = baseTime
        return

    def decodeAltFileName(self):
        nameStr = self.currentLine()
        parts = string.split(nameStr)

        self.AltFileName = parts[-1]  # grab the last string token

        return 

    def decodeCenterLocation(self):
        locStr = self.currentLine()
        # check for the repeat center....don't want this one
        if string.find(locStr, "REPEAT") >= 0:
            return
        
        keyWord = "NEAR"
        pos = string.find(locStr, keyWord)
        if pos > -1:  # found it
            locStr = locStr[pos + len(keyWord):]
            tokenList = string.split(locStr)
            if len(tokenList) >= 2:
                lat = self.decodeLatLonToken(tokenList[0])
                lon = self.decodeLatLonToken(tokenList[1])

                if len(tokenList) > 3:  # grab the time
                    validTime = self.convert_ddhhmm(tokenList[3], self.baseProductTime)
                # New fcst (analysis actually)
                self.currentFcst = {}
                self.currentFcst['validTime'] = validTime
                self.currentFcst['centerLocation'] = (lat, lon)
                self.currentFcst['radii'] = {}
                self.currentFcst['eyeDiameter'] = self.defaultEyeDiameter

        return

    def decodeCentralPressure(self):
        keyWord = "MINIMUM CENTRAL PRESSURE"
        presStr = self.currentLine()
        pos = string.find(presStr, keyWord)
        if pos > -1:  # found it
            presStr = presStr[pos + len(keyWord):]
            
        return
   
    def decodeMaxSustainedWinds(self):
        keyWord = "MAX SUSTAINED WINDS"
        windStr = self.currentLine()
        pos = string.find(windStr, keyWord)
        if pos > -1:  # found it
            windList = []
            tokenList = string.split(windStr)
            for i in range(len(tokenList)):
                if string.find(tokenList[i], "KT") >= 0:
                    windList.append(float(tokenList[i - 1]))

        # Sometimes there is no max wind/gust reported
        if windList == []:
            return

        # store the max wind
        self.currentFcst["maxWind"] = windList[0]
        self.currentFcst["maxGust"] = windList[1]

        # if we have a center location and a max wind we can calc
        # the eye diameter
        if self.currentFcst.has_key('centerLocation') and \
           self.currentFcst.has_key('maxWind'):
            # if it's zero it's not in the product and the user didn't
            # change it, so calculate it based on the Willoughby formula
            if self.currentFcst.has_key('eyeDiameter') and \
                   self.currentFcst['eyeDiameter'] == 0:
                self.currentFcst['eyeDiameter'] = self.calcEyeDiameter(
                    self.currentFcst['centerLocation'],
                    self.currentFcst['maxWind'])
            else:  # otherwise use what's been defined or read from the text
                self.currentFcst['eyeDiameter'] = self.defaultEyeDiameter
            
        return

    def decodeMaxWind(self):
        str = self.currentLine()
        str = string.replace(str, '.', ' ')  # remove ...
        tokenList = string.split(str)
        if len(tokenList) >= 6:
            maxWind = float(tokenList[2])
            maxGust = float(tokenList[5])

        # store in current fcst
        self.currentFcst["maxWind"] = maxWind
        self.currentFcst["maxGust"] = maxGust

        # if we have a center location and a max wind we can calc
        # the eye diameter
        if self.currentFcst.has_key('centerLocation') and \
           self.currentFcst.has_key('maxWind'):
            # if it's zero it's not in the product and the user didn't
            # change it, so calculate it based on the Willoughby formula
            if self.currentFcst.has_key('eyeDiameter') and \
                   self.currentFcst['eyeDiameter'] == 0:
                self.currentFcst['eyeDiameter'] = self.calcEyeDiameter(
                    self.currentFcst['centerLocation'],
                    self.currentFcst['maxWind'])
            else:  # otherwise use what's been defined or read from the text
                self.currentFcst['eyeDiameter'] = self.defaultEyeDiameter

        return

    def decodeRadii(self):
        # if there's no currentFcst dict, we cannot continue
        if self.currentFcst == {}:
            return


        str = self.currentLine()
        str = string.replace(str, '.', ' ')  # remove ...
        tokenList = string.split(str)
        # check for KT in the second slot
        if len(tokenList) < 4 or tokenList[1] != "KT":
            return
        radiiWindValue = float(tokenList[0])
        dirList = ["NE", "SE", "SW", "NW"]
        radiusList = []
        for token in tokenList:
            for d in dirList:
                pos = string.find(token, d)
                if pos >= 0:
                    radiusStr = token[:pos]
                    radius = float(radiusStr)
                    radiusList.append(radius)
        # store the radii info
        self.currentFcst['radii'][radiiWindValue] = radiusList

        return 

    def decodeWindForecast(self):
        # if we're decoding a new forecast, save the old one first
        if self.currentFcst != {}:
            self.fcstList.append(self.currentFcst)
            self.currentFcst = {}  # reset
        
        str = self.currentLine()
        str = string.replace(str, '...', '   ')  # remove ...
        
        tokenList = string.split(str)
        # decode the validTime
        validTime = self.convert_ddhhmm(tokenList[2], self.baseProductTime)
        # decode the center location
        if len(tokenList) >= 5:
            lat = self.decodeLatLonToken(tokenList[3])
            lon = self.decodeLatLonToken(tokenList[4])
            # If we can't decode the lat or lon it's probably an outlook
            # with no guidance so just return
            if lat == None or lon == None:
                print "Failed to decode latStr:", lat, "lonStr:", lon
                return

            # initialize a new forecast and begin filling values
            self.currentFcst = {}
            self.currentFcst['validTime'] = validTime
            self.currentFcst['centerLocation'] = (lat, lon)
            self.currentFcst['radii'] = {}
            self.currentFcst['eyeDiameter'] = self.defaultEyeDiameter

        return

    def decodeEyeDiameter(self):
        str = self.currentLine()

        tokenList = string.split(str)
        diameter = int(tokenList[2])

        self.currentFcst['eyeDiameter'] = diameter

        # Since we found it in the procuct, set the default diameter
        self.defaultEyeDiameter = diameter
        self.foundEyeDiameter = 1  # mark that we found it
        return

    def decodeTCMProduct(self, TCMProduct, eyeDiameter):
        self.text = TCMProduct
        self.pos = 0
        self.fcstList = []
        self.defaultEyeDiameter = eyeDiameter

        self.stripText()

        try:
            while self.pos < len(TCMProduct):
                line = self.currentLine()
                for k in self.keyWordDict.keys():
                    if string.find(line, k) > -1:
                        self.keyWordDict[k]()
                        break
                self.pos = self.pos + 1

            # store the last forecast in the list of forecasts
            if self.currentFcst != {}:
                self.fcstList.append(self.currentFcst)
                self.currentFcst = {}  # reset
        except:
            # Some problem occured during the decoding process so return an empty fcst
            self.baseProductTime = 0
            self.fcstList = {}  # reset
            
        return

    def decodeLatLonToken(self, latLonStr):
        dirList = ['N', 'S', 'E', 'W']
        for d in dirList:
            pos = string.find(latLonStr, d)
            if pos >= 0:
                try:
                    value = float(latLonStr[0:pos])
                    if d == 'S' or d == 'W':
                        value = -value  # flip the numeric sign
                    return value
                except:
                    # it was not decodable (not numbers)
                    print "Failed to decode lat/lon token:", latLonStr
                    return None

        # undecodable latLon for some reason
        return None

    def decodeJTWCProductTime(self):
        line = self.currentLine()
        tokenList = string.split(line)
        ddhhmmStr = tokenList[2]
        self.baseProductTime = self.convert_ddhhmm(ddhhmmStr, 0)

        self.baseProductTime = int(self.baseProductTime / 3600) * 3600
        return None
    
    def decodeJTWCTimeCenter(self):
        line = self.nextLine()
        tokenList = string.split(line)
        dateTimeStr = tokenList[0][0:6]
        latStr = tokenList[3]
        lonStr = tokenList[4]

        # could be None
        lat = self.decodeLatLonToken(latStr)
        lon = self.decodeLatLonToken(lonStr)
        if lon > 0:
            lon = lon - 360.0
        productTime = self.convert_ddhhmm(dateTimeStr, self.baseProductTime)

        # make a new fcst object to store the analysis
        self.currentFcst = {}
        self.currentFcst['validTime'] = productTime
        self.currentFcst['centerLocation'] = (lat, lon)
        self.currentFcst['radii'] = {}
        self.currentFcst['eyeDiameter'] = self.defaultEyeDiameter
        
    def decodeJTWCWindForecast(self):
        line = self.nextLine()

        tokenList = string.split(line)

        # Grab everything just to the left of the first 'Z'
        zPos = string.find(tokenList[0], 'Z')
        if zPos >= 0:
            timeStr = tokenList[0][0:zPos]
            validTime = self.convert_ddhhmm(timeStr, self.baseProductTime)
        else:
            print "couldnt find Z in timeStr:", line
            return

        latStr = tokenList[2]
        lonStr = tokenList[3]
        lat = self.decodeLatLonToken(latStr)
        lon = self.decodeLatLonToken(lonStr)
        if lon > 0:
            lon = lon - 360.0

        # make a new currentFcst and store the info
        self.currentFcst = {}
        self.currentFcst['validTime'] = validTime
        self.currentFcst['centerLocation'] = (lat, lon)
        self.currentFcst['radii'] = {}
        self.currentFcst['eyeDiameter'] = self.defaultEyeDiameter
        return 

    def decodeJTWCRadii(self):
        line = self.currentLine()
        radList = []
        windSpeed = 0
        while string.find(line, "---") == -1 and line != "":
            tokenList = string.split(line)
            if string.find(line, "RADIUS") >= 0:   # it's the first line
                # check to see if we need to store the radii first
                if radList != []:  # we decoded some already
                    self.currentFcst['radii'][windSpeed] = radList
                    radList = []

                # extract the windSpeed for these radii
                windSpeed = float(tokenList[2])
                if string.find(line, "QUADRANT") == -1:  # no "QUADRANT" found
                    radius = float(tokenList[6])
                    radList = [radius, radius, radius, radius]
                else: # QUADRANT found
                    radius = float(tokenList[6])
                    radList = [radius]
            else: # no RADIUS found so maybe a QUADRANT line
                if string.find(line, "QUADRANT") >= 0:                    
                    radius = float(tokenList[0])
                    radList.append(radius)
                
            line = self.nextLine()

        # save the last radii info
        if radList != []:
            self.currentFcst['radii'][windSpeed] = radList

        # save the whole forecast in the list
        self.fcstList.append(self.currentFcst)
        self.currentFcst = {}

        return 

    def endJTWCWindForecast(self):
        
        if self.currentFcst != {}:
            self.fcstList.append(self.currentFcst)

        self.currentFcst = {}
        return 
        
    def stopDecodingJTWC(self):
        line = "ZZZZZ"
        while line != "":
            line = self.nextLine()
        return
    
#  end class TCMDecoder

# begin class CircleEA
# This class helps make circular edit areas and quadrants thereof.
class CircleEA(SmartScript.SmartScript):
    def __init__(self, latGrid, lonGrid, center, slices):
        pi = 3.1459
        RadsPerDeg = 2 * pi / 360
        cosLatGrid = cos(latGrid * RadsPerDeg)
        self.xDist = (lonGrid - center[1]) * 111.1 * cosLatGrid
        self.yDist = (latGrid - center[0]) * 111.1
        self.distGrid = sqrt(pow(self.xDist, 2)+ pow(self.yDist, 2))
        
        self.tanGrid = arctan2(-self.xDist, -self.yDist)
        # mask off all but the specified quadrant.
        self.quadList = []
        for quad in range(1, slices + 1):
            minValue = -pi + (quad - 1) * 2 * pi / slices
            maxValue = -pi + quad * 2 * pi / slices

            quadrant = logical_and(greater_equal(self.tanGrid, minValue),
                                   less(self.tanGrid, maxValue))
            self.quadList.append(quadrant)
            
        return

    # Return an edit area for just one quadrant.
    # By convention quadrant numbering starts at 1 (due North) and
    # progresses clockwise by one slice increment
    def getQuadrant(self, quad, radius):
        # trim the mask beyond the specified radius
        radiusMask = less_equal(self.distGrid, radius)

        quadrant = logical_and(radiusMask, self.quadList[quad - 1])
        return quadrant

    def getDistanceGrid(self):
        return self.distGrid

    def getXYDistGrids(self):
        return self.xDist, self.yDist
    
# end class CircleEA -------------------------------------------------------


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss
        
    # Make a timeRange based on the start and end int times
    def makeTimeRange(self, start=0, end=0):
        
        if AWIPS_ENVIRON == "AWIPS1":

            if start == 0 and end == 0:
                return AFPS.TimeRange.allTimes()
                
            startTime = AFPS.AbsTime(start)
            endTime = AFPS.AbsTime(end)
    
            tr = AFPS.TimeRange(startTime, endTime)

        elif AWIPS_ENVIRON == "AWIPS2":
            if start == 0 and end == 0:
                startTime = AbsTime.AbsTime(start)
                endTime = AbsTime.maxFutureTime()
            else:        
                startTime = AbsTime.AbsTime(start)
                endTime = AbsTime.AbsTime(end)
    
            tr = TimeRange.TimeRange(startTime, endTime)
        else:
            self.statusBarMsg("Unknown AWIPS version", "U")
            tr = None

        return tr
    
    def getParmTimeConstraints(self, weName, dbName):

        parm = self.getParm(dbName, weName, "SFC")
        
        if AWIPS_ENVIRON == "AWIPS1":
            parmStart = parm.timeConstraints().startTime()
            parmDuration = parm.timeConstraints().duration()
            parmRepeat = parm.timeConstraints().repeatInterval()
            
        elif AWIPS_ENVIRON == "AWIPS2":
            parmStart = parm.getGridInfo().getTimeConstraints().getStartTime()
            parmDuration = parm.getGridInfo().getTimeConstraints().getDuration()
            parmRepeat = parm.getGridInfo().getTimeConstraints().getRepeatInterval()
        else:
            self.statusBarMsg("Unknown AWIPS version", "U")
            return None, None, None

        return parmStart, parmDuration, parmRepeat


    # Use this method if you have no luck getting products
    # directly from the text database
    def getTextProductFromFile(self, filename):
        f = file(filename, 'r')
        textList = []
        line = f.readline()
        textList.append(line)
        while line != "":
            line = f.readline()
            textList.append(line)
        f.close()
        return textList

    # Retrieves a text product from the text database
    def getTextProductFromDB(self, productID):
        
        cmd = "textdb -r " + productID

        # if your path does not include FXA_HOME/bin,
        # this line may work instead of the above line.
#        cmd = "/awips/fxa/bin/textdb -r " + productID

        (stdout, stdin, stderr) = popen2.popen3(cmd)
        
        textList = []
        line = stdout.readline()
        textList.append(line)
        while line != "":
            line = stdout.readline()
            textList.append(line)
        return textList

    def printFcst(self, f, baseTime=None):
        print "=============================================================="
        print "Time:", time.asctime(time.gmtime(f['validTime'])),
        if baseTime is not None:
            print "LeadTime:", (f['validTime'] - baseTime) / 3600 + 3
        print "Center:", f['centerLocation']
        print "Eye:", f['eyeDiameter']
        if f.has_key('maxWind'):
            print "Max Wind:", f['maxWind']
        radKeys = f['radii'].keys()
        sort(radKeys)
        print "RADII:"
        for r in radKeys:
            print r, "kts:", f['radii'][r]

    def getWEInventory(self, modelName, WEName, level):
        allTimes = self.makeTimeRange(0, 0)
        gridInfo = self.getGridInfo(modelName, WEName, level, allTimes)
        trList = []
        for g in gridInfo:
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            tr = self.makeTimeRange(start, end)
            trList.append(tr)

        return trList

    def timeRangeSort(self, a, b):
        if a.startTime() <= b.startTime():
            return -1
        else:
            return 1
        
    # returns a wind grid from the specified model most closely matched in
    # time
    def getClosestWindGrid(self, modelName, bgDict, timeTarget):
        topo = self.getTopo()
        calmGrid = self.makeWindGrid(0.0, 0.0, topo.shape)
        
        if len(bgDict.keys()) == 0:
#            print "No background grids available...Using calm grid."
            return calmGrid

        minDiff = 3600 * 24 * 365  # just a large number
        gridIndex = -1
        tr = None

        # sort the keys by time so we get consistent behavior
        bgKeys = bgDict.keys()
        bgKeys.sort(self.timeRangeSort)
        targetTR = self.makeTimeRange(timeTarget, timeTarget + 3600)
        # figure out which grid is closest in time
        for invTR in bgKeys:

            # if we have an exact match, we're done
            if invTR.overlaps(targetTR):
                tr = invTR  # set the tr
                minDiff = 0
                break

            # update stats for "closest" grid
            gTime = invTR.startTime().unixTime()
            diff = abs(gTime - timeTarget)
            
            if diff < minDiff:
                tr = invTR
                minDiff = diff

        # if we're off by more than 4 hours, return a calm grid
        if minDiff > (4 * 3600):
            return calmGrid

        # return the closest grid in time
        if modelName == "Fcst":
                grid = bgDict[tr]
        else:
            grid = bgDict[tr]
            grid = (grid[0] * 1.944, grid[1]) # convert from m/s

        return grid

    # makes a direction grid where winds blow counter-clockwise about
    # the specified center.
    def makeDirectionGrid(self, latGrid, lonGrid, latCenter, lonCenter):
        cycWt = 0.7  # cyclonic circulation weight
        convWt = 0.3  # convergence weight
        cycU = -(latGrid - latCenter)  # pure counter-clockwise circulation
        cycV = lonGrid - lonCenter
        convU = -cycV  # pure convergence
        convV = cycU
        u = cycU * cycWt + convU * convWt
        v = cycV * cycWt + convV * convWt
        mag, dir = self.UVToMagDir(u, v) 

        return dir

    # interpolates radii information based on the specified info.
    # returns a new radii
    def interpRadii(self, t1, t2, newTime, f1Radii, f2Radii):
        # Add radii if they are not there
        radiiList = [34.0, 50.0, 64.0, 100.0]
        for r in radiiList:
            if not f1Radii.has_key(r):
                f1Radii[r] = [0, 0, 0, 0]
            if not f2Radii.has_key(r):
                f2Radii[r] = [0, 0, 0, 0]

        newRadii = {}
        for r in radiiList:
            quadList = []
            # Check for partial list of radii
            if len(f1Radii[r]) < 4 or len(f2Radii[r]) < 4:
                print "Partial radii list found. Substituting with zero radius."

            #  Add zeros if list is partial
            while len(f1Radii[r]) < 4:
                f1Radii[r].append(0)
            while len(f2Radii[r]) < 4:
                f2Radii[r].append(0)

            for i in range(4):
                r1 = f1Radii[r][i]
                r2 = f2Radii[r][i]
                radius = r1 + (r2 - r1) * (newTime - t1) / (t2 - t1)
                quadList.append(radius)
            newRadii[r] = quadList

        return newRadii

    # interpolate the wind forecasts inbetween the two specified forecasts.
    # interval is assumed to be specified in hours.
    # returns a new list of forecasts with f1 at the front of the list
    # and f2 not present at all in the list.
    def interpolateWindFcst(self, f1, f2, interval):
        
        intSecs = 3600 * interval
        t1 = f1['validTime']
        t2 = f2['validTime']
        # Just return the first fcst if the interval is too big
        if t2 - t1 <= intSecs:
            return [f1]

        f1Lat = f1['centerLocation'][0]
        f1Lon = f1['centerLocation'][1]
        f2Lat = f2['centerLocation'][0]
        f2Lon = f2['centerLocation'][1]
        f1Eye = f1['eyeDiameter']
        f2Eye = f2['eyeDiameter']
        tDiff = f2['validTime'] - f1['validTime']
        f1MaxWind = f1['maxWind']
        f2MaxWind = f2['maxWind']
        timeSlots = int(tDiff / intSecs)
        dLat = (f2Lat - f1Lat) / timeSlots
        dLon = (f2Lon - f1Lon) / timeSlots
        dEye = (f2Eye - f1Eye) / timeSlots
        dMaxWind = (f2MaxWind - f1MaxWind) / timeSlots
        f1Radii = f1['radii']
        f2Radii = f2['radii']

        if f1.has_key("editedMaxWinds"):
            emw1 = array(f1["editedMaxWinds"])
            emw2 = array(f2["editedMaxWinds"])
            demw = (emw2 - emw1) / timeSlots

        fcstList = [f1]  # include the first fcst in the list
        for i in range(1, timeSlots):
            newTime = t1 + (i * intSecs)
            newLat = f1Lat + (i * dLat)
            newLon = f1Lon + (i * dLon)
            newEye = f1Eye + (i * dEye)
            newMaxWind = f1MaxWind + (i * dMaxWind)
            if f1.has_key("editedMaxWinds"):
                newEMW = emw1 + (i * demw)

            newRadii = self.interpRadii(t1, t2, newTime, f1Radii, f2Radii)
            f = {}
            f['centerLocation'] = (newLat, newLon)
            f['eyeDiameter'] = newEye
            f['validTime'] = newTime
            f['maxWind'] = newMaxWind
            f['radii'] = newRadii
            if f1.has_key("editedMaxWinds"):
                f['editedMaxWinds'] = list(newEMW)
            fcstList.append(f)

        return fcstList

    def calcRadiusList(self, maxWind, rmw, rad34, newRadii):
        for i in range(len(newRadii)):
            # linearly interpolate
            newRadii[i] = rmw + ((rmw - rad34) / (maxWind - 34.0)) / (64.0 - maxWind)
            if newRadii[i] < 0:
                newRadii[i] = 0
        return newRadii


    # This method fills in radii/wind values one way for the 36-72 hour period
    # and another way for the 72-120 hour period.  The concept is to add more
    # data values to the wind field so that the wind grids look more realistic.
    def extrapolateRadii(self, fcstList, baseDecodedTime, radiiFactor):
        for i in range(1, len(fcstList)):
            fcst = fcstList[i]
            prevFcst = fcstList[i-1]

            # calc the lead time in hours
            leadTime = (fcst['validTime'] - baseDecodedTime) / 3600 + 3


            extRadius = self.getOutlookRadius(leadTime)
            zeroRadius = extRadius * radiiFactor

            if leadTime <= 36:   # no extrapolation for these times
                continue
            # for this period, manufacture new 64 knot radii under specific conditions
            if leadTime > 36 and leadTime <= 72:
                # make sure we have the data we need
                if not prevFcst['radii'].has_key(64):
                    continue
                if not prevFcst['radii'].has_key(50):
                    continue
                if not fcst['radii'].has_key(50):
                    continue
                if fcst['radii'].has_key(64):
                    continue

                if fcst['maxWind'] <= 64:
                    continue

                prev50 = prevFcst['radii'][50]
                prev64 = prevFcst['radii'][64]
                fcst50 = fcst['radii'][50]
                newRadii = [0, 0, 0, 0]
                for i in range(len(prev50)):
                    if prev50[i] == 0:
                        continue

                    newRadii[i] = fcst50[i] / prev50[i] * prev64[i]

                if not fcst['radii'].has_key(64):
                    fcst['radii'][64] = newRadii
                # add in a 5 knot radius for better blending
                fcst['radii'][5.0] = [zeroRadius, zeroRadius, zeroRadius, zeroRadius]
                
            elif leadTime > 72:    # different algorithm for beyond 72 hours

                # if there are radii already defined, don't extrapolate new radii
                if fcst.has_key("radii"):
                    if len(fcst["radii"]) > 0:
                        continue

                # Stuff radii into the rDict to make a cyclone
                maxWind = 0
                if fcst.has_key("maxWind"):
                    maxWind = fcst["maxWind"]

                rDict = {}

                # add the radii for maxWind at the rmw        
                if maxWind > 0:
                    # calculate an rmw
                    lat = fcst["centerLocation"][0]   # latitude in degrees
                    rmw = 46.29 * exp(-0.0153 * (maxWind / 1.944) + 0.0166 * lat)
                    rmw = rmw / 1.852   # convert to nautical miles

                for ws in [64.0, 50.0]:
                    newRadii = [0, 0, 0, 0]
                    if ws < maxWind:
                        newRadii = self.calcRadiusList(maxWind, rmw, extRadius, newRadii)
                        rDict[ws] = newRadii
                    
                rDict[34.0] = [extRadius, extRadius, extRadius, extRadius]
                rDict[5.0] = [zeroRadius, zeroRadius, zeroRadius, zeroRadius]
                fcst['radii'] = rDict
                
        return fcstList

    # Smooths the specified grid by the specified factor
    # With factor == 3, 3x3 smooth, factor == 5 5x5 smooth, etc.
    # Even factors (4, 6, 8,...) round up to the next odd value
    # If factors <3 are specified, the unmodified grid is returned.
    def smoothGrid(self, grid, factor):
        # factors of less than 3 are useless or dangerous
        if factor < 3:
            return grid

        # Specifying the grid type depends on the environment

        typecode = float64
        
        st = time.time()
        half = int(factor)/ 2
        sg = zeros(grid.shape, typecode)
        count = zeros(grid.shape, typecode)
        gridOfOnes = ones(grid.shape, typecode)

        for y in range(-half, half + 1):
            for x in range(-half, half + 1):
                if y < 0:
                    yTargetSlice = slice(-y, None, None)
                    ySrcSlice = slice(0, y, None)
                if y == 0:
                    yTargetSlice = slice(0, None, None)
                    ySrcSlice = slice(0, None, None)
                if y > 0:
                    yTargetSlice = slice(0, -y, None)
                    ySrcSlice = slice(y, None, None)
                if x < 0:
                    xTargetSlice = slice(-x, None, None)
                    xSrcSlice = slice(0, x, None)
                if x == 0:
                    xTargetSlice = slice(0, None, None)
                    xSrcSlice = slice(0, None, None)
                if x > 0:
                    xTargetSlice = slice(0, -x, None)
                    xSrcSlice = slice(x, None, None)

                target = [yTargetSlice, xTargetSlice]
                src = [ySrcSlice, xSrcSlice]
                sg[target] = sg[target] + grid[src]
                count[target] = count[target] + gridOfOnes[src]
        return sg / count

    # Smooths the direction grid without regard to the magnitude
    def smoothDirectionGrid(self, dirGrid, factor):
        mag = ones(dirGrid.shape, float32)  # 1.0 everywhere
        u, v = self.MagDirToUV(mag, dirGrid)
        u = self.smoothGrid(u, factor)
        v = self.smoothGrid(v, factor)
        mag, dirGrid = self.UVToMagDir(u, v)
        return dirGrid
    
    def makeWindGrid(self, mag, direct, gridShape):
        mag = ones(gridShape, float32) * mag
        direct = ones(gridShape, float32) * direct
        return mag, direct

    def decreaseWindOverLand(self, grid, fraction, Topo, timeRange):

        if self.lessOverLandGrid == "Grid":
            
            windFactorGrid = self.getWindReductionFactorGrid("Fcst", timeRange)
            if windFactorGrid is not None:
                # Restrict reduction to the cyclone winds defined by the TCM
                grid = where(self._cycloneMask, grid * (1 - windFactorGrid), grid)
                return grid
            else:
                # If no grid was found just return the standard reduction
                self.statusBarMsg("Wind Reduction Factor grid not found. Using standard reduction." , "S")

        # If area over which you desire to apply land correction you prefer be
        # based on Edit Are instead of areas with Topo greater than zero then
        # uncomment the next two lines and specify Edit Area to use.
        
        #editArea = self.getEditArea("LAND_EDIT_ARE_NAME_HERE")
        #mask = self.encodeEditArea(editArea)

        # Restrict reduction to the cyclone winds defined by the TCM
        mask = logical_and(greater(Topo, 0.0), self._cycloneMask)
        
        grid = where(mask, grid * fraction, grid)
        
        return grid
    
    # fetches and returns all of the wind reduction factor grids in Fcst DB.
    def getWindReductionFactorGrid(self, modelName, timeRange):
        try:
            inv = self.getWEInventory(modelName, "WindReductionFactor", "SFC")
            for tr in inv:
                if tr.overlaps(timeRange):
                    WindRedGrid = self.getGrids(modelName, "WindReductionFactor", "SFC",
                                                timeRange, mode="First")
                    return WindRedGrid
            # If no overlapping grids, return None
            return None
        except:
            return None
                
    def getTimeConstraintDuration(self, element):
        
        parmStart, parmDuration, parmRepeat = self.getParmTimeConstraints(element, "Fcst")
        return parmDuration

    def getParmMinMaxLimits(self, modelName, weName):
        
        parm = self.getParm(modelName, weName, "SFC")

        if AWIPS_ENVIRON == "AWIPS1":
            return parm.minLimit(), parm.maxLimit()
        elif AWIPS_ENVIRON == "AWIPS2":
            return parm.getGridInfo().getMinValue(), parm.getGridInfo().getMaxValue()
        else:
            self.statusBarMsg("Unknown AWIPS version", "U")
            return None, None
        
        return
    
    # returns the maximum allowable wind speed based on NWS directives
    def getMaxAllowableWind(self, maxWind):
        minAllowable, maxAllowable = self.getParmMinMaxLimits("Fcst", "Wind")

        return min(maxWind, maxAllowable)
    
    # returns an interpolated radius based on input radii
    def getOutlookRadius(self, leadTime):
        leadTimeList = [72, 96, 120]
        radiusList = [self.day3Radius, self.day4Radius, self.day5Radius]

        if leadTime < leadTimeList[0]:
            return radiusList[0]
    
        for i in range(1, len(leadTimeList)):
            if leadTime < leadTimeList[i]:
                dt = leadTimeList[i] - leadTimeList[i - 1]
                dr = radiusList[i] - radiusList[i - 1]
                return radiusList[i - 1] + (leadTime - leadTimeList[i - 1]) * dr / dt

        return radiusList[-1]  # return the last item

    #  Blends the specified grid together
    def blendGrids(self, windGrid, bgGrid):

        # Combine the two grids using the windGrid for the cyclone and the
        # background grid everywhere else.

        windMag, windDir = windGrid
        bgMag, bgDir = bgGrid

        mask = greater_equal(windMag, 34.0)

        # No background winds inside any defined wind radii
        # Add in the point inside the defined wind radii
        mask = logical_or(mask, self._cycloneMask)

        magGrid = where(mask, windMag, bgMag)
        dirGrid = where(mask, windDir, bgDir)

        return magGrid, dirGrid

    def getLatLonGrids(self):
        # Try to get them from the fcst database to save time
        try:
            trList = self.getWEInventory("Fcst", "latGrid", "SFC")
        except:
            trList= []

        if len(trList) > 0:
            timeRange = trList[0]
            latGrid = self.getGrids("Fcst", "latGrid", "SFC", timeRange,
                                    mode = "First", noDataError = 0)
            lonGrid = self.getGrids("Fcst", "lonGrid", "SFC", timeRange,
                                    mode = "First", noDataError = 0)
            if latGrid != None and lonGrid != None:
                return latGrid, lonGrid

        # make the lat and lon grids
        gridLoc = self.getGridLoc()

        latGrid, lonGrid = MetLib.getLatLonGrids(gridLoc)

        start = int(time.time() / (24 * 3600)) * 24 * 3600
        end = start + (24 * 3600)
        timeRange = self.makeTimeRange(start, end)

        # Temporarily save them in the forecast database
        self.createGrid("Fcst", "latGrid", "SCALAR", latGrid, timeRange,
                        descriptiveName=None, timeConstraints=None,
                        precision=1, minAllowedValue=-90.0,
                        maxAllowedValue=90.0)
        
        self.createGrid("Fcst", "lonGrid", "SCALAR", lonGrid, timeRange,
                        descriptiveName=None, timeConstraints=None,
                        precision=1, minAllowedValue=-360.0,
                        maxAllowedValue=180.0)

        return latGrid, lonGrid

    # This method interpolates the specified radii in rDict to the
    # number of slices specified in pieSlices.  This adds more angular
    # resolution to the wind forecast which typically comes with 4 slices.
    def interpolateQuadrants(self, rDict, pieSlices):
        # make sure we have something to do first
        if pieSlices <= 4:
            return rDict

        newDict = {}
        for k in rDict.keys():
            rList = rDict[k]  # fetch the list of radii

            interpFactor = pieSlices / len(rList)
            newList = []
            for i in range(-1, len(rList) -1):
                minVal = rList[i]
                maxVal = rList[i + 1]
                dVal = (maxVal - minVal) / interpFactor
                for f in range(interpFactor):
                    radius = minVal + dVal * f
                    # make sure we never exceed the forecast radius
##                    if radius > minVal:
##                        radius = minVal
                    newList.append(radius)

            # Since we started with the NW quadrant we need to shift
            # the list so that it starts at North to conform to convention
            shift = int(pieSlices / 4) - 1
            shiftedList = newList[shift:]
            shiftedList = shiftedList + newList[:shift]
            newDict[k] = shiftedList
        return newDict


    # fetches and returns all of the wind grids specified by the model
    # name.  Should be called before any new wind grids are created
    def getBackgroundGrids(self, modelName):
        bgDict = {}

        modelName = "Fcst"
        siteID = self.getSiteID()
        if modelName == "Fcst":
            level = "SFC"
            elementName = "Wind"
        else:
            modelName = siteID + "_D2D_" + modelName
            if modelName.find("ECMWFHiRes") > -1:
                level = "SFC"
                elementName = "wind"
            else:
                level = "FHAG10"
                elementName = "wind"

        inv = self.getWEInventory(modelName, elementName, level)
        for tr in inv:
            bgDict[tr] = self.getGrids(modelName, elementName, level,
                                       tr, mode="First")
        return bgDict
    
    def secondsToYYYYMMDDHH(self, baseTime):
        # convert the base time to a string
        gTime = time.gmtime(baseTime)
        yearStr = str(gTime.tm_year)
        monthStr = str(gTime.tm_mon)
        dayStr = str(gTime.tm_mday)
        hourStr = str(gTime.tm_hour)
        while len(monthStr) < 2:
            monthStr = "0" + monthStr
        while len(dayStr) < 2:
            dayStr = "0" + dayStr
        while len(hourStr) < 2:
            hourStr = "0" + hourStr

        baseTimeStr = yearStr + monthStr + dayStr + hourStr
       
        return baseTimeStr
    

    # returns the index corresponding to the specified timeStr and fcstHour
    def findFcst(self, fcstList, fcstHour):
        for i in range(len(fcstList)):
            validTime = fcstList[i]["validTime"]
            leadTime = (validTime - self.baseDecodedTime) / 3600
            if fcstHour == leadTime:
                return i

        return None

    # Accepts the number of slices to interpolate and a list of defined
    # wind values.  Returns a new list of length slices with the specified
    # windList interpolated to the new resolution.
    def interpWindMax(self, slices, windList):
        
        maxWindList = [0.0] * slices

        quads = len(windList)
        ratio =  slices / quads
        intOffset = int(ratio / 2)
        floatOffset = float(intOffset) / ratio
        sliceMap = []
        windPos = [0] * len(windList)

        # Figure out the left and right positions for each new slice
        for i in range(slices):
            left = int((i - int(ratio/2)) / ratio)
            if i % ratio == int(ratio/2):
                right = left
                windPos[left] = i
            else:
                right = left + 1

            if right >= quads:
                right = right - quads

            sliceMap.append((left, right))

        # Do the actual interpolation based on the above positions
        interpWindList = []
        for i in range(slices):
            left, right = sliceMap[i]

            if left == right:
                val = windList[left]
                absDist = 1.1111
            elif windPos[left] > windPos[right]:
                absDist = slices - abs(windPos[right] - windPos[left])
            else:
                absDist = abs(windPos[right] - windPos[left])

            diff = i - windPos[left]
            if diff < 0:
                diff = slices + diff
            val = windList[left] + diff * ((windList[right] - windList[left]) / absDist)
            interpWindList.append(val)

        return interpWindList


    # Calculate the radius of the maxWind based on teh specified eyeDiameter
    def maxWindRadius(self, eyeDiameter=None):

        if eyeDiameter is None:
            return 12.5

        rmw = (eyeDiameter / 2.0) + 8.0
        
        return rmw

    def adjustMaxWind(self, outSpeed, inSpeed, outRadius, inRadius,
                      globalMaxWind, maxWindList, maxWindRadius, quad):

        maxWind = maxWindList[quad]
        
        # check which speed/radius should be modified
        if outSpeed == globalMaxWind:
            outSpd = maxWind
            outRad = maxWindRadius
            inSpd = inSpeed
            inRad = inRadius
        elif inSpeed == globalMaxWind:
            inSpd = maxWind
            inRad = maxWindRadius
            outSpd = outSpeed
            outRad = outRadius
        else:
            print "ERROR!!! Neither inSpeed or outSpeed is max!!!"

        return outSpd, inSpd, outRad, inRad, maxWind

    # Makes a Rankine Vortex wind speed grid that decreases exponentially
    # from the known values at known radii.  Inside the Radius of maximum
    # wind the wind decreases linearly toward the center
    def makeRankine(self, f, latGrid, lonGrid, pieSlices, radiiFactor, timeRange):
        st = time.time()
        rDict = f['radii']

        rDict = self.interpolateQuadrants(rDict, pieSlices)
        
        validTime = f['validTime']
        center = f['centerLocation']
        maxWind = f['maxWind']

        circleEA = CircleEA(latGrid, lonGrid, center, pieSlices)

        # make a list that contains the highest non-zero radius speed
        centerWindList = [0] * pieSlices
        for k in rDict.keys():
            for i in range(len(rDict[k])):
                if rDict[k][i] > 0 and k > centerWindList[i]:
                    centerWindList[i] = k


        for k in rDict.keys():
            if rDict[k] == [0] * pieSlices:
               del rDict[k]
        # make a list of lowest wind speed found with zero radius
        # and save the next lowest wind speed for later
        if rDict.has_key(100.0):
            speedList = [None, 100.0, 50.0, 34.0, 5.0]
        else:
            speedList = [None, 64.0, 50.0, 34.0, 5.0]

        zeroRadList = [999] * pieSlices
        validRadList = [999] * pieSlices
        for s in range(len(speedList) - 1):
            speed = speedList[s]
            nextSpeed = speedList[s + 1]
            if not rDict.has_key(speed):
                zeroRadList = [speed] * pieSlices
                validRadList = [nextSpeed] * pieSlices
            else:
                for i in range(len(rDict[speed])):
                    if rDict[speed][i] == 0:
                        zeroRadList[i] = speed
                        validRadList[i] = nextSpeed

        # get the distance grid and make sure it's never zero anywhere
        distanceGrid = circleEA.getDistanceGrid() / 1.852  # dist in NM
        distanceGrid[distanceGrid == 0] =  0.01

        # make a grid into which we will define the wind speeds
        grid = self.empty()

        # The cyclone algorithm depends on the forecast lead time
        fcstLeadTime = (validTime - self.baseDecodedTime) / 3600

        # add the radius for maxWind for interpolation
        if f.has_key('eyeDiameter'):
            eyeDiameter = f['eyeDiameter']
        else:
            print "Error --- no eye diameter found."
            eyeDiameter = None

        maxWindRadius = self.maxWindRadius(eyeDiameter)

        maxWindList = []
        # add the edited maxWind values, if any
        if f.has_key("editedMaxWinds"):
            # First interpolate based on pie slices
            maxWindList = self.interpWindMax(pieSlices, f["editedMaxWinds"])

        # Add in the maxWind and radius as a point
        if not rDict.has_key('maxWind'):
            rDict[maxWind] = [maxWindRadius] * pieSlices
        # extract the list and sort it
        wsList = rDict.keys()
        wsList.sort()
        
        # insert a dummy wind near the center and append so it's done last
        rDict[1] = [1] * pieSlices
        wsList.append(1.0)        

        # insert an artificial 5 knot radius at a distance proportional
        # to the 34 knot radius for that quadrant
        tenKnotRadiusList = [108.0] * pieSlices
        
        if rDict.has_key(34.0):
            tenKnotRadList = []
            radList34 = rDict[34.0]
            for r in radList34:
                tenKnotRadList.append(r * radiiFactor)
            
            # insert the 5 knot radius at the beginning so is made first
            rDict[5.0] = tenKnotRadList
            wsList.insert(0, 5.0)

        insideRMWMask = self.empty(bool)
        self._cycloneMask = self.empty(bool)
        # for each rDict record and quadrant, make the grid one piece at a time
        for i in range(len(wsList) - 1):
            self.lastRadius = [None] * pieSlices
            if not rDict.has_key(wsList[i]):
                continue
            radiusList = rDict[wsList[i]]
            nextRadiusList = rDict[wsList[i + 1]]

            maxRadius = maxWindRadius   # temp copy
            for quad in range(len(radiusList)):

                maxRadius = maxWindRadius   # temp copy
                maxWind = f['maxWind']   # reset maxWind as we may fiddle with it

                # fetch the speeds and radii we'll need
                outSpeed = float(wsList[i])
                inSpeed = float(wsList[i + 1])
                outRadius = float(radiusList[quad])
                inRadius = float(nextRadiusList[quad])
                
                # Here's where the speeds and radii are adjusted based
                # on the edited values but only if they have been edited
                # and only if we're working on the maxWind.
                if f.has_key("editedMaxWinds"):
                    if maxWind == wsList[i] or maxWind == wsList[i+1]:
                        outSpeed, inSpeed, outRadius, inRadius, maxWind = \
                            self.adjustMaxWind(outSpeed, inSpeed, outRadius,
                                               inRadius, maxWind, maxWindList,
                                               maxWindRadius, quad)

                # Some cases require we adjust the maxWindRadius
                if outSpeed in [64.0, 50.0, 34.0] and outRadius <= maxWindRadius:
                    inRadius = outRadius * 0.9
                    self.lastRadius[quad] = outRadius
                elif inSpeed == 1.0 and self.lastRadius[quad] is not None:
                    outRadius = self.lastRadius[quad] * 0.9
                    #print "Adjusting MaxWindRadius at:", inSpeed, "kts"
                    self.lastRadius[quad] = None

                # reset the speeds if they exceed the maxWind
                if fcstLeadTime <= 72 and zeroRadList[quad] is not None:
                    if inSpeed >= zeroRadList[quad]:
                        inSpeed = validRadList[quad]
                    if outSpeed >= zeroRadList[quad]:
                        outSpeed = validRadList[quad]

                # set the center value to max fcst wind
                if inSpeed == 1.0:
                    inSpeed = centerWindList[quad]

                # get the edit area for this quadrant
                mask = circleEA.getQuadrant(quad + 1, outRadius * 1.852)

                # log10 and exp math functions are fussy about zero
                if inSpeed == 0.0:
                    inSpeed = 0.1
                if outSpeed == 0.0:
                    outSpeed = 0.1
                if inRadius == 0.0:
                    inRadius = 0.1
                if outRadius == 0.0:
                    outRadius = 0.1
                # no wind speed can never exceed the maximum allowable wind speed
                if inSpeed > maxWind:
                    inSpeed = maxWind
                if outSpeed > maxWind:
                    outSpeed = maxWind

                # don't bother with trivial cases
                if inRadius < 2.0 and outRadius < 2.0:
                    continue
                if inRadius > outRadius:
                    continue

                if inSpeed == 0.0 or outSpeed == 0.0:
                    continue
                # calculate the exponent so that we exactly fit the next radius
                denom = log10(inRadius / outRadius)
                if denom == 0:
                    exponent = 1.0
                else: 
                    exponent = (log10(outSpeed) - log10(inSpeed)) / denom
                
                # make sure the exponent behaves itself
                if exponent > 10.0:
                    exponent = 10.0
                # inside RMW gets a linear slope to largest of max wind forecasts
                if inRadius <= 1.0:
                    dSdR = (outSpeed - inSpeed) / (outRadius - inRadius)
                    grid =  where(mask, inSpeed + (dSdR * distanceGrid), grid)
                    insideRMWMask[mask] = True
                else:  # outside RMW
                    grid = where(mask, inSpeed * power((inRadius / distanceGrid), exponent),
                             grid)
                if outSpeed >= 34.0 and inSpeed >= 34.0:
                    self._cycloneMask = logical_or(self._cycloneMask, mask)

        # Apply the NC State correction outside the RMW
        if self._applyNCSCorrection:
            corrGrid = self.makeCorrectionGrid(latGrid, lonGrid, center)
##            self.createGrid("Fcst", "NCSCorr", "SCALAR", corrGrid, self._timeRange,
##                    precision=3, minAllowedValue=-1.0, maxAllowedValue=1.0)

            m = logical_not(insideRMWMask)
            grid[m] *= (1 - corrGrid)[m]

        maxWind = f['maxWind']   # reset again before clipping

        dirGrid = self.makeDirectionGrid(latGrid, lonGrid, center[0], center[1])
        

        # clip values between zero and the maximum allowable wind speed
        maxWind = self.getMaxAllowableWind(maxWind)
        grid.clip(0.0, maxWind, grid)
        # apply the wind reduction over land
        fraction = 1.0 - (self.lessOverLand / 100.0)
        grid = self.decreaseWindOverLand(grid, fraction, self.elevation, timeRange)
        return (grid, dirGrid)

    def makeMaxWindGrid(self, interpFcstList, interval, latGrid, lonGrid, pieSlices,
                        radiiFactor):

##        if len(interpFcstList) == 0:
##            return

        startTime = interpFcstList[0]["validTime"]
        endTime = startTime + (123 * 3600)  # 123 hours later

        timeRange = self.makeTimeRange(startTime, endTime)
        
        # Used getGrids to calculate the maximum wind grid.
        #
        # Fetch the max of the wind grids just generated as this is very fast.
        maxWindGrid, maxDirGrid = self.getGrids("Fcst", "Wind", "SFC", timeRange, mode="Max")

        maxWindGrid = self.smoothGrid(maxWindGrid,3)

        self.createGrid("Fcst", "TCMMaxWindComposite", "SCALAR", maxWindGrid, timeRange,
                        precision=1, minAllowedValue=0.0, maxAllowedValue=200.0)
        
        # save the grid in the server
        self.saveObject("TCMMaxWindGrid", maxWindGrid, "WindGrid")
        
        return

    def validateCycloneForecast(self, fcstList, baseTime):
            
        # Now check each forecast to make sure that we have a radius for any
        # standard wind values less than the maxWind

        if len(fcstList) == 0:
            return False
        
        windValues = [64, 50, 34]
        for f in fcstList:
            for value in windValues:
                if value > f["maxWind"]:
                    continue
                if not f["radii"].has_key(value):
                    print f["radii"].keys(), "is missing value:", value
                    return False

        return True

    # Returns a dictionary that lists the min and max allowed wind for each hour
    def makeWindDict(self, fcstList):

        windDict = {}

        
        for f in fcstList:
            windValues = f["radii"].keys()
            hour = (f["validTime"] - self.baseDecodedTime) / 3600
            maxWind = f["maxWind"]
            minWind = 999999.0
            if len(f["radii"].keys()) == 0:
                minWind = 0.0

            # Grab the first (highest) forecast wind speed value
            if len(windValues) > 0:
                   minWind = windValues[0]
            else:
                minWind = 0.0
            
            windDict[hour] = (minWind, maxWind)

        return windDict

    # Pop up a GUI that will maxWind values for each quadrant and time
    def launchMaxWindGUI(self, fcstList):
        
        windDict = self.makeWindDict(fcstList)
        if AWIPS_ENVIRON == "AWIPS1":
            eaMgr = self.eaMgr()
        else:
            eaMgr = None

        self._maxWindGUI = DefineMaxWindGUI.DefineMaxWindGUI(self._dbss, eaMgr)

        newMaxWinds = self._maxWindGUI.displayGUI(windDict)

        if newMaxWinds is not None:
            
            hourList = newMaxWinds.keys()
            hourList.sort()

        self._maxWindGUI.cancelCommand()

        return newMaxWinds

    # Make the NCState bais correction grid based on the forecast.
    def makeCorrectionGrid(self, latGrid, lonGrid, center):


        # structure to hold the polynomial coefficients
        coeff = [[1.282e-011, -3.067e-008, 2.16e-005, -5.258e-003, 3.794e-001],
                 [3.768e-011, -4.729e-008, 2.097e-005, -3.904e-003, 2.722e-001],
                 [4.692e-011, -5.832e-008, 2.565e-005, -4.673e-003, 2.952e-001],
                 [3.869e-011, -4.486e-008, 1.84e-005, -3.331e-003, 2.738e-001]]

        # make the circle edit area and distance grid
        pieSlices = 4
        circleEA = CircleEA(latGrid, lonGrid, center, pieSlices)

        dist = circleEA.getDistanceGrid() # dist in km

        corrGrid = self.empty()

        for quad in range(pieSlices):

            ea = circleEA.getQuadrant(quad + 1, 500.0)
            grid = coeff[quad][0] * pow(dist, 4) + coeff[quad][1] * pow(dist, 3) + \
                   coeff[quad][2] * pow(dist, 2) + coeff[quad][3] * dist + \
                   coeff[quad][4]

            corrGrid = where(ea, grid, corrGrid)

        return corrGrid

    def execute(self, varDict, timeRange):
        
        RADII_FACTOR = 4.5
        
        self.setToolType("numeric")
        self.toolTimeRange = timeRange

        # define the default eye diameter for bulletins where they are missing
        eyeStr = varDict["Eye Diameter:"]
        self.dialogEyeDiameter = float(eyeStr)
        maxwindswath = varDict["MaxWind Swath for \nTCWindThreat?"]
        
        Topo = self.getTopo()

        tcDuration = self.getTimeConstraintDuration("Wind")
        tcHours = int(tcDuration / 3600)  # durations are expressed in seconds
        # set the time interpolation interval to the duration
        interval = tcHours

        # get the product ID
        productList1 = varDict["Product to\ndecode:"]
        productList2 = varDict["Product to\n decode:"]
        productList1 = productList1 + productList2  # concatenate
        if len(productList1) != 1:
            self.statusBarMsg("Please select one TCM bulletin only.", "S")
            return None

        productID = productList1[0]

        # get the ID for this site
        siteID = self.getSiteID()
            
        bgModelName = "Fcst"
        self.day3Radius = varDict["34 knot radius at 3 days (NM):"]
        self.day4Radius = varDict["34 knot radius at 4 days (NM):"]
        self.day5Radius = varDict["34 knot radius at 5 days (NM):"]

        # grab all of the background grids now before we make any new grids
        bgDict = self.getBackgroundGrids(bgModelName)

        # Radial slices hard-coded to 4.  Changing this will divide the wind
        # forecast into more radial pieces.  Recommended alternative values:
        # 12, 20, 36, 72.
        pieSlices = int(varDict["Number of Pie Slices?"])

        # define radii factor - may make this configurable
        # Multiply 3-5 day radius by this factor to get the zero radius.
        # Smaller values ramp the cyclone down to zero more quickly.


        self.lessOverLand = int(varDict["Decrease Wind over Land by (%):"])
        self.lessOverLandGrid = varDict["Constant Land\nReduction (Slider Bar)\nor Wind Reduction\nFactor Grid?"]
        self.elevation = Topo
        rclDecoder = TCMDecoder()
        tcmDecoder = TCMDecoder()

        msg = ""

        # Fetch the text product
        if productID == "preTCM":
            textProduct = self.getTextProductFromFile("/tmp/Wilma.txt")
            decoder = TCMDecoder()
            decoder.decodeTCMProduct(textProduct, self.dialogEyeDiameter)
            fcstList = decoder.getFcstList()
            baseTime = decoder.getBaseProductTime()
        #elif productID == "WRKTCM":
        #    textProduct = self.getTextProductFromFile("/data/local/research/TPCWindProb/WRKTCM")
        else:
            # try fetching the RCL first. 
            rclProductID = "MIARCL" + productID[3:]
            print "Attempting to Fetch rclProductID:", rclProductID
            rclTextProduct = self.getTextProductFromDB(rclProductID)
            completeFcst = False
            if len(rclTextProduct) < 5:
                #msg = rclProductID + " not found.  Using TCM to make cyclone."
                # self.statusBarMsg(msg, "S")
                rclBaseTime = 0
            else:
                rclDecoder.decodeTCMProduct(rclTextProduct, self.dialogEyeDiameter)
                rclFcstList = rclDecoder.getFcstList()
                rclBaseTime = rclDecoder.getBaseProductTime()
                completeFcst = self.validateCycloneForecast(rclFcstList, rclBaseTime)
                
            if productID[:3] == "PRE":
                productID = "MIA" + productID
                
            tcmTextProduct = self.getTextProductFromDB(productID)

            if len(tcmTextProduct) < 5:
                msg = productID + " could not be retrieved from the text database."
                self.statusBarMsg(msg, "S")
                return None   # Just return if no TCM is found.  Something's really wrong
            else:
                tcmDecoder.decodeTCMProduct(tcmTextProduct, self.dialogEyeDiameter)
                tcmFcstList = tcmDecoder.getFcstList()
                tcmBaseTime = tcmDecoder.getBaseProductTime()         

            #print "TCM and RCL Base Times are: ", tcmBaseTime, rclBaseTime
            if not completeFcst or rclBaseTime != tcmBaseTime:
                msg = "Problem decoding " + rclProductID + " Used TCM to make cyclone.\n"
                msg = msg + " Used GUI sliders for 3, 4, 5 day forecast."
                #self.statusBarMsg(msg, "S")
                fcstList = tcmFcstList
                baseTime = tcmBaseTime
            else:
                msg = "RCL message looked good so used that for TCM."
                fcstList = rclFcstList
                baseTime = rclBaseTime
                productID = rclProductID
        
        print "Decoded:", len(fcstList), " forecasts."

        # Set the baseDecodedTime - validTime of first entry - 3 hours
        if len(fcstList) > 0:
            self.baseDecodedTime = fcstList[0]['validTime'] - 3 * 3600

        if varDict["Define Asymmetrical \nMax Winds?"] == "Yes":

            newMaxWinds = self.launchMaxWindGUI(fcstList)
            for i in range(len(fcstList)):
                fcstHour = (fcstList[i]['validTime'] - baseTime) / 3600 + 3
                maxList = newMaxWinds[fcstHour]
                fcstList[i]["editedMaxWinds"] = maxList
                
        fcstList = self.extrapolateRadii(fcstList, baseTime, RADII_FACTOR)

##        # See if the decoded fcst is close to the current time.  This is needed
##        # so the tool will work on archived data sets (testMode)
##        testMode = False
##        if abs(time.time() - self.baseDecodedTime) > 2 * 24 * 3600:  # older than 2 days
##            testMode = True

        # restrict grids to the selected time period if option is selected.
        testMode = False
        restrictAnswer = varDict["Make Grids over \nSelected Time Only:"]
        if restrictAnswer == "Yes":
            testMode = True

        # Turn off testMode if the selected timeRange is less than an hour in duration
        if self.toolTimeRange.duration() < 3600:
            testMode = False

        # interpolate the wind forecasts we got from the decoder
        selectedStartTime = self.toolTimeRange.startTime().unixTime()
        selectedEndTime = self.toolTimeRange.endTime().unixTime()
        interpFcstList = []
        for i in range(len(fcstList) - 1):
            newFcstList = self.interpolateWindFcst(fcstList[i], fcstList[i+1],
                                                   interval)

            # Make sure the fcst is within the selected time range or we're in testMode
            for f in newFcstList:
                if (testMode and (f['validTime'] >= selectedStartTime and \
                                 f['validTime'] < selectedEndTime)) or (not testMode):
                    interpFcstList.append(f)
                    
        # append the very last forecast on to the end of the interpolated list
        if len(fcstList) > 0:
            if (testMode and (f['validTime'] >= selectedStartTime and \
                             f['validTime'] < selectedEndTime)) or (not testMode):
                interpFcstList.append(fcstList[-1])

        if len(fcstList) == 1:
            interpFcstList = fcstList

        if len(interpFcstList) == 0:
            self.statusBarMsg("No cyclone forecasts found within the Selected TimeRange",
                              "S")
        else:
            # If the wind grids are more than 3 hours long, the first grid ends up being double
            # duration.  So, add an extra duplicate forecast at the beginning and reset
            # the validTime
            print "tcHours:", tcHours
            if tcHours > 3: 
                interpFcstList.insert(0, copy.deepcopy(interpFcstList[0]))
                interpFcstList[0]["validTime"] = (int(interpFcstList[0]["validTime"] / tcDuration) \
                                                 * tcDuration)
                interpFcstList[1]["validTime"] = (int(interpFcstList[0]["validTime"] / tcDuration) \
                                                 * tcDuration) + tcDuration
                print "Adjusted time for first forecast"
            print "Generating", len(interpFcstList), "wind grids"

        # get the lat, lon grids
        latGrid, lonGrid = self.getLatLonGrids()

        self._applyNCSCorrection = False
        if varDict["Reduce Radii by 15% or \n NC State Bias Correction"] == "Reduce by 15%":
            # Reduce the extent of the wind radii per Mark De Maria's research
            # Loop through each wind radius and modify in place
            for f in interpFcstList:
                for windValue in f["radii"]:
                    for i in range(len(f["radii"][windValue])):
                        f["radii"][windValue][i] = f["radii"][windValue][i] * 0.85
        elif varDict["Reduce Radii by 15% or \n NC State Bias Correction"] == "NC State Bias Correction":
            self._applyNCSCorrection = True
       
        # make a grid for each interpolate forecast
        gridCount = 0
        for f in interpFcstList:

            self._timeRange = timeRange

            validTime = int(f['validTime'] / 3600) * 3600
            bgGrid = self.getClosestWindGrid(bgModelName, bgDict, validTime)
            startTime = validTime
            endTime = validTime + (interval * 3600)
            timeRange = self.makeTimeRange(startTime, endTime)
            self._cycloneTimeRange = timeRange

            t1 = time.time()
            windGrid = self.makeRankine(f, latGrid, lonGrid, pieSlices, RADII_FACTOR, timeRange)
            print "Time to makeRankine:", time.time() - t1

            magGrid, dirGrid = self.blendGrids(windGrid, bgGrid)
            magGrid = self.smoothGrid(magGrid, 5)
            dirGrid = self.smoothDirectionGrid(dirGrid, 5)

            name = "Wind"
            self.createGrid("Fcst", name, "VECTOR", (magGrid, dirGrid), timeRange,
                    descriptiveName=None, timeConstraints=None,
                    precision=1, minAllowedValue=0.0,
                    maxAllowedValue=200.0)
            
            gridCount = gridCount + 1
            print "TCMWindTool:", productID, "- Generated", gridCount, \
                  "out of", len(interpFcstList), "grids", \
                  time.asctime(time.gmtime(timeRange.startTime().unixTime()))

        # interpolate through forecast period to very high resolution and make
        # a composite maxWind grid from those wind grids
        if maxwindswath == "Yes":
            t1 = time.time()
            self.makeMaxWindGrid(interpFcstList, interval, latGrid, lonGrid, pieSlices,
                                 RADII_FACTOR)
            print time.time() - t1, "seconds to generate Max wind composite."

        if msg != "":
            self.statusBarMsg(msg, "S")

        return None
