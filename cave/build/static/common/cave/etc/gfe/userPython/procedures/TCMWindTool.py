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
# TCMWindTool
#
# Version 2.4   July 18, 2006
# Version 2.7.1   2 Sept 2010  Modified to Fix RCL error
# Version 2.7.2   01 Feb 2011  Fixed Pie Slice Algorithm/Added Backgroun Options
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
                 ["TCMCP1", "TCMCP2", "TCMCP3", "TCMCP4", "TCMCP5",
                  "TCPWP1", "TCPWP2", "TCPWP3", "TCPWP4", "TCPWP5"]),
                ("Background\nModel:", "Fcst", "radio", ["GFS40", "NAM12", "ECMWFHiRes", "Fcst"]),
                ("Number of Pie Slices?", "4", "radio", ["4", "8", "12", "16"]),
                ("Eye Diameter:", 0, "scale", [0, 100], 1),
                ("34 knot radius at 3 days (NM):", 100, "scale", [0, 1000], 10),
                ("34 knot radius at 4 days (NM):", 100, "scale", [0, 1000], 10),
                ("34 knot radius at 5 days (NM):", 100, "scale", [0, 1000], 10),
                ("Decrease Wind over Land by (%):", 10, "scale", [-20, 50], 1),
                ("Make Grids over Selected Time Only:", "No", "radio", ["Yes", "No"]),
                ("MaxWind Swath for TCWindThreat?", "No", "radio", ["Yes", "No"]),
                ]

import TimeRange
import AbsTime

import SmartScript

import string, time
from numpy import *


## For available commands, see SmartScript

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
        maxWindC = maxWind / 1.944  # convert to meters per second
        rmw = 46.29 * exp(-0.0153 * maxWindC + 0.0166 * lat)

        # convert to diameter and convert from km to nm
        ed = rmw * 2.0 / 1.852
        return ed

    def stripText(self):
        endStr = chr(13) + chr(13) + chr(10)
        for i in xrange(len(self.text)):
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
        self.pos += 1
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
            month += 1
            if month > 12:
                month = 1
                year += 1

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
        #print "I AM HERE AND AltFileName is: ", self.AltFileName
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
            for i in xrange(len(tokenList)):
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

        # Since we found it in the product, set the default diameter
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
            # Some problem occurred during the decoding process so return an empty fcst
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
            lon -= 360.0
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
            lon -= 360.0

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
        for quad in xrange(1, slices + 1):
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

    def printFcst(self, f, baseTime):
        print "=============================================================="
        print "Time:", time.asctime(time.gmtime(f['validTime'])),
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
        yesterday = self._gmtime() - (2 * 24 * 3600) # two days ago
        later = self._gmtime() + 10 * 24 * 3600  # 10 days from now
        allTimes = TimeRange.TimeRange(yesterday, later)
        parm = self.getParm(modelName, WEName, level);
        inv = parm.getGridInventory(allTimes.toJavaObj())
        trList = []
        for gd in inv:
            tr = TimeRange.TimeRange(gd.getGridTime())
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
            print "No background grids available...Using calm grid."
            return calmGrid

        minDiff = 3600 * 24 * 365  # just a large number
        gridIndex = -1
        tr = None

        # sort the keys by time so we get consistent behavior
        bgKeys = bgDict.keys()
        bgKeys.sort(self.timeRangeSort)
        
        # figure out which grid is closest in time
        for invTR in bgKeys:

            # if we have an exact match, we're done
            if invTR.contains(AbsTime.AbsTime(timeTarget)):
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

            for i in xrange(4):
                r1 = f1Radii[r][i]
                r2 = f2Radii[r][i]
                radius = r1 + (r2 - r1) * (newTime - t1) / (t2 - t1)
                quadList.append(radius)
            newRadii[r] = quadList

        return newRadii

    # interpolates the wind forecasts inbetween the two specified forecasts.
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
        fcstList = [f1]  # include the first fcst in the list
        for i in xrange(1, timeSlots):
            newTime = t1 + (i * intSecs)
            newLat = f1Lat + (i * dLat)
            newLon = f1Lon + (i * dLon)
            newEye = f1Eye + (i * dEye)
            newMaxWind = f1MaxWind + (i * dMaxWind)
            newRadii = self.interpRadii(t1, t2, newTime, f1Radii, f2Radii)
            f = {}
            f['centerLocation'] = (newLat, newLon)
            f['eyeDiameter'] = newEye
            f['validTime'] = newTime
            f['maxWind'] = newMaxWind
            f['radii'] = newRadii
            fcstList.append(f)

        return fcstList

    def calcRadiusList(self, maxWind, rmw, rad34, newRadii):
        for i in xrange(len(newRadii)):
            # linearly interpolate
            newRadii[i] = rmw + ((rmw - rad34) / (maxWind - 34.0)) / (64.0 - maxWind)
            if newRadii[i] < 0:
                newRadii[i] = 0
        return newRadii

    # This method fills in radii/wind values one way for the 36-72 hour period
    # and another way for the 72-120 hour period.  The concept is to add more
    # data values to the wind field so that the wind grids look more realistic.
    def extrapolateRadii(self, fcstList, baseDecodedTime, radiiFactor):
        for i in xrange(1, len(fcstList)):
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
                for i in xrange(len(prev50)):
                    if prev50[i] == 0:
                        continue

                    newRadii[i] = fcst50[i] / prev50[i] * prev64[i]

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
                    rmw /= 1.852   # convert to nautical miles

                for ws in [64.0, 50.0]:
                    newRadii = [0, 0, 0, 0]
                    if ws < maxWind:
                        newRadii = self.calcRadiusList(maxWind, rmw, extRadius, newRadii)
                        rDict[ws] = newRadii

                rDict[34.0] = [extRadius, extRadius, extRadius, extRadius]
                rDict[5.0] = [zeroRadius, zeroRadius, zeroRadius, zeroRadius]
                fcst['radii'] = rDict
#                print "From extrapolateRadii added rDict:", rDict

        return fcstList

    # Smooths the specified grid by the specified factor
    # With factor == 3, 3x3 smooth, factor == 5 5x5 smooth, etc.
    # Even factors (4, 6, 8,...) round up to the next odd value
    # If factors <3 are specified, the unmodified grid is returned.
    def smoothGrid(self, grid, factor):
        # factors of less than 3 are useless or dangerous
        if factor < 3:
            return grid
        st = time.time()
        half = int(factor)/ 2
        sg = zeros(grid.shape,float64)
        count = zeros(grid.shape,float64)
        gridOfOnes = ones(grid.shape,float64)
        for y in xrange(-half, half + 1):
            for x in xrange(-half, half + 1):
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
                sg[target] += grid[src]
                count[target] += gridOfOnes[src]
        return sg / count

    # Smooths the direction grid without regard to the magnitude
    def smoothDirectionGrid(self, dir, factor):
        mag = ones(dir.shape, float)  # 1.0 everywhere
        u, v = self.MagDirToUV(mag, dir)
        u = self.smoothGrid(u, factor)
        v = self.smoothGrid(v, factor)
        mag, dir = self.UVToMagDir(u, v)
        return dir

    def makeWindGrid(self, mag, direct, gridShape):
        mag = ones(gridShape, float) * mag
        direct = ones(gridShape, float) * direct
        return mag, direct

    def decreaseWindOverLand(self, grid, fraction, Topo):
        mask = greater(Topo, 0.0)
        
        # If area over which you desire to apply land correction you prefer be
        # based on Edit Are instead of areas with Topo greater than zero then
        # uncomment the next two lines and specify Edit Area to use.
        
        #editArea = self.getEditArea("LAND_EDIT_AREA_NAME_HERE")
        #mask = self.encodeEditArea(editArea)
        gridC = where(mask, grid * fraction, grid)
        return gridC

    def getTimeConstraintDuration(self, element):
        return self.getParm("Fcst", element, "SFC").getGridInfo()\
                .getTimeConstraints().getDuration()

    # returns the maximum allowable wind speed based on NWS directives
    def getMaxAllowableWind(self, maxWind):
        parm = self.getParm("Fcst", "Wind", "SFC")
        maxAllowable = parm.getGridInfo().getMaxValue()
        return min(maxWind, maxAllowable)

    # returns an interpolated radius based on input radii
    def getOutlookRadius(self, leadTime):
        leadTimeList = [72, 96, 120]
        radiusList = [self.day3Radius, self.day4Radius, self.day5Radius]

        if leadTime < leadTimeList[0]:
            return radiusList[0]

        for i in xrange(1, len(leadTimeList)):
            if leadTime < leadTimeList[i]:
                dt = leadTimeList[i] - leadTimeList[i - 1]
                dr = radiusList[i] - radiusList[i - 1]
                return radiusList[i - 1] + (leadTime - leadTimeList[i - 1]) * dr / dt

        return radiusList[-1]  # return the last item

    #  Blends the specified grid together
    def blendGrids(self, windGrid, bgGrid):

        # make a mask around the edge
        windMag = windGrid[0]
        backMag = bgGrid[0]
        mag = windMag.copy()

        # if we have a calm cyclone, return the background grid
        windMask = greater(mag, 1.0)
        if sum(sum(windMask)) == 0:
            return bgGrid

        # now check the background grid
        bgMask = greater(backMag, 1.0)
        if sum(sum(bgMask)) == 0:
            return windGrid

        # make a weightingGrid
        upper = 33.9   # start blending at this mag
        # stop blending at the average background mag
        lower = sum(sum(backMag)) / sum(sum(ones(backMag.shape)))
        if lower >= upper:
            print "Problem calculating lower and upper ring thresholds."
            print "lower = ", lower, "upper = ", upper
            return bgGrid

        # calculate the average value over the area where blending will occur
        ringMask = logical_and(less_equal(mag, upper), greater_equal(mag, lower))

        ringMaskSum = sum(sum(ringMask))
        if ringMaskSum == 0:
            print "Problem calculating ringMask.  No blending for this grid."
            windMag = self.smoothGrid(windMag, 9)
            ringMask = logical_and(less_equal(mag, upper), greater_equal(mag, lower))
            windMask = greater(windMag, 5.0)
            magGrid = where(windMask, windMag, backMag)
            dirGrid = where(windMask, windGrid[1], bgGrid[1])
            return (magGrid, dirGrid)

        avgGrid = where(ringMask, backMag, 0.0)
        lower = sum(sum(avgGrid)) / sum(sum(ringMask))

        # a nearly calm grid means no blending required so return the cyclone
        if lower < 1.0:
            return windGrid

        wtGrid = where(greater(mag, upper), 1.0, 0.0)
        ringMask = logical_and(less(mag, upper), greater(mag, lower))
        wtGrid = where(ringMask, (mag - lower) / (upper - lower), wtGrid)
        wtGrid[less(mag, lower)] = 0.0
        wtGrid = self.smoothGrid(wtGrid, 5)

        # calculate the new mag grid
        mag *= wtGrid
        mag += backMag * (1 - wtGrid)

        # calculate direction grid
        onesGrid = ones(mag.shape)
        gridU, gridV = self.MagDirToUV(onesGrid, windGrid[1])
        bgU, bgV = self.MagDirToUV(onesGrid, bgGrid[1])
        gridU *= wtGrid
        gridU += bgU * (1 - wtGrid)
        gridV *= wtGrid
        gridV += bgV * (1 - wtGrid)

        # get the dirGrid and toss out the magnitude
        magGrid, dirGrid = self.UVToMagDir(gridU, gridV)

        return mag, dirGrid

    def getLatLonGrids(self):
        # Try to get them from the fcst database to save time
        startTime = AbsTime.current() - 86400
        endTime = AbsTime.current() + 86400 # 1 day
        timeRange = TimeRange.TimeRange(startTime, endTime)
        latGrid = self.getGrids("Fcst", "latGrid", "SFC", timeRange,
                                mode = "First", noDataError = 0)
        lonGrid = self.getGrids("Fcst", "lonGrid", "SFC", timeRange,
                                mode = "First", noDataError = 0)
        if latGrid != None and lonGrid != None:
            return latGrid, lonGrid

        # make the latGird and lonGrid
        latGrid, lonGrid = SmartScript.SmartScript.getLatLonGrids(self)
        
        # Temporarliy save them in the forecast database
        startTime = AbsTime.current()
        endTime = AbsTime.current() + 86400 * 7  # 7 days
        timeRange = TimeRange.TimeRange(startTime, endTime)
        self.createGrid("Fcst", "latGrid", "SCALAR", latGrid, timeRange,
                        descriptiveName=None, timeConstraints=None,
                        precision=1, minAllowedValue=0.0,
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
            for i in xrange(-1, len(rList) -1):
                minVal = rList[i]
                maxVal = rList[i + 1]
                dVal = (maxVal - minVal) / interpFactor
                for f in xrange(interpFactor):
                    radius = minVal + dVal * f
                    # make sure we never exceed the forecast radius
##                    if radius > minVal:
##                        radius = minVal
                    newList.append(radius)

            # Since we started with the NW quadrant we need to shift
            # the list so that it starts at North to conform to convention
            shift = int(pieSlices / 4)
            shiftedList = newList[shift:]
            shiftedList += newList[:shift]
            newDict[k] = shiftedList
        return newDict

    # fetches and returns all of the wind grids specified by the model
    # name.  Should be called before any new wind grids are created
    def getBackgroundGrids(self, modelName):
        bgDict = {}

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
        gTime = time.gmtime(baseTime)
        return time.strftime("%Y%m%d%H", gTime)
    
    # returns the index corresponding to the specified timeStr and fcstHour
    def findFcst(self, fcstList, fcstHour):
        for i in xrange(len(fcstList)):
            validTime = fcstList[i]["validTime"]
            leadTime = (validTime - self.baseDecodedTime) / 3600
            if fcstHour == leadTime:
                return i

        return None    

    # Makes a Rankine Vortex wind speed grid that decreases exponentially
    # from the known values at known radii.  Inside the Radius of maximum
    # wind the wind decreases linearly toward the center
    def makeRankine(self, f, latGrid, lonGrid, pieSlices, radiiFactor):
        st = time.time()
        grid = zeros(latGrid.shape)
        rDict = f['radii']
        
##        print "rDict before interpolating Quads:"
##        for r in rDict:
##            print rDict[r]

        rDict = self.interpolateQuadrants(rDict, pieSlices)

##        print "rDict after interpolating Quads:"
##        for r in rDict:
##            print rDict[r]
##
##        return (None, None)        
        
        validTime = f['validTime']
        center = f['centerLocation']
        maxWind = f['maxWind']
        circleEA = CircleEA(latGrid, lonGrid, center, pieSlices)

        # make a list that contains the highest non-zero radius speed
#        centerWindList = [0, 0, 0, 0]
        centerWindList = [0] * pieSlices
        for k in rDict.keys():
            for i in xrange(len(rDict[k])):
                if rDict[k][i] > 0 and k > centerWindList[i]:
                    centerWindList[i] = k

        for k in rDict.keys():
#            if rDict[k] == [0, 0, 0, 0]:
            if rDict[k] == [0] * pieSlices:
                del rDict[k]

        # make a list of lowest wind speed found with zero radius
        # and save the next lowest wind speed for later
        if rDict.has_key(100.0):
            speedList = [None, 100.0, 50.0, 34.0, 5.0]
        else:
            speedList = [None, 64.0, 50.0, 34.0, 5.0]

#        zeroRadList = [999, 999, 999, 999]
#        validRadList = [999, 999, 999, 999]
        zeroRadList = [999] * pieSlices
        validRadList = [999] * pieSlices
        for s in xrange(len(speedList) - 1):
            speed = speedList[s]
            nextSpeed = speedList[s + 1]
            if not rDict.has_key(speed):
#                zeroRadList = [speed, speed, speed, speed]
#                validRadList = [nextSpeed, nextSpeed, nextSpeed, nextSpeed]
                zeroRadList = [speed] * pieSlices
                validRadList = [nextSpeed] * pieSlices
            else:
                for i in xrange(len(rDict[speed])):
                    if rDict[speed][i] == 0:
                        zeroRadList[i] = speed
                        validRadList[i] = nextSpeed

        # get the distance grid and make sure it's never zero anywhere
        distanceGrid = circleEA.getDistanceGrid() / 1.852  # dist in NM
        distanceGrid[equal(distanceGrid, 0)] = 0.01

        # make a grid into which we will define the wind speeds
        grid = zeros(latGrid.shape, float)

        # The cyclone algorithm depends on the forecast lead time
        fcstLeadTime = (validTime - self.baseDecodedTime) / 3600
        # add the radius for maxWind for interpolation
        if f.has_key('eyeDiameter'):
            maxRad = f['eyeDiameter'] / 2.0 + 8.0
        else:
            print "Error --- no eye diameter found."
            maxRad = 12.5 # half of default 25 nm eye diameter

#        rDict[maxWind] = [maxRad, maxRad, maxRad, maxRad]
        if not rDict.has_key('maxWind'):
            rDict[maxWind] = [maxRad] * pieSlices

        # make sure no wind exceeds the maximum allowable windspeed
        # remove any rDict entries that are higher than this

        # extract the list and sort it
        wsList = rDict.keys()
        wsList.sort()

        # insert a dummy wind near the center and append so it's done last
#        rDict[1] = [1, 1, 1, 1]
        rDict[1] = [1] * pieSlices
        wsList.append(1.0)

        # insert an artificial 5 knot radius at a distance proportional
        # to the 34 knot radius for that quadrant
#        tenKnotRadiusList = [108.0, 108.0, 108.0, 108.0]  # 200 km
        tenKnotRadiusList = [108.0] * pieSlices
        if rDict.has_key(34.0):
            tenKnotRadList = []
            radList34 = rDict[34.0]
            for r in radList34:
                tenKnotRadList.append(r * radiiFactor)

            # insert the 10 knot radius at the beginning so is made first
            rDict[5.0] = tenKnotRadList
            wsList.insert(0, 5.0)

        # for each rDict record and quadrant, make the grid one piece at a time
        for i in xrange(len(wsList) - 1):
            if not rDict.has_key(wsList[i]):
                continue
            radiusList = rDict[wsList[i]]
            nextRadiusList = rDict[wsList[i + 1]]
            for quad in xrange(len(radiusList)):
                outSpeed = float(wsList[i])
                inSpeed = float(wsList[i + 1])
                outRadius = float(radiusList[quad])
                inRadius = float(nextRadiusList[quad])
                # reset the speeds if they exceed the max non-zero wind
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
                else:  # outside RMW
                    grid = where(mask, inSpeed * power((inRadius / distanceGrid), exponent),
                             grid)
#                    grid = clip(grid, 0.0, 200.0)
        dirGrid = self.makeDirectionGrid(latGrid, lonGrid, center[0], center[1])

        # clip values between zero and the maximum allowable wind speed
        maxWind = self.getMaxAllowableWind(maxWind)
        grid = clip(grid, 0.0, maxWind)
        # apply the wind reduction over land
        fraction = 1.0 - (self.lessOverLand / 100.0)
        grid = self.decreaseWindOverLand(grid, fraction, self.elevation)

        return (grid, dirGrid)

    def makeMaxWindGrid(self, interpFcstList, interval, latGrid, lonGrid, pieSlices,
                        radiiFactor):

##        if len(interpFcstList) == 0:
##            return

        maxWindGrid = zeros_like(self.getTopo())
        
        startTime = interpFcstList[0]["validTime"]
        endTime = startTime + (123 * 3600)  # 123 hours later

        fcstIndex = 0
        while fcstIndex < len(interpFcstList)-1:
            f1 = interpFcstList[fcstIndex]
            f2 = interpFcstList[fcstIndex + 1]
            
            f1Time = interpFcstList[fcstIndex]['validTime']
            f2Time = interpFcstList[fcstIndex + 1]['validTime']

            print "Interpolating from:", time.asctime(time.gmtime(f1Time)), \
                "to", time.asctime(time.gmtime(f2Time))

            lat1, lon1 = f1['centerLocation']
            lat2, lon2 = f2['centerLocation']

            x1, y1 = self.getGridCell(lat1, lon1)
            x2, y2 = self.getGridCell(lat2, lon2)

            if x1 is None or y1 is None or x2 is None or y2 is None:
                fcstIndex += 1
                continue

            dx = abs(x2 - x1)
            dy = abs(y2 - y1)

            dmax = max(dx, dy)

            newInterval = float(interval) / dmax

            newFcstList = self.interpolateWindFcst(f1, f2, newInterval)

            print "Created", len(newFcstList), "new wind forecasts."

            for f in newFcstList:
                mag, dir = self.makeRankine(f, latGrid, lonGrid, pieSlices, radiiFactor)

                maxWindGrid = where(greater(mag, maxWindGrid), mag, maxWindGrid)
                
            fcstIndex += 1
            
            if f2["validTime"] >= endTime:
                break

        start = AbsTime.AbsTime(startTime)
        end = AbsTime.AbsTime(endTime)
        timeRange = TimeRange.TimeRange(start, end)

        maxWindGrid = self.smoothGrid(maxWindGrid, 3)

        self.createGrid("Fcst", "TCMMaxWindComposite", "SCALAR", maxWindGrid, timeRange,
                        precision=1, minAllowedValue=0.0, maxAllowedValue=200.0)
        
        # save the grid in the server
        self.saveObject("TCMMaxWindGrid", maxWindGrid, "WindGrid")
        
        return


    def validateCycloneForecast(self, fcstList, baseTime):
        print "Validating Forecasts:"
##
##     This section is overly restrictive, so it is commented out pending
##     better logic that compares the TCM to the RCL forecasts
##  
##        leadTimeList = [12, 24, 36, 48, 72, 96, 120]
##        fcstHourList = []
##        for f in fcstList:
##            # calc the leadTime in hours
##            leadTime = (f['validTime'] - baseTime) / 3600 + 3
##            fcstHourList.append(int(leadTime))
##
##        # Make sure all of the forecasts are there
##        for leadTime in leadTimeList:
##            if leadTime not in fcstHourList:
##                return False
            
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

    def execute(self, varDict, timeRange):
        self.setToolType("numeric")
        self.toolTimeRange = timeRange

        # define the default eye diameter for bulletins where they are missing
        eyeStr = varDict["Eye Diameter:"]
        self.dialogEyeDiameter = float(eyeStr)
        maxwindswath = varDict["MaxWind Swath for TCWindThreat?"]

        Topo = self.getTopo()

        tcDuration = self.getTimeConstraintDuration("Wind")
        tcHours = int(tcDuration / 3600)  # durations are expressed in seconds
        # set the time interpolation interval to the duration
        interval = tcHours

        # get the product ID
        productList1 = varDict["Product to\ndecode:"]
        productList2 = varDict["Product to\n decode:"]
        productList1C = productList1 + productList2  # concatenate
        if len(productList1C) != 1:
            self.statusBarMsg("Please select one TCM bulletin only.", "S")
            return None

        productID = productList1C[0]

        # get the ID for this site
        siteID = self.getSiteID()

        bgModelName = varDict["Background\nModel:"]
        self.day3Radius = varDict["34 knot radius at 3 days (NM):"]
        self.day4Radius = varDict["34 knot radius at 4 days (NM):"]
        self.day5Radius = varDict["34 knot radius at 5 days (NM):"]

        # grab all of the background grids now before we make any new grids
        bgDict = self.getBackgroundGrids(bgModelName)

        # Radial slices hard-coded to 4.  Changing this will divide the wind
        # forecast into more radial pieces.  Recommended alternative values:
        # 12, 20, 36, 72.
        pieSlices = int(varDict["Number of Pie Slices?"])
        print "Number of Pie Slices: ", pieSlices
        # pieSlices = 8

        # define radii factor - may make this configurable
        radiiFactor = 1.5

        self.lessOverLand = int(varDict["Decrease Wind over Land by (%):"])
        self.elevation = Topo
        
        rclDecoder = TCMDecoder()
        tcmDecoder = TCMDecoder()

        msg = ""

#        # Use this method to fetch a product from the text database
        if productID == "preTCM":
            textProduct = self.getTextProductFromFile("/tmp/Irene.txt")
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

            tcmTextProduct = self.getTextProductFromDB(productID)
            if len(tcmTextProduct) < 5:
                msg = productID + " could not be retrieved from the text database."
                self.statusBarMsg(msg, "S")
                return None   # Just return if no TCM is found.  Something's really wrong
            else:
                tcmDecoder.decodeTCMProduct(tcmTextProduct, self.dialogEyeDiameter)
                tcmFcstList = tcmDecoder.getFcstList()
                tcmBaseTime = tcmDecoder.getBaseProductTime()         

            print "TCM and RCL Base Times are: ", tcmBaseTime, rclBaseTime
            if not completeFcst or rclBaseTime != tcmBaseTime:
                msg = "Problem decoding " + rclProductID + " Used TCM to make cyclone.\n"
                msg += " Used GUI sliders for 3, 4, 5 day forecast."
                #self.statusBarMsg(msg, "S")
                fcstList = tcmFcstList
                baseTime = tcmBaseTime
            else:
                msg = "RCL message looked good so used that for TCM."
                fcstList = rclFcstList
                baseTime = rclBaseTime
                productID = rclProductID

##        # Use this method to fetch a text product from a file
##        textProduct = self.getTextProductFromFile("/tmp/Irene.txt")
##
##        decoder = TCMDecoder()
##        decoder.decodeTCMProduct(textProduct, self.dialogEyeDiameter)
##        fcstList = decoder.getFcstList()
##        baseTime = decoder.getBaseProductTime()

        print "Decoded:", len(fcstList), " forecasts."

        # Set the baseDecodedTime - validTime of first entry - 3 hours
        if len(fcstList) > 0:
            self.baseDecodedTime = fcstList[0]['validTime'] - 3 * 3600
            
        fcstList = self.extrapolateRadii(fcstList, baseTime, radiiFactor)

        # See if the decoded fcst is close to the current time.  This is needed
        # so the tool will work on archived data sets (testMode)
        testMode = 0
        if abs(time.time() - self.baseDecodedTime) > 2 * 24 * 3600:  # older than 2 days
            testMode = 1
        # restrict grids to the selected time period if option is selected.
        restrictAnswer = varDict["Make Grids over Selected Time Only:"]
        if restrictAnswer == "Yes":
            testMode = 1

        # Turn off testMode if the selected timeRange is less than an hour in duration
        if self.toolTimeRange.duration() < 3600:
            testMode = 0

        # interpolate the wind forecasts we got from the decoder
        print "Interpolating wind forecasts:"
        selectedStartTime = self.toolTimeRange.startTime().unixTime()
        selectedEndTime = self.toolTimeRange.endTime().unixTime()

        interpFcstList = []
        for i in xrange(len(fcstList) - 1):
            newFcstList = self.interpolateWindFcst(fcstList[i], fcstList[i+1],
                                                   interval)
            # Make sure the fcst is within the selected time range or we're in testMode
            for f in newFcstList:
                if (testMode and (f['validTime'] >= selectedStartTime and \
                                 f['validTime'] < selectedEndTime)) or testMode == 0:
                    interpFcstList.append(f)

        if len(fcstList) == 1:
            interpFcstList = fcstList

        if len(interpFcstList) == 0:
            self.statusBarMsg("No cyclone forecasts found within the Selected TimeRange",
                              "S")
        else:
            print "Generating", len(interpFcstList), "wind grids"
        
        # get the lat, lon grids
        latGrid, lonGrid = self.getLatLonGrids()
        
        # interpolate through forecast period to very high resolution and make
        # a composite maxWind grid from those wind grids
        if maxwindswath is "Yes":
            t1 = time.time()
            self.makeMaxWindGrid(interpFcstList, interval, latGrid, lonGrid, pieSlices,
                                 radiiFactor)
            print time.time() - t1, "seconds to generate Max wind composite."

        # make a grid for each interpolate forecast
        gridCount = 0
        for f in interpFcstList:
            t1 = time.time()
            windGrid = self.makeRankine(f, latGrid, lonGrid, pieSlices, radiiFactor)
            print "time to makeRankine:", time.time() - t1

            validTime = int(f['validTime'] / 3600) * 3600
            bgGrid = self.getClosestWindGrid(bgModelName, bgDict, validTime)

            start = AbsTime.AbsTime(int(validTime))
            timeRange = TimeRange.TimeRange(start, start + interval * 3600)

            grid = self.blendGrids(windGrid, bgGrid)

            name = "Wind"
            self.createGrid("Fcst", name, "VECTOR", grid, timeRange,
                    descriptiveName=None, timeConstraints=None,
                    precision=1, minAllowedValue=0.0,
                    maxAllowedValue=200.0)

            gridCount += 1
            print "TCMWindTool:", productID, "- Generated",gridCount, \
                  "out of", len(interpFcstList), "grids", time.asctime(time.gmtime(timeRange.startTime().unixTime()))

        if msg != "":
            self.statusBarMsg(msg, "S")
        
        return None


