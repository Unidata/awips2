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
# GenerateCyclone
#
# Author: lefebvre
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Populate"]

VariableList = [("ProductID:", "", "alphaNumeric"),
                ("Background\nModel:", "Fcst", "radio", ["GFS80", "NAM12", "Fcst"]),
                ("Number of\n Pie Slices:", "20", "radio", ["4", "12", "20", "36", "72"]),
#                ("Time Interval\n(hours):", "1", "radio", ["1", "3", "6", "12"]),
                ("Make Grids over\nSelected Time Only:", "No", "radio", ["Yes", "No"]),
                ("Decrease Wind over Land by (%):", 0, "scale", [-20, 50], 1),
                ]

import TimeRange
import AbsTime

import SmartScript

import string, time
import Exceptions

from numpy import *


## For available commands, see SmartScript

class TCMDecoder:
    def __init__(self):
        self.pos = 0
                            # key words in TCM products from NCEP
        self.keyWordDict = {"FORECAST VALID" : self.decodeWindForecast,
                            "TPC/NATIONAL HURRICANE CENTER" : self.decodeAltFilename,
                            "CENTER LOCATED NEAR" : self.decodeCenterLocation,
                            "CENTER LOCATED INLAND NEAR" : self.decodeCenterLocation,
                            "MAX SUSTAINED WINDS" : self.decodeMaxSustainedWinds,
                            "MAX WIND" : self.decodeMaxWind,
                            "EYE DIAMETER" : self.decodeEyeDiameter,
                            "KT..." : self.decodeRadii,
                            # key words for JTWC products
                            "WTPN" : self.decodeJTWCProductTime,
                            "WARNING POSITION:" : self.decodeJTWCTimeCenter,
                            "VALID AT:" : self.decodeJTWCWindForecast,
                            "RADIUS OF" : self.decodeJTWCRadii,
                            "    ---" : self.endJTWCWindForecast,
                            "REMARKS:" : self.stopDecodingJTWC,
                            }

        self.fcstList = []  # a place to store all of the forecasts

        self.text = []  #  the text product

        self.currentFcst = {}  # the current forecast we are docoding

        self.baseProductTime = 0

        self.foundEyeDiameter = 0

        self.altFilename = ""

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

    def getAltInfoFilename(self):
        return self.altFilename

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
        # timeStr format: "HHMMZ DAY MON DD YYYY"

        # extract time parts from the str
        strList = string.split(timeStr)
        if len(strList) != 5:
            print "Invalid time string:", timeStr
            print "Format should be of the form HHMMZ DAY MON DD YYYY"
            return

        hour = int(timeStr[0:2])
        minute = int(timeStr[2:4])
        monthStr = strList[2]
        month = self.monthNum(monthStr)
        day = int(strList[3])
        year = int(strList[4])

        # time.mktime returns time in seconds but in local time
        baseTime = time.mktime((year, month, day, hour, minute, 0, 0, 0, 0))

        # Adjust to UTC
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

    def decodeAltFilename(self):
        nameStr = self.currentLine()
        parts = string.split(nameStr)

        self.altFilename = parts[-1]  # grab the last string token
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
            else:
                print "Invalid Center Location string:", locStr
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
            print "No Max Sustained Winds or Gusts found."
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
        if self.currentFcst == {}:   # can't continue
            return

        str = self.currentLine()
        str = string.replace(str, '.', ' ')  # remove ...
        tokenList = string.split(str)
        # check for KT in the second slot
        if len(tokenList) < 4 or tokenList[1] != "KT":
            print "Invalid TCM wind string:", str
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


        if len(radiusList) == 0:
            print "Error decoding radii in string:", str
        # store the radii info
        if not self.currentFcst.has_key("radii"):
            self.currentFcst['radii'] = {}

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
        if self.baseProductTime == 0:
            self.baseProductTime = validTime
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
        while self.pos < len(TCMProduct):
            line = self.currentLine()
            for k in self.keyWordDict.keys():
                if string.find(line, k) > -1:
                    self.keyWordDict[k]()
                    break
            self.pos += 1

        # store the last forecast in the list of forecasts
        if self.currentFcst != {}:
            self.fcstList.append(self.currentFcst)
            self.currentFcst = {}  # reset

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
        if len(tokenList) >= 5:
            dateTimeStr = tokenList[0][0:6]
            latStr = tokenList[3]
            lonStr = tokenList[4]
        else:
            print "Error decoding JTWC Time/Center string:", line
            print "Format should be: DDHHMMZx --- NEAR Lat Lon"
            return

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
        return

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

        self.fcstWindGrids = {}

    # Use this method if you want to get your product
    # from a simple text file
    def getTextProductFromFile(self, filename):
        try:
            f = file(filename, 'r')
        except:
            print filename, "not found when getting product from file."
            return []
        textList = []
        line = f.readline()
        textList.append(line)
        while line != "":
            line = f.readline()
            textList.append(line)
        f.close()

        return textList

    # Reads decodes depression information using the specified product.
    def decodeDepressionInfo(self, textProduct):

        for line in textProduct:
            parts = string.split(line, ",")
            if len(parts) < 20:
                continue
            if parts[4] == " CARQ" and parts[5] == "   0":
                outsideRad = int(parts[18])
                rmw = int(parts[19])
                return (rmw, outsideRad)

        return (0, 0)

    def injectDepressionInfo(self, fcstList, rmw, outsideRad):
        if rmw == 0 and outsideRad == 0:
            return fcstList

        eyeDiam = (rmw + 8.0) * 2.0
        for f in fcstList:
            f['eyeDiameter'] = eyeDiam
            # add the maxWind radius
            maxWind = f['maxWind']
            maxWindRadii = [rmw, rmw, rmw, rmw]
            f['radii'][maxWind] = maxWindRadii

            # Make an arbitrary radius at the outer most isobar
            outSpeed = maxWind / 3.0   # somewhat arbitrary
            f['radii'][outSpeed] = [outsideRad, outsideRad, outsideRad, outsideRad]

        return fcstList

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

    # returns a wind grid from the specified model most closely matched in
    # time
    def getClosestWindGrid(self, modelName, timeTarget):
        t1 = AbsTime.AbsTime(0)
        t2 = AbsTime.current() + 300 * 24 * 3600  # 300 days out
        timeRange = TimeRange.TimeRange(t1, t2)
        siteID = self.getSiteID()
        if modelName == "Fcst":
            level = "SFC"
            elementName = "Wind"
        else:
            modelName = siteID + "_D2D_" + modelName
            level = "FHAG10"
            elementName = "wind"

        topo = self.getTopo()
        calmGrid = self.makeWindGrid(0.0, 0.0, topo.shape)
        gridInfo = []
        try:
            gridInfo = self.getGridInfo(modelName, elementName, level, timeRange)
        except Exceptions.EditActionError:
            print "No grids found for model/level:", modelName, level
            if string.find(modelName, "GFS") >= 0:
                modelName = siteID + "_D2D_" + "AVN"
                level = "BL030"
                try:
                    gridInfo = self.getGridInfo(modelName, elementName, level, timeRange)
                except Exceptions.EditActionError:
                    print "No grids found for model", modelName, "level:", level
                    print "Using calm grid."
                    return calmGrid

        if len(gridInfo) == 0:
            print "No grid info found for:", modelName, "at:", timeRange
            print "No grid info...Using calm grid."
            return calmGrid

        minDiff = 3600 * 24 * 365  # just a large number
        gridIndex = -1
        tr = None
        # figure out which grid is closest in time
        for i in xrange(len(gridInfo)):
            gridTime = gridInfo[i].gridTime()
            gTime = gridTime.startTime().unixTime()
            diff = abs(gTime - timeTarget)
            if diff < minDiff:
                tr = gridInfo[i].gridTime()
                minDiff = diff
                if diff == 0:
                    break

        if minDiff > 3 * 3600:
            print "Returning calm grid as background."
            return calmGrid

        grid = calmGrid
        # fetch the grid
        if modelName == "Fcst":
            if self.fcstWindGrids.has_key(tr):
                grid = self.fcstWindGrids[tr]
            else:
                # hunt down any grid that overlaps the timeTarget
                for gridTR in self.fcstWindGrids.keys():
                    if gridTR.contains(AbsTime.AbsTime(timeTarget)):
                        grid = self.fcstWindGrids[gridTR]
        else:
            grid = self.getGrids(modelName, elementName, level, tr, mode="First")
            grid = (grid[0] * 1.944, grid[1])

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
        # set the list of radii based on the first set: f1Radii
        radiiList = f1Radii

        newRadii = {}
        for r in radiiList:
            quadList = []
            for i in xrange(4):   # always and only 4 quadrants at this point
                r1 = f1Radii[r][i]
                if f2Radii.has_key(r):
                    r2 = f2Radii[r][i]
                else:
                    msg = "Wind forecast missing wind value: " + str(r) + " knots.  "
                    msg += "Recommend defining wind radii for " + str(r) + " knots."
                    ##self.statusBarMsg(msg, "S")
                    r2 = r1  # just use the f1 value so we can keep going
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


    # Smooths the direction grid without regard to the magnitude
    def smoothDirectionGrid(self, dir, factor):
        mag = ones(dir.shape, float)  # 1.0 everywhere
        u, v = self.MagDirToUV(mag, dir)
        u = self.smoothGrid(u, factor)
        v = self.smoothGrid(v, factor)
        mag, dir = self.UVToMagDir(u, v)
        return dir

    def makeWindGrid(self, mag, dir, gridShape):
        mag = ones(gridShape, float) * mag
        dir = ones(gridShape, float) * dir
        return mag, dir

    def decreaseWindOverLand(self, grid, fraction, Topo):
        mask = greater(Topo, 0.0)
        grid = where(mask, grid * fraction, grid)
        return grid

    def getTimeConstraintDuration(self, element):
        return self.getParm("Fcst", element, "SFC").getGridInfo()\
               .getTimeConstraints().getDuration()

    #  Blends the specified grid together
    def blendGrids(self, windGrid, bgGrid):

        # make a mask around the edge
        windMag = windGrid[0]
        backMag = bgGrid[0]
        mag = windMag.copy()

        # make a weightingGrid
        lower = sum(sum(backMag)) / sum(sum(ones(backMag.shape)))
        # calculate the average value over the area where blending will occur

        upper = lower + 10.0

        ringMask = logical_and(less(mag, upper), greater(mag, lower))

        avgGrid = where(ringMask, backMag, 0.0)

        # a nearly calm grid means no blending required
        if lower < 1.0:
            return windGrid

        wtGrid = where(greater(mag, upper), 1.0, 0.0)
        ringMask = logical_and(less(mag, upper), greater(mag, lower))
        wtGrid = where(ringMask, (mag - lower) / (upper - lower), wtGrid)
        wtGrid[less(mag, lower)]= 0.0
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

        # make the latGrid and lonGrid
        latGrid, lonGrid = SmartScript.SmartScript.getLatLonGrids(self)

        # Temporarliy save them in the forecast database
        startTime = AbsTime.current()
        endTime = startTime + 86400 * 7 # 7 days
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
                    newList.append(radius)

            # Since we started with the NW quadrant we need to shift
            # the list so that it starts at North to conform to convention
            shift = int(pieSlices / 4)
            shiftedList = newList[shift:]
            shiftedList += newList[:shift]
            newDict[k] = shiftedList
        return newDict

    # Makes a Rankine Vortex wind speed grid that decreases exponentially
    # from the known values at known radii.  Inside the Radius of maximum
    # wind the wind decreases linearly toward the center
    def makeRankine(self, f, latGrid, lonGrid, pieSlices):
        st = time.time()
        grid = zeros(latGrid.shape)
        rDict = f['radii']
        validTime = f['validTime']
        center = f['centerLocation']
        circleEA = CircleEA(latGrid, lonGrid, center, pieSlices)

        rDict = self.interpolateQuadrants(rDict, pieSlices)

        # get the distance grid and make sure it's never zero anywhere
        distanceGrid = circleEA.getDistanceGrid() / 1.852  # dist in NM
        distanceGrid[equal(distanceGrid, 0)] = 0.01

        # make a grid into which we will define the wind speeds
        grid = zeros(latGrid.shape, float)

        # insert the maxWind radii
        if f.has_key('maxWind'):
            maxWind = f['maxWind']
            if f.has_key('eyeDiameter'):
                maxRad = f['eyeDiameter'] / 2.0 + 8.0
            else:
                print "Error --- no eye diameter found."
                maxRad = 12.5 # half of default 25 nm eye diameter

            # add an entry that uses the max wind and radius
            rDict[maxWind] = [maxRad] * pieSlices

        # make a list sorted by average radii value
        wsList = rDict.keys()

        if len(wsList) == 0:
            print "No radii found.  Returning calm grid."
            return (grid, grid)

        radList = []
        for ws in wsList:
            rList = rDict[ws]
            sum = 0
            for r in rList:
                sum += r
            average = sum / len(rList)
            radList.append((average, ws))

        radList.sort()
        radList.reverse()

        wsList = []
        for rad, ws in radList:
            wsList.append(ws)


        maxRad, maxWindValue = radList[-1]
        maxWindValue += 0.1
        rDict[maxWindValue] = [1.0] * pieSlices
        wsList.append(maxWindValue)

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
                    grid = clip(grid, 0.0, 200.0)

        dirGrid = self.makeDirectionGrid(latGrid, lonGrid, center[0], center[1])

        # clip values between zero and maxWind
        grid = clip(grid, 0.0, maxWind)
        # apply the wind reduction over land
        fraction = 1.0 - (self.lessOverLand / 100.0)
        grid = self.decreaseWindOverLand(grid, fraction, self.elevation)

        return (grid, dirGrid)

    def execute(self, varDict, timeRange):
        self.setToolType("numeric")
        self.toolTimeRange = timeRange

        # define the default eye diameter for bulletins where they are missing
        self.dialogEyeDiameter = 0.0

        Topo = self.getTopo()

##        interval = int(varDict["Time Interval\n(hours):"])

        tcDuration = self.getTimeConstraintDuration("Wind")
        tcHours = int(tcDuration / 3600)  # durations are expressed in seconds
        # set the time interpolation interval to the duration
        interval = tcHours
        # get the product ID
##        productList1 = varDict["Product to\ndecode:"]
##        productList2 = varDict["Product to\n decode:"]
##        productList1 = productList1 + productList2  # concatenate
##        if len(productList1) != 1:
##            self.statusBarMsg("Please select one TCM bulletin only.", "S")
##            return None

##        productID = productList1[0]

        # special code for GUM since they do things just a little differently
        siteID = self.getSiteID()
        if siteID == "GUM":
            productID = "GTW" + productID

        bgModelName = varDict["Background\nModel:"]
        # If we're using the Fcst, grab all of the grids now
        if bgModelName == "Fcst":
            inv = self.getWEInventory("Fcst", "Wind", "SFC")
            for tr in inv:
                self.fcstWindGrids[tr] = self.getGrids("Fcst", "Wind", "SFC",
                                                       tr, mode="First")

        pieSlices = int(varDict["Number of\n Pie Slices:"])

        self.lessOverLand = int(varDict["Decrease Wind over Land by (%):"])
        self.elevation = Topo

##        # Use this method to fetch a text product from a file
##        fileName = "Your path and file name goes here"
##        textProduct = self.getTextProductFromFile(fileName)
##        productID = string.split(fileName, "/")[-1]

        # Use this method to fetch a product from the text database
        productID = varDict["ProductID:"]
        textProduct = self.getTextProductFromDB(productID)
        if len(textProduct) < 5:
            print productID, "could not be retrieved from text database."
            return None

        decoder = TCMDecoder()
        decoder.decodeTCMProduct(textProduct, self.dialogEyeDiameter)
        fcstList = decoder.getFcstList()
        print "Decoded:", len(fcstList), " forecasts."

        # Attempt to get the alternate info from a file or the textDB
        altFileName = decoder.getAltInfoFilename()

        altFileName = "/home/eagle6/lefebvre/TPC/" + altFileName

        # get additional info if available
        altProduct = self.getTextProductFromDB(altFileName)

##        ## use this version to fetch from a file
##        altProduct = self.getTextProductFromFile(altFileName)

        rmw, outsideRad = (0, 0)   # initialize
        if len(altProduct) < 5:
            print altProduct, "alternate info file could not be retrieved from text database."
        else:
            rmw, outsideRad = self.decodeDepressionInfo(altProduct)

        # Set the baseDecodedTime - validTime of first entry - 3 hours
        if len(fcstList) > 0:
            self.baseDecodedTime = fcstList[0]['validTime'] - 3 * 3600

##        # See if the decoded fcst is close to the current time.  This is needed
##        # so the tool will work on archived data sets
        selectionTROnly = 0
##        if abs(time.time() - self.baseDecodedTime) > 2 * 24 * 3600:  # older than 2 days
##            testMode = 1
        # restrict grids to the selected time period if option is selected.
        restrictAnswer = varDict["Make Grids over\nSelected Time Only:"]
        if restrictAnswer == "Yes":
            selectionTROnly = 1

        # push this info in the fcsts
        fcstList = self.injectDepressionInfo(fcstList, rmw, outsideRad)

        # interpolate the wind forecasts we got from the decoder
        print "Interpolating wind forecasts:"
        selectedStartTime = self.toolTimeRange.startTime().unixTime()
        selectedEndTime = self.toolTimeRange.endTime().unixTime()
        interpFcstList = []
        for i in xrange(len(fcstList) - 1):

            newFcstList = self.interpolateWindFcst(fcstList[i], fcstList[i+1],
                                                   interval)
            # if we've processed the last time segment, append the last forecast
            if i == len(fcstList) - 2:
                newFcstList.append(fcstList[-1])

            # Make sure the fcst is within the selected time range
            for f in newFcstList:
                if (selectionTROnly and (f['validTime'] >= selectedStartTime and \
                                 f['validTime'] < selectedEndTime)) or not selectionTROnly:
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

        # make a grid for each interpolate forecast
        gridCount = 0
        for f in interpFcstList:
            windGrid = self.makeRankine(f, latGrid, lonGrid, pieSlices)

            validTime = int(f['validTime'] / 3600) * 3600
            bgGrid = self.getClosestWindGrid(bgModelName, validTime)

            if bgGrid is None:
                print "Using calm background grid."
                bgGrid = self.makeWindGrid(0.0, 0.0, latGrid.shape)

            grid = self.blendGrids(windGrid, bgGrid)

            start = AbsTime.AbsTime(int(validTime))
            timeRange = TimeRange.TimeRange(start, start + interval * 3600)

            name = "Wind"
            self.createGrid("Fcst", name, "VECTOR", grid, timeRange,
                            precision=1, minAllowedValue=0.0,
                            maxAllowedValue=200.0)

            gridCount += 1
            print "GenerateCyclone tool:", productID, "- Generated",gridCount, \
                  "out of", len(interpFcstList), "grids"

        return None
