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
# TimeRangeUtils.py
# Utilities for dealing with Time Ranges in Text Products.
#
# Author: hansen
# ----------------------------------------------------------------------------

import time, string, types, os
import TextUtils
import AbsTime, TimeRange

class TimeRangeUtils(TextUtils.TextUtils):
    def __init__(self):
        TextUtils.TextUtils.__init__(self)

    def makeTimeRange(self, startTime, endTime):
        try:
            return TimeRange.TimeRange(startTime, endTime)
        except:
            startTime = AbsTime.AbsTime(int(startTime))
            endTime = AbsTime.AbsTime(int(endTime))
            return TimeRange.TimeRange(startTime, endTime)

    def createTimeRange(self, startHour, endHour, mode="LT"):
        # Returns an TimeRange.TimeRange object given by:
        #    startHour, endHour
        #       (range is startHour up to and not including endHour)
        #       startHour and endHour are relative to midnight of the
        #          current day either in Local or Zulu time (see below)
        #    mode can be:
        #    "LT" : the startHour and endHour are relative to midnight local time
        #    "Zulu": relative to 0Z Zulu time
        #
        # E.g.
        #    timeRange = self.createTimeRange(0, 121, "Zulu")

        if mode == "LT":
            localTime, shift = self.determineTimeShift()
            today = AbsTime.absTimeYMD(localTime.year, localTime.month,
                                    localTime.day, 0, 0, 0)
            start = today + (startHour *3600) - shift
            end = today + (endHour * 3600) - shift
            return TimeRange.TimeRange(start, end)
        else:
            gmTime = time.gmtime(time.time())
            today = AbsTime.absTimeYMD(gmTime[0],gmTime[1],gmTime[2], 0, 0, 0)
            start = today + (startHour *3600)
            end = today + (endHour * 3600)
            return TimeRange.TimeRange(start, end)

    ##
    # Get the time range corresponding to the named time range.
    # @param timeRangeName: The time range name to find, i.e., "tonight"
    # @type timeRangeName: string
    # @param argDict: argDict
    # @return: The named time range, based on the current time.
    # @rtype: Python TimeRange
    def getTimeRange(self, timeRangeName, argDict=None):
        # Return a timeRange corresponding to the named time range
        return TimeRange.TimeRange(argDict['dataMgr'].getSelectTimeRangeManager().getRange(timeRangeName).toTimeRange());

    def getPeriod(self, timeRange, shiftToLocal=0):
        # Based on midpoint hour,
        # Return 1 if day, 0 if night
        # Return -1 if greater than or equal 24 hours duration
        if shiftToLocal == 1:
            timeRange = self.shiftedTimeRange(timeRange)
        if timeRange.duration() >= 24 * 3600:
            return self.DAYNIGHT()
        # Find midpoint hour
        startTime = timeRange.startTime() + timeRange.duration()/2
        hour = startTime.hour
        day = self.DAY()
        night = self.NIGHT()
        if hour >= day and hour < night:
            return self.DAYTIME()
        else:
            return self.NIGHTTIME()

    def shiftedTimeRange(self, timeRange):
        # Shift the given time range to local time.
        # It is assumed to be in GMT.
        localTime, shift = self.determineTimeShift()
        return TimeRange.TimeRange(timeRange.startTime() + shift,
                              timeRange.endTime() + shift)

    def getTimeRangeList(self, argDict, rangeNames, labelMethod=None, labelFormat=None):
        # Make a list of (timeRange, label) tuples for the given
        # timeRanges and labelMethod
        #
        #Arguments:
        # rangeNames = a list of time range names or actual time ranges
        # labelMethod = text method to label each time range.
        # labelFormat = format for labeling time ranges.
        #   If included, overrides labelMethod.
        #   Format is of form: ( LT_OR_Zulu, durationFmt, startFmt, endFmt)
        #   See Text Product User Guide to see possible formats.
        #   e.g. ("Zulu", "", "%HZ/%d", "")
        #
        trList = []
        for name in rangeNames:
            if isinstance(name, TimeRange.TimeRange):
                range = name
            else:
                range = self.getTimeRange(name, argDict)
            if labelFormat is not None:
                LTorZulu, durFmt, startFmt, endFmt = labelFormat
                label = self.timeDisplay(
                    range, LTorZulu, durFmt, startFmt, endFmt)
            elif labelMethod is not None:
                label = labelMethod(range)
            else:
                label = ""
            trList.append((range, label))
        return trList

    def getPeriods(self, timeRange, period, span, numPeriods=None,
                    labelMethod=None, labelFormat=None):
        # Make a list of (timeRange, label) tuples for the given
        # timeRange, period and span using the labelMethod or labelFormat
        #
        #Arguments:
        # timeRange = a TimeRange from TimeRange.py
        # period = number of hours between periods beginning at
        #   the start of the given timeRange
        # span = number of hours duration for each period.
        #  (Note that with the "period" and "span" arguments, you
        #  could have periods every 12 hours which are only
        #  1-hour in duration: period=12, span=1.)
        # numPeriods = Number of periods desired.
        #   If None, periods go to the end of the timeRange.
        # labelMethod = text method to label the period.
        # labelFormat = format for labeling periods.
        #   If included, overrides labelMethod.
        #   Format is of form: ( LT_OR_Zulu, durationFmt, startFmt, endFmt)
        #   See Text Product User Guide to see possible formats.
        #   e.g. ("Zulu", "", "%HZ/%d", "")
        #

        actualTR = timeRange
        if numPeriods is not None:
            actualTR = TimeRange.TimeRange(
                timeRange.startTime(),
                timeRange.startTime() + int(numPeriods * period * 3600))
        periodList = []
        start = actualTR.startTime()
        while start < actualTR.endTime():
            # Create Time Range for next period
            end = start + 3600 * span # 3600 = 1 hour in seconds
            tr = TimeRange.TimeRange(start, end)
            if labelFormat is not None:
                LTorZulu, durFmt, startFmt, endFmt = labelFormat
                label = self.timeDisplay(
                    tr, LTorZulu, durFmt, startFmt, endFmt)
            elif labelMethod is not None:
                label = labelMethod(tr)
            else:
                label = ""
            periodList.append((tr, label))
            start = start + int(3600 * period)
        return periodList

    def adjustTimeRange(self, timeRange, adjustHours):
        # Return a time range adjusted by the given number of hours
        return TimeRange.TimeRange(timeRange.startTime() + adjustHours*3600,
                              timeRange.endTime() + adjustHours*3600)

    def localTime(self, startTime, hours, shift):
        # Return the local time using shift and the startTime
        return startTime + hours * 3600 - shift

    def hrToSec(self, hours):
        "Convert hours given by period to seconds"
        return hours * 60 * 60


    def daylight(self):
        # Return 1 if local time is currently daylight savings
        localtime = time.localtime(time.time())
        dayLight =  localtime[8]
        if dayLight > 0:
            return 1
        else:
            return 0

    def determineShift(self):
        # Return the difference: Local Time - GMT time
        localTime, shift = self.determineTimeShift()
        return shift

    def determineTimeShift(self):
        # Return the current local time and the difference:
        #   Local Time - GMT time
        curTime = time.time()
        localtime = time.localtime(curTime)
        currentLocalTime = AbsTime.absTimeYMD(
            localtime[0],localtime[1],localtime[2],localtime[3],localtime[4])
        gmTime = time.gmtime(curTime)
        currentGMTime = AbsTime.absTimeYMD(
            gmTime[0],gmTime[1],gmTime[2],gmTime[3],gmTime[4])
        shift = currentLocalTime - currentGMTime
        return currentLocalTime, shift

    def timeDisplay(self, timeRange, LTorZulu, durFmt, startFmt, endFmt):
        # Return a string display for the given timeRange, assumed to be
        #  in GMT.
        # If LTorZulu == "LT", the timeRange will be converted from GMT
        #  to local time.
        # durationFmt, startFmt, endFmt are format strings for the
        #  timeRange duration, the start time and end time respectively.
        # See Text Product User Guide to see possible formats.
        #
        # Example:
        #   self.timeDisplay(timeRange, "LT", "%H hours ",
        #                     "%a %b %d, %Y %I:%M %p",
        #                    " to %a %b %d, %Y %I:%M %p %Z")
        #
        #   yields a string such as:
        #
        #  12 hours Mon Apr 23, 2001 06:00 AM to Mon Apr 23, 2001 06:00 PM MDT.

        if LTorZulu == "LT":
            # Convert to local time
            timeRange = self.shiftedTimeRange(timeRange)
        display = ""
        if durFmt != "":
            duration = timeRange.duration()
            durHours = duration / 3600
            durMinutes = duration / 3600 / 60
            durStr = string.replace(durFmt, "%H", `durHours`)
            durStr = string.replace(durStr, "%M", `durMinutes`)
            display = display + durStr
        if startFmt != "":
            #display = display + timeRange.startTime().stringFmt(startFmt)
            display = display + timeRange.startTime().strftime(startFmt)
        if endFmt != "":
            #display = display + timeRange.endTime().stringFmt(endFmt)
            display = display + timeRange.endTime().strftime(endFmt)
        if LTorZulu == "LT":
            # Adjust time zone to local time
            localTime = time.localtime(time.time())
            zoneName = time.strftime("%Z",localTime)
            display = string.replace(display,"GMT",zoneName)
        return display
