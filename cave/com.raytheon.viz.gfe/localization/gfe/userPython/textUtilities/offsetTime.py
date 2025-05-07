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
# offsetTime.py
# Handles Displaced Real Time for various applications
#
# Author: hansen/romberg
# ----------------------------------------------------------------------------
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/30/16        6016          randerso       Changed to use SimulatedTime.setTimeOffset()
#    05/11/23        2033877       smoorthy       Adjust intercepted time functions and variables 
#                                                 to account for timezone being stored in this file,
#                                                 as opposed to os.environ['TZ'] with tzset().
#    09/19/23        2036183       mapeters       Add time.mktime() intercept
#    10/12/23        2036310       mapeters       Setup intercepts in setTimeZone(), reset more
#                                                 things in reset()
#    02/16/24        2036854       smoorthy       Mimic os.environ functionality and return TypeError
#                                                 if user attempts to set tz to a list.
#
##

##
# This is a base file that is not intended to be overridden.
##

import time, string
import logging
import datetime as datetime_top
import pytz
import re
from awips import TimeUtil
from datetime import datetime




offset = 0
timeStr = ""
launchStr = ""


timezoneMappings={'America/Anchorage':('AKST', 'AKDT'),
                  'America/Juneau':('AKST', 'AKDT'),
                  'US/Arizona':('MST', 'MST'),
                  'Pacific/Guam':('ChST', 'ChST'),
                  'Pacific/Honolulu':('HST', 'HST'),
                  'Pacific/Samoa':('SST', 'SST'),
                  'America/Puerto_Rico':('AST', 'AST')}

tz = 'GMT'


oldTime = time.time
oldLocaltime = time.localtime
oldMktime = time.mktime
oldGmtime = time.gmtime
oldAsctime = time.asctime
oldCtime = time.ctime
oldStrftime = time.strftime

oldAltzone = time.altzone
oldTzname = time.tzname
oldDaylight = time.daylight

log = logging.getLogger("FormatterRunner.offsetTime")
# Method called by modules running applications
# to be run in Displaced Real Time (DRT).
# "timeString" can be in 3 formats:
#       YYYYMMDD_HHMM -- Desired Displaced Real Time
#      SYYYYMMDD_HHMM -- Synchronize to most recent hour
#       YYYYMMDD_HHMM,YYYYMMDD_HHMM -- Determine offset based on
#                                      difference between given times
def setDrtOffset(timeString):
    global offset, timeStr, launchStr
    # Do not re-set offset after set once
    if offset != 0:
        return


    seconds = 0
    launchString = "default launch, no displacement."
    if len(timeString) > 0: 
        seconds, launchString = TimeUtil.determineDrtOffset(timeString)

    # Save the offset and timeStr
    timeStr = timeString
    launchStr = launchString
    # Try to set AbsTime offset as well
    # for applications involving C++
    try:
        from com.raytheon.uf.common.time import SimulatedTime
        stOffset = SimulatedTime.getSystemTime().getOffset() / 1000.0
        if abs(seconds - stOffset) > 60:
            offset = seconds
            SimulatedTime.getSystemTime().setTimeOffset(offset * 1000)
        else:
            offset = stOffset
    except:
        log.exception("Problem setting simulated time ")

    _setupIntercepts()

    # Log Event
    log.info("Setting DRT mode: " + timeStr + \
       "\n                                      Offset: " + repr(offset) + " seconds" +\
       "\n                                      LaunchStr: " + launchString)

def _setupIntercepts():
    # Override the time module methods/fields. Anything overridden here needs
    # to be handled in reset() below as well.
    time.gmtime = offsetGmtime
    time.time = offsetTime
    time.localtime = offsetLocaltime
    time.mktime = offsetMktime
    time.asctime = offsetAsctime
    time.ctime = offsetCtime
    time.strftime = offsetStrftime

    time.altzone = getAltzone()
    time.tzname = getTzname()
    time.daylight = getDaylight()

# Methods substituted for time module when in
# DRT mode
def offsetTime():
    tmp = oldTime()
    return tmp + offset

def offsetGmtime(secs=None):
    if secs is None:
        secs = oldTime() + offset
    return oldGmtime(secs)

def offsetLocaltime(secs=None):
    timezone = pytz.timezone(tz)


    if secs is None:
        dt = datetime.now(timezone)
        timeDelta = datetime_top.timedelta(0, offset)
        dt = dt + timeDelta
        return dt.timetuple()
    else:
        dt = datetime.fromtimestamp(secs, timezone)
        return dt.timetuple()

def offsetMktime(t):
    # local time tuple -> UTC seconds

    # Create datetime from relevant fields, truncating leap seconds to 59 so
    # that datetime doesn't throw an error
    dt = datetime(*t[:5], min(t[5], 59))
    # Localize and convert to UTC seconds
    dt = pytz.timezone(tz).localize(dt)
    return dt.timestamp()

def offsetAsctime(time_s=None):
    if time_s is None:
        time_s = time.localtime()
    return oldAsctime(time_s)

def offsetCtime(secs=None):
    if secs is None:
        secs = oldTime() + offset
    return oldCtime(secs)

def offsetStrftime(format, time_s=None):
    if time_s is None:
        time_s = time.localtime()

    timeStr = oldStrftime(format, time_s)

    if re.search('%Z', format) != None:
        #replace os.environ-based TZ string with new one
        timeZoneStr = re.search('([A-Za-z]{1,2}[S,D,M]T)', timeStr).groups()[0]
        timezone = time.tzname[time.localtime().tm_isdst]
        timeStr = timeStr.replace(timeZoneStr, timezone)

    return timeStr



def getTimeZone():
    return tz

def setTimeZone(newTimezone):
    global tz

    if isinstance(newTimezone, list):
        raise TypeError("str expected, not list")
    tz = newTimezone

    _setupIntercepts()

def getTzname():

    #e.g CST6CDT
    timezoneSplit = re.split('\d', tz)

    if len(timezoneSplit) == 2:
        #e.g (CST, CDT)
        return tuple(timezoneSplit)
    if len(timezoneSplit) == 1:
        #e.g 'Pacific/Honolulu', tzname = ('HST', 'HST')
        if tz in timezoneMappings:
            return timezoneMappings[tz]
        else:
            return (tz, tz)
    else:
        return ('GMT', 'GMT')


def getAltzone():
    # Gets the equivalent of time.altzone, which is DST seconds from UTC.
    # Postive difference if TZ is west of UTC, and negative otherwise.
    # Expectation for both this and original time.altzone is that timezone
    # has a DST defined. If not, results are not similar.

    myTimezone = pytz.timezone(tz)
    myTimeStruct = datetime.now(myTimezone).timetuple()

    gmtTimezone = pytz.timezone('GMT')
    gmtTimeStruct = datetime.now(gmtTimezone).timetuple()


    dayDiff = gmtTimeStruct.tm_yday - myTimeStruct.tm_yday

    #next day is first day of new year
    if dayDiff > 100:
        #myTimeStruct > gmtTimeStruct
        dayDiff = -1
    if dayDiff < -100: 
        #myTimeStruct < gmtTimeStruct
        dayDiff = 1

    diffSeconds = (gmtTimeStruct.tm_hour + dayDiff*24 - myTimeStruct.tm_hour)*60*60

    #if not DST, subtract 1 hour from difference to get DST difference
    if not myTimeStruct.tm_isdst:
        diffSeconds = diffSeconds - 60*60

    return diffSeconds


def getDaylight():
    # Return 1 if a daylight savings timezone is defined, e.g CST6CDT. Else, 0.
    # Similar to time.daylight variable.

    tzSplit = re.split('\d', tz)
    if len(tzSplit) == 2 and tzSplit[1] != '':
        return 1
    return 0

# Accessor methods

def drtOffset():
    return offset

def drtTimeStr():
    return timeStr

def drtLaunchStr():
    return launchStr

def reset():
    global offset, timeStr, launchStr, tz
    offset = 0
    timeStr = ""
    launchStr = ""
    tz = 'GMT'

    #reload(time)
    time.time = oldTime
    time.localtime = oldLocaltime
    time.mktime = oldMktime
    time.gmtime = oldGmtime
    time.asctime = oldAsctime
    time.ctime = oldCtime
    time.strftime = oldStrftime

    time.altzone = oldAltzone
    time.tzname = oldTzname
    time.daylight = oldDaylight

    from com.raytheon.uf.common.time import SimulatedTime
    SimulatedTime.getSystemTime().setRealTime()
