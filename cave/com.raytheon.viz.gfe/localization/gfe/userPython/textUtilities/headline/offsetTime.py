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

import time, string
import logging
from ufpy import TimeUtil

offset = 0
timeStr = ""
launchStr = ""

oldTime = time.time
oldLocaltime = time.localtime
oldGmtime = time.gmtime
oldAsctime = time.asctime
oldCtime = time.ctime
oldStrftime = time.strftime

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
    seconds, launchString = TimeUtil.determineDrtOffset(timeString)
    # Save the offset and timeStr
    offset = seconds
    timeStr = timeString
    launchStr = launchString
    # Try to set AbsTime offset as well
    # for applications involving C++
    try:
        from com.raytheon.uf.common.time import SimulatedTime
        from java.util import Date
        if not SimulatedTime.getSystemTime().isRealTime():
            SimulatedTime.getSystemTime().setRealTime()
        seconds = SimulatedTime.getSystemTime().getTime().getTime() / 1000
        seconds += offset
        SimulatedTime.getSystemTime().setTime(Date(seconds * 1000))
        #print "setting abstime offset", seconds, seconds/3600
    except:
        log.exception("Problem setting simulated time ")        
    # Override the time module methods
    time.gmtime = offsetGmtime
    time.time = offsetTime
    time.localtime = offsetLocaltime
    time.asctime = offsetAsctime
    time.ctime = offsetCtime
    time.strftime = offsetStrftime
    # Log Event
    log.info("Setting DRT mode: " + timeStr + \
       "\n                                      Offset: " + `offset` + " seconds" +\
       "\n                                      LaunchStr: " + launchString)
    
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
    if secs is None:
        secs= oldTime() + offset
    return oldLocaltime(secs)

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
    return oldStrftime(format, time_s)

# Accessor methods 
def drtOffset():
    return offset

def drtTimeStr():
    return timeStr

def drtLaunchStr():
    return launchStr

def reset():
    #reload(time)
    time.time = oldTime
    time.localtime = oldLocaltime
    time.gmtime = oldGmtime 
    time.asctime = oldAsctime
    time.ctime = oldCtime
    time.strftime = oldStrftime
    from com.raytheon.uf.common.time import SimulatedTime
    SimulatedTime.getSystemTime().setRealTime()
