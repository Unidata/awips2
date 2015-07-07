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

# File auto-generated against equivalent DynamicSerialize Java class. Then modified to add functionality
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/??                      xxxxxxxx       Initial Creation.
#    01/22/14        2667          bclement       fixed millisecond support
#    02/28/14        2667          bclement       constructor can take extra micros for start and end
#    06/24/15        4480          dgilling       fix __eq__.
#    
# 
#

import calendar
import datetime
import time

MAX_TIME = 2147483647
MICROS_IN_SECOND = 1000000

class TimeRange(object):
    def __init__(self, start=None, end=None, startExtraMicros=None, endExtraMicros=None):
        self.start = self.__convertToDateTimeWithExtra(start, startExtraMicros)
        self.end = self.__convertToDateTimeWithExtra(end, endExtraMicros)
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return "(" + self.start.strftime("%b %d %y %H:%M:%S %Z") + ", " + self.end.strftime("%b %d %y %H:%M:%S %Z") + ")"
    
    def __eq__(self, other):
        if type(self) != type(other):
            return False
        
        if self.isValid() and other.isValid():
            return self.getStart() == other.getStart() and self.getEnd() == other.getEnd()
        elif not self.isValid() and not other.isValid():
            return True
        else:
            return False
    
    def __ne__(self, other):
        return (not self.__eq__(other))
    
    def __convertToDateTimeWithExtra(self, timeArg, extraMicros):
        rval = self.__convertToDateTime(timeArg)
        if rval is not None and extraMicros is not None:
            rval = rval + datetime.timedelta(microseconds=extraMicros)
        return rval

    def __convertToDateTime(self, timeArg):
        if timeArg is None:
            return None
        if isinstance(timeArg, datetime.datetime):
            return timeArg
        elif isinstance(timeArg, time.struct_time):
            return datetime.datetime(*timeArg[:6])
        elif isinstance(timeArg, float):
            # seconds as float, should be avoided due to floating point errors
            totalSecs = long(timeArg)
            micros = int((timeArg - totalSecs) * MICROS_IN_SECOND)
            return self.__convertSecsAndMicros(totalSecs, micros)
        elif isinstance(timeArg, (int, long)):
            # seconds as integer
            totalSecs = timeArg
            return self.__convertSecsAndMicros(totalSecs, 0)
        else:
            return None

    def __convertSecsAndMicros(self, seconds, micros):
        if seconds < MAX_TIME:
            rval = datetime.datetime.utcfromtimestamp(seconds)
        else:
            extraTime = datetime.timedelta(seconds=(seconds - MAX_TIME))
            rval = datetime.datetime.utcfromtimestamp(MAX_TIME) + extraTime
        return rval.replace(microsecond=micros)

    def getStart(self):
        return self.start.utctimetuple()
    
    def getStartInMillis(self):
        return self._getInMillis(self.start)

    def setStart(self, start, extraMicros=None):
        self.start = self.__convertToDateTimeWithExtra(start, extraMicros)

    def getEnd(self):
        return self.end.utctimetuple()
    
    def getEndInMillis(self):
        return self._getInMillis(self.end)
    
    def _getInMillis(self, time):
        rval = long(calendar.timegm(time.utctimetuple()) * 1000)
        rval += time.microsecond // 1000
        return rval

    def setEnd(self, end, extraMicros=None):
        self.end = self.__convertToDateTimeWithExtra(end, extraMicros)
                
    def duration(self):
        delta = self.end - self.start
        return long(delta.total_seconds())        
    
    def contains(self, timeArg):
        if isinstance(timeArg, TimeRange):
            if self.duration() == 0:
                return self.__eq__(timeArg)
            elif timeArg.duration() == 0:
                return self.contains(timeArg.start)
            return (timeArg.start >= self.start and timeArg.end <= self.end)
        else:
            convTime = self.__convertToDateTime(timeArg)
            if type(convTime) is not datetime.datetime:
                raise TypeError("Invalid type for argument time specified to TimeRange.contains().")
            if self.duration() != 0:
                return (convTime >= self.start and convTime < self.end)
            return convTime == self.start
    
    def isValid(self):
        return bool(self.start != self.end)
    
    def overlaps(self, timeRange):
        return (timeRange.contains(self.start) or self.contains(timeRange.start))
    
    def combineWith(self, timeRange):
        if self.isValid() and timeRange.isValid():
            newStart = min(self.start, timeRange.start)
            newEnd = max(self.end, timeRange.end)
            return TimeRange(newStart, newEnd)
        elif self.isValid():
            return self
        
        return timeRange
    
    @staticmethod        
    def allTimes():
        return TimeRange(0, MAX_TIME)
