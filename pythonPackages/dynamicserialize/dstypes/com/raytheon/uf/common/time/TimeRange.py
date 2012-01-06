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

# File auto-generated against equivalent DynamicSerialize Java class

import calendar
import datetime
import time

MAX_TIME = 2147483647

class TimeRange(object):
    def __init__(self):
        self.start = None
        self.end = None
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return "(" + self.start.strftime("%b %d %y %H:%M:%S %Z") + ", " + self.end.strftime("%b %d %y %H:%M:%S %Z") + ")"
    
    def __eq__(self, other):
        return ((self.start == other.start) and (self.end == other.end))
    
    def __ne__(self, other):
        return (not self.__eq__(other))

    def getStart(self):
        return self.start.utctimetuple()
    
    def getStartInMillis(self):
        return long(calendar.timegm(self.getStart()) * 1000)

    def setStart(self, start):
        if isinstance(start, datetime.datetime):
            self.start = start
        elif isinstance(start, time.struct_time):
            self.start = datetime.datetime(*start[:6])
        else:
            totalSecs = long(start)
            if totalSecs < MAX_TIME:
                self.start = datetime.datetime.utcfromtimestamp(totalSecs)
            else:
                extraTime = datetime.timedelta(seconds=(totalSecs - MAX_TIME))
                self.start = datetime.datetime.utcfromtimestamp(MAX_TIME) + extraTime

    def getEnd(self):
        return self.end.utctimetuple()
    
    def getEndInMillis(self):
        return long(calendar.timegm(self.getEnd()) * 1000)

    def setEnd(self, end):
        if isinstance(end, datetime.datetime):
            self.end = end
        elif isinstance(end, time.struct_time):
            self.end = datetime.datetime(*end[:6])
        else:
            totalSecs = long(end)
            if totalSecs < MAX_TIME:
                self.end = datetime.datetime.utcfromtimestamp(totalSecs)
            else:
                extraTime = datetime.timedelta(seconds=(totalSecs - MAX_TIME))
                self.end = datetime.datetime.utcfromtimestamp(MAX_TIME) + extraTime
                
    def isValid(self):
        return (self.start != self.end)
    
    @staticmethod        
    def allTimes():
        tr = TimeRange()
        tr.setStart(0)
        tr.setEnd(MAX_TIME)
        return tr
