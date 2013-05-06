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

from com.raytheon.uf.common.time import TimeRange as JavaTimeRange
import AbsTime
import JUtil

#
# Provides a AWIPS I GFE compatible wrapper to TimeRange
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/10/08                      chammack       Initial Creation.
#    09/30/08         1566         wdougher       Quit returning TimeRange from overlaps(), etc.
#    09/16/09         2899         njensen        Huge performance boost by caching
#    04/04/2013      #1787         randerso       Removed isValid check to allow 0 duration
#                                                 time ranges to be used in python
# 
#

class TimeRange(JUtil.JavaWrapperClass):
    def __init__(self, time1, time2=None):        
        # these vars are only calculated if requested
        self.__hash = None
        self.__start = None
        self.__end = None
        self.__duration = None
        
        # Single argument assumes a passed in java timerange
        if time2 == None:
            self.__tr = time1
            return

        # Check to see if abstimes or java classes are passed in
        if isinstance(time1, AbsTime.AbsTime):
            time1_java = time1.javaDate()
            self.__start = time1
        else:
            time1_java = time1
            
        if isinstance(time2, AbsTime.AbsTime):
            time2_java = time2.javaDate()
            self.__end = time2
        else:
            time2_java = time2
        self.__tr = JavaTimeRange(time1_java, time2_java)
 

    def startTime(self):
        if not self.__start:
            self.__start = self.__asAbsTime(self.__tr.getStart()) 
        return self.__start
    
    def endTime(self):
        if not self.__end:
            self.__end = self.__asAbsTime(self.__tr.getEnd())
        return self.__end
    
    def __asAbsTime(self, date):
        if date is None:
            return None
        
        A = date.getTime() 
        return AbsTime.AbsTime(A / 1000);
    
    def duration(self):
        if not self.__duration:
            self.__duration = self.__tr.getDuration() / 1000 
        return self.__duration
    
    def toJavaObj(self):
        return self.__tr
    
    def contains(self, timeOrTimeRange):
        if isinstance(timeOrTimeRange, AbsTime.AbsTime):
            return self.__tr.contains(timeOrTimeRange.javaDate())
        if isinstance(timeOrTimeRange, TimeRange):
            return self.__tr.contains(timeOrTimeRange.toJavaObj())
    
    def overlaps(self, timeRange):
        return self.__tr.overlaps(timeRange.toJavaObj())

    def isAdjacentTo(self, timeRange):
        return self.__tr.isAdjacentTo(timeRange.toJavaObj())
    
    def join(self, timeRange):
        return TimeRange(self.__tr.join(timeRange.toJavaObj()))
 
    def intersection(self, timeRange):
        return TimeRange(self.__tr.intersection(timeRange.toJavaObj()))

    def gap(self, timeRange):
        return TimeRange(self.__tr.gap(timeRange.toJavaObj()))

    def span(self, timeRange):
        return TimeRange(self.__tr.span(timeRange.toJavaObj()))

    def combineWith(self, timeRange):
        return TimeRange(self.__tr.combineWith(timeRange.toJavaObj()))
 
    def isValid(self):
        return self.__tr.isValid()
    
    def __eq__(self, other):
        return self.__tr.equals(other.toJavaObj())
    
    def __ne__(self, other):
        return not self == other
    
    def __lt__(self, other):
        return self.__tr.compareTo(other.toJavaObj()) < 0

    def __le__(self, other):
        return self.__tr.compareTo(other.toJavaObj()) <= 0

    def __gt__(self, other):
        return self.__tr.compareTo(other.toJavaObj()) > 0

    def __ge__(self, other):
        return self.__tr.compareTo(other.toJavaObj()) >= 0

    def __hash__(self):
        if not self.__hash:
            self.__hash = self.startTime().unixTime() ^ self.endTime().unixTime()
        return self.__hash
    
    def __str__(self):
        return str(self.__tr.toString())

    def __repr__(self):
        return str(self.__tr.toString())

def javaTimeRangeListToPyList(timeRanges):
    pylist = []
    size = timeRanges.size()
    for i in range(size):
        jtr = timeRanges.get(i)
        timelist = encodeJavaTimeRange(jtr)        
        pylist.append(timelist)
    return pylist

def encodeJavaTimeRange(javaTimeRange):
    time = TimeRange(javaTimeRange)
    start = time.startTime()
    end = time.endTime()
    return (start.unixTime(), end.unixTime())

def allTimes():
    tr = JavaTimeRange.allTimes()
    return TimeRange(tr.getStart(), tr.getEnd())

def default():
    tr = JavaTimeRange()
    return TimeRange(tr)

