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
from datetime import datetime, timedelta
import calendar
from java.util import Date 
import JUtil

import dateutil.tz
GMT_ZONE = dateutil.tz.gettz('GMT')

#
# Provides a AWIPS I GFE partially-compatible wrapper to AbsTime
# 
# This class extends standard Python dateTime, so many convenience and
# manipulation methods are available using the standard syntax.  
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/10/08                      chammack       Initial Creation.
#    12/01/2014       #3875        randerso       Set time zone on underlying datetime object to GMT
# 
#
class AbsTime(datetime, JUtil.JavaWrapperClass):
    
    def __new__(self, arg):
       if arg is None:
           return None;
       
       if isinstance(arg, int) or isinstance(arg, long) or isinstance(arg, float):            
           tmp = datetime.utcfromtimestamp(arg)
       else:
           tmp = datetime.utcfromtimestamp(arg.getTime() / 1000)
           
       return datetime.__new__(self, tmp.year,tmp.month,
              tmp.day, tmp.hour, tmp.minute, tmp.second, tzinfo=GMT_ZONE)
       
    def unixTime(self):
        tuple = self.utctimetuple()        
        return calendar.timegm(tuple)
    
    def _fromDateTime(self, dt):
        return AbsTime(calendar.timegm(dt.utctimetuple()))
    
    def __add__(self, arg):        
        if isinstance(arg, int) or isinstance(arg, long) or isinstance(arg, float):
            offset = arg         
        key = {"seconds": offset}
        dt = datetime.__add__(self, timedelta(**key))
        return self._fromDateTime(dt)
        
    def __sub__(self, arg):
        if isinstance(arg, AbsTime):
            return self.unixTime() - arg.unixTime()
        elif isinstance(arg, int) or isinstance(arg, long) or isinstance(arg, float):
            offset = arg 
        key = {"seconds": offset}
        dt = datetime.__sub__(self, timedelta(**key))
        return self._fromDateTime(dt)
        
    def javaDate(self):
        if not hasattr(self, '__javaDate'):
            sec = calendar.timegm(self.utctimetuple())
            self.__javaDate = Date(sec*1000)
        return self.__javaDate
    
    def toJavaObj(self):
        return javaDate()

    def stringFmt(self, fmt):
        return self.strftime(fmt)
    
    def string(self):
        return self.stringFmt("%h %d %y %T GMT")

def absTimeYMD(year, month, day, hour=0, minute=0, second=0):
    tm = datetime(year, month, day, hour, minute, second)
    tup = tm.utctimetuple()
    sec = calendar.timegm(tup)
    return AbsTime(sec)

def current():
    from com.raytheon.uf.common.time import SimulatedTime
    return AbsTime(SimulatedTime.getSystemTime().getTime())

def maxFutureTime():
    from com.raytheon.uf.common.time import TimeRange as JavaTimeRange
    tr = JavaTimeRange.allTimes()
    return AbsTime(tr.getEnd())
    
