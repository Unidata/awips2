##
##

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

##
# This is a base file that is not intended to be overridden.
##



from datetime import datetime, timedelta
import calendar
from java.util import Date 
import JUtil

import dateutil.tz
GMT_ZONE = dateutil.tz.gettz('GMT')


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
    
