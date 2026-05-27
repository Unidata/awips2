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

#
# Provides a AWIPS I GFE partially-compatible wrapper to AbsTime
#
# AbsTime is an immutable object that holds time info down to second
# precision.  If sub-second precision is required, it will need to be
# reworked.
#
# Prior to 21.4.1, this class used to extend standard Python dateTime.
# This no longer works.  Based on the statement on Python bug 32417
# (https://bugs.python.org/issue32417#msg331347),
#
# "we have realized that this change will *also* break anyone whose default
# constructor does not support the same signature as the base datetime",
#
# it appears that that fix broke it.  Note that we could theoretically still
# extend datetime but the constructor's method signature would have to match
# datetime's, which would break our usage of AbsTime and backwards
# compatibility with code developed against the earlier version of AbsTime.
# Therefore, this class was refactored to wrap a Java Date and Python datetime
# (since both are immutable), and methods and fields were added to ensure the
# same behavior.
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/10/08                      chammack       Initial Creation.
#    12/01/2014       #3875        randerso       Set time zone on underlying datetime object to GMT
#    Aug 06, 2021     #8511        njensen        Rewrote to wrap java.util.Date instead of extending Python datetime
#    Nov 02, 2021     #8511        njensen        Add strftime method
#    Dec 07, 2021     #8511        njensen        Add timetuple method
#    Mar 14, 2022     #8764        tgurney        Add astimezone method
#    Mar 15, 2022     #8764        tgurney        Add utctimetuple method
#    Nov 29, 2022      8983        njensen        Added __new__ to handle None arg and handle FormattedDate in __init__
#    Feb 17, 2023      9028        tgurney        Add __str__ and __repr__, fix toJavaObj
#    Feb 20, 2023      9028        njensen        Implemented more datetime and timedelta compatibility
#
#

##
# This is a base file that is not intended to be overridden.
##


import datetime
import calendar
from java.util import Date
import JUtil

# for backwards compatibility we use GMT instead of UTC
import dateutil.tz
GMT_ZONE = dateutil.tz.gettz('GMT')


class AbsTime(JUtil.JavaWrapperClass):

    def __new__(cls, arg):
        if arg is None:
            return None
        return JUtil.JavaWrapperClass.__new__(cls)

    def __init__(self, arg):
        """ Constructor takes an int or float in seconds or java.util.Date """
        if isinstance(arg, int) or isinstance(arg, float):
            self.__javaDate = Date(int(arg * 1000))
        else:
            errMsg = 'AbsTime constructor only supports ints, floats, java.util.Dates, or com.raytheon.uf.common.time.FormattedDates, but received a '
            if hasattr(arg, 'java_name'):
                if arg.java_name == 'java.util.Date':
                    self.__javaDate = arg
                elif arg.java_name == 'com.raytheon.uf.common.time.FormattedDate':
                    self.__javaDate = Date(arg.getTime())
                else:
                    errMsg += 'PyJObject of type ' + str(arg.java_name)
                    raise TypeError(errMsg)
            else:
                errMsg += str(type(arg))
                raise TypeError(errMsg)

        self.__pydt = datetime.datetime.fromtimestamp(self.__javaDate.getTime() // 1000).astimezone(GMT_ZONE)

        # we hold these attributes as properties to resemble a datetime object and make them read-only
        self._tzinfo = self.__pydt.tzinfo
        self._year = self.__pydt.year
        self._month = self.__pydt.month
        self._day = self.__pydt.day
        self._hour = self.__pydt.hour
        self._minute = self.__pydt.minute
        self._second = self.__pydt.second
        self._microsecond = self.__pydt.microsecond
        self._weekday = self.__pydt.weekday()

    def unixTime(self):
        """ Returns the time as seconds since the epoch """
        return self.javaDate().getTime() // 1000

    def _fromDateTime(self, dt):
        # this really shouldn't be an instance method, but there's concern that
        # localapps may have used this method, so we're leaving it as is
        return AbsTime(calendar.timegm(dt.utctimetuple()))

    def _toDateTime(self):
        return self.__pydt

    def __add__(self, arg):
        if isinstance(arg, int) or isinstance(arg, float):
            seconds = self.unixTime() + arg
            return AbsTime(seconds)
        elif isinstance(arg, datetime.timedelta):
            newdt = self._toDateTime() + arg
            return self._fromDateTime(newdt)
        else:
            return NotImplemented

    def __sub__(self, arg):
        if isinstance(arg, AbsTime):
            diff = self.unixTime() - arg.unixTime()
            return diff
        elif isinstance(arg, int) or isinstance(arg, float):
            seconds = self.unixTime() - arg
            return AbsTime(seconds)
        elif isinstance(arg, datetime.datetime):
            diff = self._toDateTime() - arg
            return diff
        elif isinstance(arg, datetime.timedelta):
            diff = self._toDateTime() - arg
            return self._fromDateTime(diff)
        else:
            return NotImplemented

    __radd__ = __add__

    # we override __rsub__ to support datetime - AbsTime
    def __rsub__(self, arg):
        if isinstance(arg, datetime.datetime):
            diff = arg - self._toDateTime()
            return diff
        else:
            return NotImplemented

    def __lt__(self, arg):
        if isinstance(arg, AbsTime):
            return self.javaDate().getTime() < arg.javaDate().getTime()
        elif isinstance(arg, datetime.datetime):
            return self._toDateTime() < arg
        else:
            return NotImplemented

    def __le__(self, arg):
        if isinstance(arg, AbsTime):
            return self.javaDate().getTime() <= arg.javaDate().getTime()
        elif isinstance(arg, datetime.datetime):
            return self._toDateTime() <= arg
        else:
            return NotImplemented

    def __gt__(self, arg):
        if isinstance(arg, AbsTime):
            return self.javaDate().getTime() > arg.javaDate().getTime()
        elif isinstance(arg, datetime.datetime):
            return self._toDateTime() > arg
        else:
            return NotImplemented

    def __ge__(self, arg):
        if isinstance(arg, AbsTime):
            return self.javaDate().getTime() >= arg.javaDate().getTime()
        elif isinstance(arg, datetime.datetime):
            return self._toDateTime() >= arg
        else:
            return NotImplemented

    def __eq__(self, arg):
        if isinstance(arg, AbsTime):
            return self.javaDate().getTime() == arg.javaDate().getTime()
        elif isinstance(arg, datetime.datetime):
            return self._toDateTime() == arg
        else:
            return NotImplemented

    def __ne__(self, arg):
        if isinstance(arg, AbsTime):
            return self.javaDate().getTime() != arg.javaDate().getTime()
        elif isinstance(arg, datetime.datetime):
            return self._toDateTime() != arg
        else:
            return NotImplemented

    def __hash__(self):
        # note this does not match the datetime hash, but now matches AWIPS 1 GFE
        return self.unixTime()

    def weekday(self):
        return self._weekday

    def javaDate(self):
        return self.__javaDate

    def toJavaObj(self):
        return self.javaDate()

    # AbsTime is always UTC
    def timetuple(self):
        return self.utctimetuple()

    def utctimetuple(self):
        dt = self._toDateTime()
        return dt.timetuple()

    def stringFmt(self, fmt):
        dt = self._toDateTime()
        return dt.strftime(fmt)

    def strftime(self, fmt):
        return self.stringFmt(fmt)

    def string(self):
        return self.stringFmt("%h %d %y %T GMT")

    def astimezone(self, tz=None):
        return self._toDateTime().astimezone(tz)

    def __str__(self):
        return str(self._toDateTime())

    def __repr__(self):
        return 'AbsTime({})'.format(self.unixTime())

    def tzname(self):
        return self._toDateTime().tzname()

    def replace(self, **kwargs):
        # this should probably return an AbsTime, but for backwards
        # compatibility we're returning a datetime
        newdt = self._toDateTime().replace(**kwargs)
        return newdt

    def timestamp(self):
        return self._toDateTime().timestamp()

    def date(self):
        return self._toDateTime().date()

    def time(self):
        return self._toDateTime().time()

    def timetz(self):
        return self._toDateTime().timetz()

    def ctime(self):
        return self._toDateTime().ctime()

    def isoformat(self, sep='T', timespec='auto'):
        return self._toDateTime().isoformat(sep, timespec)

    def utcoffset(self):
        return self._toDateTime().utcoffset()

    def dst(self):
        return self._toDateTime().dst()

    # we use properties to make these read-only
    @property
    def tzinfo(self):
        return self._tzinfo

    @property
    def year(self):
        return self._year

    @property
    def month(self):
        return self._month

    @property
    def day(self):
        return self._day

    @property
    def hour(self):
        return self._hour

    @property
    def minute(self):
        return self._minute

    @property
    def second(self):
        return self._second

    @property
    def microsecond(self):
        return self._microsecond


def absTimeYMD(year, month, day, hour=0, minute=0, second=0):
    tm = datetime.datetime(year, month, day, hour, minute, second)
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
