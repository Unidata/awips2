# #
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
# #

# File auto-generated against equivalent DynamicSerialize Java class
# and then modified post-generation to add additional features to better
# match Java implementation.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/??                      xxxxxxxx       Initial Creation.
#    05/28/13         2023         dgilling       Implement __str__().
#    01/22/14         2667         bclement       preserved milliseconds in string representation
#    03/03/14         2673         bsteffen       allow construction using a Date for refTime
#    06/24/14         3096         mnash          implement __cmp__
#    06/24/15         4480         dgilling       implement __hash__ and __eq__,
#                                                 replace __cmp__ with rich comparison
#                                                 operators.
#    05/26/16         2416         rjpeter        Added str based constructor.
#    08/02/16         2416         tgurney        Forecast time regex bug fix,
#                                                 plus misc cleanup


import calendar
import datetime
import numpy
import re
import StringIO
import time

from dynamicserialize.dstypes.java.util import Date
from dynamicserialize.dstypes.java.util import EnumSet

from TimeRange import TimeRange

_DATE=r'(\d{4}-\d{2}-\d{2})'
_TIME=r'(\d{2}:\d{2}:\d{2})'
_MILLIS='(?:\.(\d{1,3})(?:\d{1,4})?)?' # might have microsecond but that is thrown out
REFTIME_PATTERN_STR=_DATE + '[ _]' + _TIME + _MILLIS
FORECAST_PATTERN_STR=r'(?:[ _]\((\d+)(?::(\d{1,2}))?\))?'
VALID_PERIOD_PATTERN_STR=r'(?:\['+ REFTIME_PATTERN_STR + '--' + REFTIME_PATTERN_STR + r'\])?'
STR_PATTERN=re.compile(REFTIME_PATTERN_STR + FORECAST_PATTERN_STR + VALID_PERIOD_PATTERN_STR)

class DataTime(object):

    def __init__(self, refTime=None, fcstTime=None, validPeriod=None):
        """
        Construct a new DataTime.
        May also be called as DataTime(str) to parse a string and create a
        DataTime from it. Some examples of valid DataTime strings:

             '2016-08-02 01:23:45.0'
             '2016-08-02 01:23:45.123'
             '2016-08-02 01:23:45.0 (17)',
             '2016-08-02 01:23:45.0 (17:34)'
             '2016-08-02 01:23:45.0[2016-08-02_02:34:45.0--2016-08-02_03:45:56.0]'
             '2016-08-02 01:23:45.456_(17:34)[2016-08-02_02:34:45.0--2016-08-02_03:45:56.0]'
        """
        if fcstTime is not None:
            self.fcstTime = int(fcstTime)
        else:
            self.fcstTime = 0
        self.refTime = refTime
        if validPeriod is not None and type(validPeriod) is not TimeRange:
            raise ValueError("Invalid validPeriod object specified for DataTime.")
        self.validPeriod = validPeriod
        self.utilityFlags = EnumSet('com.raytheon.uf.common.time.DataTime$FLAG')
        self.levelValue = numpy.float64(-1.0)

        if self.refTime is not None:
            if isinstance(self.refTime, datetime.datetime):
                self.refTime = long(calendar.timegm(self.refTime.utctimetuple()) * 1000)
            elif isinstance(self.refTime, time.struct_time):
                self.refTime = long(calendar.timegm(self.refTime) * 1000)
            elif hasattr(self.refTime, 'getTime'):
                # getTime should be returning ms, there is no way to check this
                # This is expected for java Date
                self.refTime = long(self.refTime.getTime())
            else:
                try:
                    self.refTime = long(self.refTime)
                except ValueError:
                    # Assume first arg is a string. Attempt to parse.
                    match = STR_PATTERN.match(self.refTime)
                    if match is None:
                        raise ValueError('Could not parse DataTime info from '
                                         + str(refTime))

                    groups = match.groups()
                    rDate = groups[0]
                    rTime = groups[1]
                    rMillis = groups[2] or 0
                    fcstTimeHr = groups[3]
                    fcstTimeMin = groups[4]
                    periodStart = groups[5], groups[6], (groups[7] or 0)
                    periodEnd = groups[8], groups[9], (groups[10] or 0)
                    self.refTime = self._getTimeAsEpochMillis(rDate, rTime, rMillis)

                    if fcstTimeHr is not None:
                        self.fcstTime = long(fcstTimeHr) * 3600
                        if fcstTimeMin is not None:
                            self.fcstTime += long(fcstTimeMin) * 60

                    if periodStart[0] is not None:
                        self.validPeriod = TimeRange()
                        periodStartTime = self._getTimeAsEpochMillis(*periodStart)
                        self.validPeriod.setStart(periodStartTime / 1000)
                        periodEndTime = self._getTimeAsEpochMillis(*periodEnd)
                        self.validPeriod.setEnd(periodEndTime / 1000)

            self.refTime = Date(self.refTime)

            if self.validPeriod is None:
                validTimeMillis = self.refTime.getTime() + long(self.fcstTime * 1000)
                self.validPeriod = TimeRange()
                self.validPeriod.setStart(validTimeMillis / 1000)
                self.validPeriod.setEnd(validTimeMillis / 1000)

        # figure out utility flags
        if self.fcstTime:
            self.utilityFlags.add("FCST_USED")
        if self.validPeriod and self.validPeriod.isValid():
            self.utilityFlags.add("PERIOD_USED")

    def getRefTime(self):
        return self.refTime

    def setRefTime(self, refTime):
        self.refTime = refTime

    def getFcstTime(self):
        return self.fcstTime

    def setFcstTime(self, fcstTime):
        self.fcstTime = fcstTime

    def getValidPeriod(self):
        return self.validPeriod

    def setValidPeriod(self, validPeriod):
        self.validPeriod = validPeriod

    def getUtilityFlags(self):
        return self.utilityFlags

    def setUtilityFlags(self, utilityFlags):
        self.utilityFlags = utilityFlags

    def getLevelValue(self):
        return self.levelValue

    def setLevelValue(self, levelValue):
        self.levelValue = numpy.float64(levelValue)

    def __str__(self):
        buffer = StringIO.StringIO()

        if self.refTime is not None:
            refTimeInSecs = self.refTime.getTime() / 1000
            micros = (self.refTime.getTime() % 1000) * 1000
            dtObj = datetime.datetime.utcfromtimestamp(refTimeInSecs)
            dtObj = dtObj.replace(microsecond=micros)
            # This won't be compatible with java or string from java since its to microsecond
            buffer.write(dtObj.isoformat(' '))

        if "FCST_USED" in self.utilityFlags:
            hrs = int(self.fcstTime / 3600)
            mins = int((self.fcstTime - (hrs * 3600)) / 60)
            buffer.write(" (" + str(hrs))
            if mins != 0:
                buffer.write(":" + str(mins))
            buffer.write(")")

        if "PERIOD_USED" in self.utilityFlags:
            buffer.write("[")
            buffer.write(self.validPeriod.start.isoformat(' '))
            buffer.write("--")
            buffer.write(self.validPeriod.end.isoformat(' '))
            buffer.write("]")

        strVal = buffer.getvalue()
        buffer.close()
        return strVal

    def __repr__(self):
        return "<DataTime instance: " + str(self) + " >"

    def __hash__(self):
        hashCode = hash(self.refTime) ^ hash(self.fcstTime)
        if self.validPeriod is not None and self.validPeriod.isValid():
            hashCode ^= hash(self.validPeriod.getStart())
            hashCode ^= hash(self.validPeriod.getEnd())
        hashCode ^= hash(self.levelValue)
        return hashCode

    def __eq__(self, other):
        if type(self) != type(other):
            return False

        if other.getRefTime() is None:
            return self.fcstTime == other.fcstTime

        dataTime1 = (self.refTime, self.fcstTime, self.validPeriod, self.levelValue)
        dataTime2 = (other.refTime, other.fcstTime, other.validPeriod, other.levelValue)
        return dataTime1 == dataTime2

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if type(self) != type(other):
            return NotImplemented

        myValidTime = self.getRefTime().getTime() + self.getFcstTime()
        otherValidTime = other.getRefTime().getTime() + other.getFcstTime()
        if myValidTime < otherValidTime:
            return True

        if self.fcstTime < other.fcstTime:
            return True

        if self.levelValue < other.levelValue:
            return True

        myValidPeriod = self.validPeriod
        otherValidPeriod = other.validPeriod
        if myValidPeriod != otherValidPeriod:
            if myValidPeriod.duration() < otherValidPeriod.duration():
                return True
            return myValidPeriod.getStartInMillis() < otherValidPeriod.getStartInMillis()
        return False

    def __le__(self, other):
        if type(self) != type(other):
            return NotImplemented

        return self.__lt__(other) or self.__eq__(other)

    def __gt__(self, other):
        if type(self) != type(other):
            return NotImplemented

        myValidTime = self.getRefTime().getTime() + self.getFcstTime()
        otherValidTime = other.getRefTime().getTime() + other.getFcstTime()
        if myValidTime > otherValidTime:
            return True

        if self.fcstTime > other.fcstTime:
            return True

        if self.levelValue > other.levelValue:
            return True

        myValidPeriod = self.validPeriod
        otherValidPeriod = other.validPeriod
        if myValidPeriod != otherValidPeriod:
            if myValidPeriod.duration() > otherValidPeriod.duration():
                return True
            return myValidPeriod.getStartInMillis() > otherValidPeriod.getStartInMillis()
        return False

    def __ge__(self, other):
        if type(self) != type(other):
            return NotImplemented

        return self.__gt__(other) or self.__eq__(other)

    def _getTimeAsEpochMillis(self, dateStr, timeStr, millis):
        t = time.strptime(dateStr + ' ' + timeStr, '%Y-%m-%d %H:%M:%S')
        epochSeconds = calendar.timegm(t)
        return long(epochSeconds * 1000) + long(millis)
