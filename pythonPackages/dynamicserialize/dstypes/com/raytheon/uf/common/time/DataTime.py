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
import numpy
import time

from dynamicserialize.dstypes.java.util import Date
from dynamicserialize.dstypes.java.util import EnumSet
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange


class DataTime(object):

    def __init__(self, refTime=None, fcstTime=None, validPeriod=None):
        self.fcstTime = int(fcstTime) if fcstTime is not None else 0
        self.refTime = refTime if refTime is not None else None
        if validPeriod is not None and type(validPeriod) is not TimeRange:
            ValueError("Invalid validPeriod object specified for DataTime.")
        self.validPeriod = validPeriod if validPeriod is not None else None
        self.utilityFlags = EnumSet('com.raytheon.uf.common.time.DataTime$FLAG')
        # Only way to get a Double type to Java is to use numpy.float64 type,
        # which means we need to create an array to hold this single Double value.
        # FIXME: Find a better way to store doubles
        self.levelValue = numpy.array([-1.0], dtype=numpy.float64)
        
        if self.refTime is not None:
            if isinstance(self.refTime, datetime.datetime):
                self.refTime = long(calendar.timegm(self.refTime.utctimetuple()) * 1000) 
            elif isinstance(self.refTime, time.struct_time):
                self.refTime = long(calendar.timegm(self.refTime) * 1000)
            else:
                self.refTime = long(refTime)
            dateObj = Date()
            dateObj.setTime(self.refTime)
            self.refTime = dateObj
            
            if self.validPeriod is None:
                validTimeMillis = self.refTime + long(fcstTime * 1000)
                self.validPeriod = TimeRange()
                self.validPeriod.setStart(validTimeMills/1000)
                self.validPeriod.setEnd(validTimeMills/1000)
                
        # figure out utility flags
        if fcstTime is not None:
            self.utilityFlags.add("FCST_USED")
        if self.validPeriod.getStart() != self.validPeriod.getEnd():
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
        return self.levelValue[0]

    def setLevelValue(self, levelValue):
        self.levelValue[0] = levelValue

