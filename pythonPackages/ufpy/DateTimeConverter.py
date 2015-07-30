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

#
# Functions for converting between the various "Java" dynamic serialize types
# used by EDEX to the native python time datetime.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/24/15         #4480        dgilling       Initial Creation.
#

import datetime
import time

from dynamicserialize.dstypes.java.util import Date
from dynamicserialize.dstypes.java.sql import Timestamp
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange


MAX_TIME = pow(2, 31) - 1
MICROS_IN_SECOND = 1000000


def convertToDateTime(timeArg):
    """ 
    Converts the given object to a python datetime object. Supports native 
    python representations like datetime and struct_time, but also
    the dynamicserialize types like Date and Timestamp. Raises TypeError
    if no conversion can be performed.
    
    Args:
            timeArg: a python object representing a date and time. Supported
            types include datetime, struct_time, float, int, long and the
            dynamicserialize types Date and Timestamp.
    
    Returns:
        A datetime that represents the same date/time as the passed in object.
    """
    if isinstance(timeArg, datetime.datetime):
        return timeArg
    elif isinstance(timeArg, time.struct_time):
        return datetime.datetime(*timeArg[:6])
    elif isinstance(timeArg, float):
        # seconds as float, should be avoided due to floating point errors
        totalSecs = long(timeArg)
        micros = int((timeArg - totalSecs) * MICROS_IN_SECOND)
        return _convertSecsAndMicros(totalSecs, micros)
    elif isinstance(timeArg, (int, long)):
        # seconds as integer
        totalSecs = timeArg
        return _convertSecsAndMicros(totalSecs, 0)
    elif isinstance(timeArg, (Date, Timestamp)):
        totalSecs = timeArg.getTime()
        return _convertSecsAndMicros(totalSecs, 0)
    else:
        objType = str(type(timeArg))
        raise TypeError("Cannot convert object of type " + objType + " to datetime.")

def _convertSecsAndMicros(seconds, micros):
    if seconds < MAX_TIME:
        rval = datetime.datetime.utcfromtimestamp(seconds)
    else:
        extraTime = datetime.timedelta(seconds=(seconds - MAX_TIME))
        rval = datetime.datetime.utcfromtimestamp(MAX_TIME) + extraTime
    return rval.replace(microsecond=micros)

def constructTimeRange(*args):
    """ 
    Builds a python dynamicserialize TimeRange object from the given
    arguments.
    
    Args:
            args*: must be a TimeRange or a pair of objects that can be 
                   converted to a datetime via convertToDateTime().
    
    Returns:
        A TimeRange.
    """
    
    if len(args) == 1 and isinstance(args[0], TimeRange):
        return args[0]
    if len(args) != 2:
        raise TypeError("constructTimeRange takes exactly 2 arguments, " + str(len(args)) + " provided.")
    startTime = convertToDateTime(args[0])
    endTime = convertToDateTime(args[1])
    return TimeRange(startTime, endTime)
