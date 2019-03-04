##
##
from AbsTime import current
from AbsTime import AbsTime
from TimeRange import TimeRange

def testAbsTimeZero():
    start = AbsTime(0)
    end = current()
    timeRange = TimeRange(start, end)
    javaTR = timeRange.toJavaObj()
    return javaTR