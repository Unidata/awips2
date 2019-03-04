##
##
import MockHazardUtils
import MockGridData
import MockSlice
import MockParm
from com.raytheon.uf.common.time import TimeRange as TR
import numpy as np
from StringIO import StringIO

jtr = None
msl = None
mgi = None
hazardUtils = None

def printvars():
    print "jtr = ", jtr
    print "msl = ", msl
    print "mgi = ", mgi
    print "hazardUtils = ", hazardUtils
    if hazardUtils is not None:
        print "hazardUtils.gridInfo = ", hazardUtils.gridInfo
