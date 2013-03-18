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
