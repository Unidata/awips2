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
# 
#      SOFTWARE HISTORY
# 
#     Date            Ticket#       Engineer       Description
#     ------------    ----------    -----------    --------------------------
#     Aug 19, 2014                  nabowle        Generated

import numpy

class GetRadarDataRecordRequest(object):

    def __init__(self):
        self.timeRange = None
        self.productCode = None
        self.radarId = None
        self.primaryElevationAngle = None

    def getTimeRange(self):
        return self.timeRange

    def setTimeRange(self, timeRange):
        self.timeRange = timeRange

    def getProductCode(self):
        return self.productCode

    def setProductCode(self, productCode):
        self.productCode = productCode

    def getRadarId(self):
        return self.radarId

    def setRadarId(self, radarId):
        self.radarId = radarId

    def getPrimaryElevationAngle(self):
        return self.primaryElevationAngle

    def setPrimaryElevationAngle(self, primaryElevationAngle):
        self.primaryElevationAngle = numpy.float64(primaryElevationAngle)

