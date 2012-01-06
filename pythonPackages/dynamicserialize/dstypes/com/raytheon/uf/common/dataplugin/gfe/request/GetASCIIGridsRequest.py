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

class GetASCIIGridsRequest(object):

    def __init__(self):
        self.databaseIds = None
        self.parmIds = None
        self.timeRange = None
        self.coordConversionString = None
        self.workstationID = None
        self.siteID = None

    def getDatabaseIds(self):
        return self.databaseIds

    def setDatabaseIds(self, databaseIds):
        self.databaseIds = databaseIds

    def getParmIds(self):
        return self.parmIds

    def setParmIds(self, parmIds):
        self.parmIds = parmIds

    def getTimeRange(self):
        return self.timeRange

    def setTimeRange(self, timeRange):
        self.timeRange = timeRange

    def getCoordConversionString(self):
        return self.coordConversionString

    def setCoordConversionString(self, coordConversionString):
        self.coordConversionString = coordConversionString

    def getWorkstationID(self):
        return self.workstationID

    def setWorkstationID(self, workstationID):
        self.workstationID = workstationID

    def getSiteID(self):
        return self.siteID

    def setSiteID(self, siteID):
        self.siteID = siteID

