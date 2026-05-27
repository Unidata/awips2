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

class SmartInitRequest(object):

    def __init__(self):
        self.moduleName = None
        self.modelTime = None
        self.calculateAll = None
        self.workstationID = None
        self.siteID = None

    def getModuleName(self):
        return self.moduleName

    def setModuleName(self, moduleName):
        self.moduleName = moduleName

    def getModelTime(self):
        return self.modelTime

    def setModelTime(self, modelTime):
        self.modelTime = modelTime

    def getCalculateAll(self):
        return self.calculateAll

    def setCalculateAll(self, calculateAll):
        self.calculateAll = calculateAll

    def getWorkstationID(self):
        return self.workstationID

    def setWorkstationID(self, workstationID):
        self.workstationID = workstationID

    def getSiteID(self):
        return self.siteID

    def setSiteID(self, siteID):
        self.siteID = siteID

