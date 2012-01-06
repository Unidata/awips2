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
# Modified by njensen to add __repr__

import time

class DatabaseID(object):

    def __init__(self, dbIdentifier=None):
        self.siteId = None
        self.format = "NONE"
        self.dbType = None
        self.modelName = None
        self.modelTime = None
        self.modelId = None
        self.shortModelId = None
        if dbIdentifier is not None:
            if (self.__decodeIdentifier(dbIdentifier)):
                self.__encodeIdentifier()
            else:
                self.format = "NONE"
                self.dbType = ""
                self.siteId = ""
                self.modelName = ""
                self.modelTime = "00000000_0000"
                self.modelId = ""
                self.shortModelId = ""

    def isValid(self) :
        return self.format != "NONE";

    def getSiteId(self):
        return self.siteId

    def setSiteId(self, siteId):
        self.siteId = siteId

    def getFormat(self):
        return self.format

    def setFormat(self, format):
        self.format = format

    def getDbType(self):
        return self.dbType

    def setDbType(self, dbType):
        self.dbType = dbType

    def getModelName(self):
        return self.modelName

    def setModelName(self, modelName):
        self.modelName = modelName

    def getModelTime(self):
        return self.modelTime

    def setModelTime(self, modelTime):
        self.modelTime = modelTime

    def getModelId(self):
        return self.modelId

    def setModelId(self, modelId):
        self.modelId = modelId

    def getShortModelId(self):
        return self.shortModelId

    def setShortModelId(self, shortModelId):
        self.shortModelId = shortModelId
        
    def __encodeIdentifier(self):
        if self.dbType is not None:
            self.modelId = self.siteId + "_" + self.format + "_" + self.dbType + "_" + self.modelName
        else:
            self.modelId = self.siteId + "_" + self.format + "__" + self.modelName

        self.shortModelId = self.modelName
        if self.dbType is not None and self.dbType != "":
            self.shortModelId += "_" + self.dbType

        if self.modelTime != "00000000_0000":
            self.modelId += "_" + self.modelTime;
            self.shortModelId += "_" + self.modelTime[6:8] + self.modelTime[9:11]
        else:
            self.modelId += "_" + "00000000_0000"

        self.shortModelId += " (" + self.siteId + ")"
    
    def __decodeIdentifier(self, dbIdentifier):
        self.format = "NONE"
        self.dbType = ""
        self.siteId = ""
        self.modelName = ""
        self.modelTime = "00000000_0000"

        # parse into '_' separated strings
        strings = dbIdentifier.split("_");
        if len(strings) != 6:
            return False

        # store the data
        if strings[1] == "GRID":
            self.format = "GRID"
        else:
            return False

        self.siteId = strings[0]
        self.dbType = strings[2]
        self.modelName = strings[3]

        # date-time group
        if (len(strings[4]) != 8 or len(strings[5]) != 4):
            return False

        # make sure the digits are there
        dtg = strings[4] + '_' + strings[5]; # back together
        if dtg != "00000000_0000":
            if not self.__decodeDtg(dtg):
                return False

        return True
    
    @staticmethod
    def decodeDtg(dtgString):
        dateStruct = time.gmtime(0)
        try:
            dateStruct = time.strptime(dtgString, "%Y%m%d_%H%M")
        except:
            return (False, dateStruct)
        return (True, dateStruct)
    
    def __decodeDtg(self, dtgString):
        try:
            time.strptime(dtgString, "%Y%m%d_%H%M")
            self.modelTime = dtgString
        except:
            return False
        return True
    
    def getModelTimeAsDate(self):
        if self.modelTime == "00000000_0000":
            return time.gmtime(0)
        else:
            return time.strptime(self.modelTime, "%Y%m%d_%H%M")
    
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return self.modelId

    def __hash__(self):
        prime = 31;
        result = 1;
        result = prime * result + (0 if self.dbType is None else hash(self.dbType))
        result = prime * result + (0 if self.format is None else hash(self.format))
        result = prime * result + (0 if self.modelId is None else hash(self.modelId))
        result = prime * result + (0 if self.modelTime is None else hash(self.modelTime))
        result = prime * result + (0 if self.siteId is None else hash(self.siteId))
        return result;
    
    def __cmp__(self, other):
        if not isinstance(other, DatabaseID):
            siteComp = cmp(self.siteId, other.siteId)
            if siteComp != 0:
                return siteComp
            
            formatComp = cmp(self.format, other.format)
            if formatComp != 0:
                return formatComp
            
            typeComp = cmp(self.dbType, other.dbType)
            if typeComp != 0:
                return typeComp
            
            nameComp = cmp(self.modelName, other.modelName)
            if nameComp != 0:
                return nameComp
            
            return -cmp(self.getModelTimeAsDate(), other.getModelTimeAsDate())
        else:
            return NotImplemented

    def __eq__(self, other):
        if not isinstance(other, DatabaseID):
            return False
        return (str(self) == str(other))
    
    def __ne__(self, other):
        return (not self.__eq__(other))
