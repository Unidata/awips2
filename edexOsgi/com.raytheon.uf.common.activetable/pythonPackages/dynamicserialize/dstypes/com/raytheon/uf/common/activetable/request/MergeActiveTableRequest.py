##
##

# File auto-generated against equivalent DynamicSerialize Java class

class MergeActiveTableRequest(object):

    def __init__(self, incomingRecords=[], tableName='PRACTICE', site=None, 
                 timeOffset=0.0, xmlSource=None, fromIngestAT=False, 
                 makeBackups=True):
        self.incomingRecords = incomingRecords
        self.site = site
        self.tableName = tableName.upper() if tableName.upper() in ['OPERATIONAL', 'PRACTICE'] else 'PRACTICE'
        self.timeOffset = float(timeOffset)
        self.xmlSource = xmlSource
        self.fromIngestAT = bool(fromIngestAT)
        self.makeBackups = bool(makeBackups)
        
    def __repr__(self):
        retVal = "MergeActiveTableRequest(" 
        retVal += repr(self.incomingRecords) + ", "
        retVal += repr(self.tableName) + ", "
        retVal += repr(self.site) + ", "
        retVal += repr(self.timeOffset) + ", "
        retVal += repr(self.xmlSource) + ", " 
        retVal += repr(self.fromIngestAT) + ", "
        retVal += repr(self.makeBackups) + ")"
        return retVal
    
    def __str__(self):
        return self.__repr__()

    def getIncomingRecords(self):
        return self.incomingRecords

    def setIncomingRecords(self, incomingRecords):
        self.incomingRecords = incomingRecords

    def getTableName(self):
        return self.tableName

    def setTableName(self, tableName):
        value = tableName.upper()
        if value not in ['OPERATIONAL', 'PRACTICE']:
            raise ValueError("Invalid value " + tableName + " specified for ActiveTableMode.")
        self.tableName = value
        
    def getSite(self):
        return self.site

    def setSite(self, site):
        self.site = site

    def getTimeOffset(self):
        return self.timeOffset

    def setTimeOffset(self, timeOffset):
        self.timeOffset = float(timeOffset)

    def getXmlSource(self):
        return self.xmlSource

    def setXmlSource(self, xmlSource):
        self.xmlSource = xmlSource
        
    def getFromIngestAT(self):
        return self.fromIngestAT

    def setFromIngestAT(self, fromIngestAT):
        self.fromIngestAT = bool(fromIngestAT)
        
    def getMakeBackups(self):
        return self.makeBackups

    def setMakeBackups(self, makeBackups):
        self.makeBackups = bool(makeBackups)

