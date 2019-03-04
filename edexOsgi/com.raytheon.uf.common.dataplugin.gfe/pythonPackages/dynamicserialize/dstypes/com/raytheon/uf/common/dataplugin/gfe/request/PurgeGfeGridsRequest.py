##
##

# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import AbstractGfeRequest

class PurgeGfeGridsRequest(AbstractGfeRequest):

    def __init__(self):
        super(PurgeGfeGridsRequest, self).__init__()
        self.databaseID = None
        
    def __str__(self):
        retVal = "PurgeGfeGridsRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "databaseID: " + str(self.databaseID) + "]"
        return retVal

    def getDatabaseID(self):
        return self.databaseID

    def setDatabaseID(self, databaseID):
        self.databaseID = databaseID
