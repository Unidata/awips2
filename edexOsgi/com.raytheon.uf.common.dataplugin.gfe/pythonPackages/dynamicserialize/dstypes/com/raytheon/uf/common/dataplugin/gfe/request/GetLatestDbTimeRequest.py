##
##

# File auto-generated against equivalent DynamicSerialize Java class
# and then modified post-generation to use AbstractGfeRequest and
# implement str(), repr()
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/22/13         2025         dgilling       Initial Creation.
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import AbstractGfeRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID


class GetLatestDbTimeRequest(AbstractGfeRequest):

    def __init__(self, dbId=None):
        super(GetLatestDbTimeRequest, self).__init__()
        if dbId is not None and isinstance(dbId, DatabaseID):
            self.dbId = dbId
            self.siteID = dbId.getSiteId()
        elif dbId is not None and not isinstance(dbId, DatabaseID):
            raise TypeError(
                "Attempt to construct GetLatestDbTimeRequest without providing a valid DatabaseID.")
        
    def __str__(self):
        retVal = "GetLatestDbTimeRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "dbId: " + str(self.dbId) + "]"
        return retVal
    
    def __repr__(self):
        retVal = "ExecuteIfpNetCDFGridRequest(" 
        retVal += "wokstationID=" + repr(self.workstationID) + ", "
        retVal += "siteID=" + repr(self.siteID) + ", "
        retVal += "dbId=" + repr(self.dbId) + ")"
        return retVal

    def getDbId(self):
        return self.dbId

    def setDbId(self, dbId):
        if isinstance(dbId, DatabaseID):
            self.dbId = dbId
        else:
            raise TypeError(
                "Attempt to call GetLatestDbTimeRequest.setDbId() without providing a valid DatabaseID.")
