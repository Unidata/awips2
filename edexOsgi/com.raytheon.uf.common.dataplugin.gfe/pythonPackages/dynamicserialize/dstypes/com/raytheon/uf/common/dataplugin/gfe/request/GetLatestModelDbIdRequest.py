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


class GetLatestModelDbIdRequest(AbstractGfeRequest):

    def __init__(self, siteId=None, modelName=None):
        super(GetLatestModelDbIdRequest, self).__init__()
        if siteId is not None:
            self.siteID = str(siteId)
        if modelName is not None:
            self.modelName = str(modelName)
        
    def __str__(self):
        retVal = "GetLatestModelDbIdRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "modelName: " + str(self.modelName) + "]"
        return retVal
    
    def __repr__(self):
        retVal = "ExecuteIfpNetCDFGridRequest(" 
        retVal += "wokstationID=" + repr(self.workstationID) + ", "
        retVal += "siteID=" + repr(self.siteID) + ", "
        retVal += "modelName=" + repr(self.modelName) + ")"
        return retVal

    def getModelName(self):
        return self.modelName

    def setModelName(self, modelName):
        self.modelName = str(modelName)
