##
##

#
# A pure python implementation of com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest
# for use by the python implementation of DynamicSerialize.
#
# File auto-generated against equivalent DynamicSerialize Java class, but additional
# useful methods have been added.
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/05/13                      dgilling       Initial Creation.
#    
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import AbstractGfeRequest


class ExportGridsRequest(AbstractGfeRequest):

    def __init__(self):
        super(ExportGridsRequest, self).__init__()
        self.site = None
        self.mode = None

    def getSite(self):
        return self.site

    def setSite(self, site):
        self.site = site

    def getMode(self):
        return self.mode

    def setMode(self, mode):
        validValues = ['CRON', 'MANUAL', 'GRIB2']
        inputVal = str(mode).upper()
        if inputVal in validValues:
            self.mode = mode
        else:
            raise ValueError(inputVal + " not a valid ExportGridsMode value. Must be one of " + str(validValues))

    def __str__(self):
        retVal = "ExportGridsRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "site: " + str(self.site) + ", "
        retVal += "mode: " + str(self.mode) + "]"
        return retVal
    
    def __repr__(self):
        retVal = "ExportGridsRequest(" 
        retVal += "wokstationID=" + repr(self.workstationID) + ", "
        retVal += "siteID=" + repr(self.siteID) + ", "
        retVal += "site=" + repr(self.site) + ", "
        retVal += "mode=" + repr(self.mode) + ")"
        return retVal
