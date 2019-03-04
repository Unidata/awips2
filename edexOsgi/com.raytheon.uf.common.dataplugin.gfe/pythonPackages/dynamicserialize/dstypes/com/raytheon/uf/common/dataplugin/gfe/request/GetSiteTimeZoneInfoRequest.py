##
##

##
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    xx/xx/??                      dgilling       Initial Creation.
#    12/02/15         5129         dgilling       Refactor based on AbstractGfeRequest.
# 
##

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import AbstractGfeRequest

class GetSiteTimeZoneInfoRequest(AbstractGfeRequest):

    def __init__(self):
        super(GetSiteTimeZoneInfoRequest, self).__init__()
        self.requestedSiteIDs = None

    def getRequestedSiteIDs(self):
        return self.requestedSiteIDs

    def setRequestedSiteIDs(self, requestedSiteIDs):
        self.requestedSiteIDs = requestedSiteIDs

