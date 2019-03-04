##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Jul 15, 2015     #4013        randerso       Initial creation (hand generated)
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import AbstractGfeRequest


class RsyncGridsToCWFRequest(AbstractGfeRequest):

    def __init__(self, siteId=None):
        super(RsyncGridsToCWFRequest, self).__init__()
        if siteId is not None:
            self.siteID = str(siteId)
