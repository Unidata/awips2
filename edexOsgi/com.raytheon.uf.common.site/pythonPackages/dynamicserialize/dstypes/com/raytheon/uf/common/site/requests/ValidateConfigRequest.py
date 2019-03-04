##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/12/16         #5888        dgilling       Initial  creation.
#
## 

### This class was manually created. DO NOT AUTOGENERATE ###

class ValidateConfigRequest(object):

    def __init__(self, siteID=None, plugin=None):
        self.siteID = siteID
        self.plugin = plugin

    def getSiteID(self):
        return self.siteID

    def setSiteID(self, siteID):
        self.siteID = siteID

    def getPlugin(self):
        return self.plugin

    def setPlugin(self, plugin):
        self.plugin = plugin
