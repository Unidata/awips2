##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/07/2014       3684         randerso       Manually updated to add sourceID
#
##    
import abc

class GfeNotification(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __init__(self):
        self.siteID = None
        self.sourceID = None

    def getSiteID(self):
        return self.siteID

    def setSiteID(self, siteID):
        self.siteID = siteID


    def getSourceID(self):
        return self.sourceID

    def setSourceID(self, sourceID):
        self.sourceID = sourceID
