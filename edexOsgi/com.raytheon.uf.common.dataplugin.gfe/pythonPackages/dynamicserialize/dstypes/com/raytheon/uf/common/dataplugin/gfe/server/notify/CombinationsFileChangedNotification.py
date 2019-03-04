##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/22/2015       4573         randerso       Initial creation (hand generated)
#
##    

import GfeNotification

class CombinationsFileChangedNotification(GfeNotification.GfeNotification):

    def __init__(self):
        super(CombinationsFileChangedNotification, self).__init__()
        self.combinationsFileName = None
        self.whoChanged = None

    def __str__(self):
        msg = "fileName: " + str(self.combinationsFileName)
        msg += '\n' + "whoChanged: " + str(self.whoChanged)
        return msg

    def getCombinationsFileName(self):
        return self.combinationsFileName
    
    def setCombinationsFileName(self, combinationsFileName):
        self.combinationsFileName = combinationsFileName
    
    def getWhoChanged(self):
        return self.whoChanged
    
    def setWhoChanged(self, whoChanged):
        self.whoChanged = whoChanged
