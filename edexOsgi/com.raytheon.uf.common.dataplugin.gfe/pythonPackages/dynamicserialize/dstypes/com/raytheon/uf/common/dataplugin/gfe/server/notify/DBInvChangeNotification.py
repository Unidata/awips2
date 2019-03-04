##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/????       ????         njensen        Modified to add __repr__
#    06/22/2015       4573         randerso       Change to extend GfeNotification
#                                                 removed inventory methods
#
##    

import GfeNotification

class DBInvChangeNotification(GfeNotification.GfeNotification):

    def __init__(self):
        super(DBInvChangeNotification, self).__init__()
        self.additions = None
        self.deletions = None

    def getAdditions(self):
        return self.additions

    def setAdditions(self, additions):
        self.additions = additions

    def getDeletions(self):
        return self.deletions

    def setDeletions(self, deletions):
        self.deletions = deletions

    def __str__(self):
        msg = 'Additions' + str(self.additions) + '\n'
        msg += 'Deletions' + str(self.deletions)
        return msg

