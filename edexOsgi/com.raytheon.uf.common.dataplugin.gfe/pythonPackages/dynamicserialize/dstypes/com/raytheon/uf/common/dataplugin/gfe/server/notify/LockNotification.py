##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/????       ????         njensen        Modified to add __repr__
#    06/22/2015       4573         randerso       Change to extend GfeNotification
#
##    

import GfeNotification

class LockNotification(GfeNotification.GfeNotification):

    def __init__(self):
        super(LockNotification, self).__init__()
        self.lockTable = None

    def getLockTable(self):
        return self.lockTable

    def setLockTable(self, lockTable):
        self.lockTable = lockTable

    def __str__(self):
        return str(self.lockTable)

