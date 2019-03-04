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

class GridHistoryUpdateNotification(GfeNotification.GfeNotification):

    def __init__(self):
        super(GridHistoryUpdateNotification, self).__init__()
        self.parmId = None
        self.workstationID = None
        self.histories = None

    def getParmId(self):
        return self.parmId

    def setParmId(self, parmId):
        self.parmId = parmId

    def getWorkstationID(self):
        return self.workstationID

    def setWorkstationID(self, workstationID):
        self.workstationID = workstationID

    def getHistories(self):
        return self.histories
    
    def setHistories(self, histories):
        self.histories = histories
        
    def __str__(self):
        msg = "ParmID: " + str(self.parmId)
        msg += '\n' + "Histories: " + str(self.histories)
        return msg

