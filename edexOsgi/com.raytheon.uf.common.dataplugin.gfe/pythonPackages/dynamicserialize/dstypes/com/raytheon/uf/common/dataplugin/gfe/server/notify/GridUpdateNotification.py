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

class GridUpdateNotification(GfeNotification.GfeNotification):

    def __init__(self):
        super(GridUpdateNotification, self).__init__()
        self.parmId = None
        self.replacementTimeRange = None
        self.workstationID = None
        self.histories = None

    def getParmId(self):
        return self.parmId

    def setParmId(self, parmId):
        self.parmId = parmId

    def getReplacementTimeRange(self):
        return self.replacementTimeRange

    def setReplacementTimeRange(self, replacementTimeRange):
        self.replacementTimeRange = replacementTimeRange

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
        msg += '\n' + "Replacement TimeRange: " + str(self.replacementTimeRange)
        msg += '\n' + "Histories: " + str(self.histories)
        return msg

