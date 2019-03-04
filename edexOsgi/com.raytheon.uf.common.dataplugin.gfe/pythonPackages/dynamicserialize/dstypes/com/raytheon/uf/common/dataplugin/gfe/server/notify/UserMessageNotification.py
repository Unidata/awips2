##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/22/2015       4573         randerso       Change to extend GfeNotification
#
##    

import GfeNotification

class UserMessageNotification(GfeNotification.GfeNotification):

    def __init__(self):
        super(UserMessageNotification, self).__init__()
        self.category = None
        self.priority = None
        self.message = None

    def getCategory(self):
        return self.category

    def setCategory(self, category):
        self.category = category

    def getPriority(self):
        return self.priority

    def setPriority(self, priority):
        self.priority = priority

    def getMessage(self):
        return self.message

    def setMessage(self, message):
        self.message = message

    def __str__(self):
        msg = 'Message: ' + str(self.message) + '\n'
        msg += 'Priority: ' + str(self.priority) + '\n'
        msg += 'Category: ' + str(self.category) + '\n'
        return msg

