##
##

##
# This is a base file that is not intended to be overridden.
##

##
# uengine is deprecated and will be removed from the system soon. Migrate your
# apps to using the Data Access Framework (DAF).
##

#
# Gfe Task script 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/18/08        #875          bphillip      Initial Creation.
#    
# 



from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


class GfeTask():

    def __init__(self, wsId, task):
        self.wsId = wsId
        self.task = task

    def addArgument(self, xml):
        self.task.addXml(xml)

    def execute(self):
        self.task.setWorkstationID(self.wsId)

        result = self.task.execute()
        payload = result.getPayload()
        messages = result.getMessages()
        notifications = result.getNotifications()
        response = ArrayList()

        if payload is not None:
            for i in range(payload.size()):                
                response.add(ResponseMessageGeneric(payload.get(i)))

        for i in range(messages.size()):
            response.add(ResponseMessageGeneric(messages.get(i)))

        for i in range(notifications.size()):
            response.add(ResponseMessageGeneric(notifications.get(i)))        

        return response
    