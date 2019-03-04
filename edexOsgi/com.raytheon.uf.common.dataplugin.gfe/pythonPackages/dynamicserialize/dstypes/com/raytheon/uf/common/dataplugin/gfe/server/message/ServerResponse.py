##
##

# File auto-generated against equivalent DynamicSerialize Java class

class ServerResponse(object):

    def __init__(self):
        self.messages = None
        self.payload = None
        self.notifications = None

    def getMessages(self):
        return self.messages

    def setMessages(self, messages):
        self.messages = messages

    def getPayload(self):
        return self.payload

    def setPayload(self, payload):
        self.payload = payload

    def getNotifications(self):
        return self.notifications

    def setNotifications(self, notifications):
        self.notifications = notifications

    def isOkay(self):
        return (self.messages is None or len(self.messages) == 0)
    
    def message(self):
        if (self.isOkay()):
            return ""
        else:
            compMessage = ""
            for serverMsg in self.messages:
                compMessage += serverMsg.getMessage() + "\n"
            
            return compMessage
        
    def __str__(self):
        return self.message()
    
    def __nonzero__(self):
        return self.isOkay()