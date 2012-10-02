##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
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