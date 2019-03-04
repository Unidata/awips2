##
##

# File auto-generated against equivalent DynamicSerialize Java class

class CommitGridRequest(object):

    def __init__(self):
        self.parmId = None
        self.dbId = None
        self.timeRange = None
        self.clientSendStatus = False

    def getParmId(self):
        return self.parmId

    def setParmId(self, parmId):
        self.parmId = parmId

    def getDbId(self):
        return self.dbId

    def setDbId(self, dbId):
        self.dbId = dbId

    def getTimeRange(self):
        return self.timeRange

    def setTimeRange(self, timeRange):
        self.timeRange = timeRange

    def getClientSendStatus(self):
        return self.clientSendStatus

    def setClientSendStatus(self, clientSendStatus):
        self.clientSendStatus = bool(clientSendStatus)

