##
##

class GetActiveTableDictRequest(object):

    def __init__(self):
        self.requestedSiteId = None
        self.mode = None
        self.wfos = None

    def getRequestedSiteId(self):
        return self.requestedSiteId

    def setRequestedSiteId(self, requestedSiteId):
        self.requestedSiteId = requestedSiteId

    def getMode(self):
        return self.mode

    def setMode(self, mode):
        self.mode = mode

    def getWfos(self):
        return self.wfos
    
    def setWfos(self, wfos):
        self.wfos = wfos;
