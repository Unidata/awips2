##
##

# File auto-generated against equivalent DynamicSerialize Java class

class StatusResponse(object):

    def __init__(self):
        self.hostname = None
        self.jvmName = None
        self.statistics = None

    def getHostname(self):
        return self.hostname

    def setHostname(self, hostname):
        self.hostname = hostname

    def getJvmName(self):
        return self.jvmName

    def setJvmName(self, jvmName):
        self.jvmName = jvmName

    def getStatistics(self):
        return self.statistics

    def setStatistics(self, statistics):
        self.statistics = statistics
    
    def __repr__(self):
        return self.hostname + ':' + self.jvmName

