##
##
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfeRequest import AbstractGfeRequest

# Manually updated
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Dec 06, 2016  6092     randerso  Initial Creation

class GfeClientRequest(AbstractGfeRequest):

    def __init__(self, script, siteID, configFile, user, args=[]):
        super(GfeClientRequest, self).__init__()
        self.script = script
        self.siteID = siteID
        self.configFile = configFile
        self.user = user
        self.args = args
        self.time = None

    def getConfigFile(self):
        return self.configFile

    def setConfigFile(self, configFile):
        self.configFile = configFile

    def getUser(self):
        return self.user

    def setUser(self, user):
        self.user = user

    def getArgs(self):
        return self.args

    def setArgs(self, args):
        self.args = args

    def getTime(self):
        return self.time

    def setTime(self, time):
        self.time = time

    def getScript(self):
        return self.script

    def setScript(self, script):
        self.script = script

    def __str__(self):
        retval = "GfeClientRequest("
        retval += "siteID:" + self.siteID + ", "
        retval += "script:" + self.script + ", "
        retval += "configFile:" + self.configFile + ", "
        retval += "user:" + self.user + ", "
        if self.time:
            retval += "time:" + str(self.time) + ", "
        retval += "args:" + str(self.args) + ")"
        return retval
