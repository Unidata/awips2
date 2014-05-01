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

class SendActiveTableRequest(object):

    def __init__(self, serverHost=None, serverPort=None, serverProtocol=None, 
                 serverSite=None, mhsId=None, sites=None, filterSites=None, 
                 mhsSites=None, issueTime=None, countDict=None, fileName=None, 
                 xmlIncoming=None, transmitScript=None):
        self.serverHost = serverHost
        self.serverPort = None if serverPort is None else int(serverPort)
        self.serverProtocol = serverProtocol
        self.serverSite = serverSite
        self.mhsId = mhsId
        self.sites = sites if sites is not None else [] 
        self.filterSites = filterSites if filterSites is not None else []
        self.mhsSites = mhsSites if mhsSites is not None else []
        self.issueTime = None if issueTime is None else float(issueTime)
        self.countDict = countDict if countDict is not None else {}
        self.fileName = fileName
        self.xmlIncoming = xmlIncoming
        self.transmitScript = transmitScript
        
    def __repr__(self):
        retVal = "SendActiveTableRequest(" 
        retVal += repr(self.serverHost) + ", "
        retVal += repr(self.serverPort) + ", " 
        retVal += repr(self.serverProtocol) + ", " 
        retVal += repr(self.serverSite) + ", "
        retVal += repr(self.mhsId) + ", " 
        retVal += repr(self.sites) + ", " 
        retVal += repr(self.filterSites) + ", " 
        retVal += repr(self.mhsSites) + ", "
        retVal += repr(self.issueTime) + ", " 
        retVal += repr(self.countDict) + ", "
        retVal += repr(self.fileName) + ", " 
        retVal += repr(self.xmlIncoming) + ", "
        retVal += repr(self.transmitScript) + ")"
        return retVal
    
    def __str__(self):
        return self.__repr__()

    def getServerHost(self):
        return self.serverHost

    def setServerHost(self, serverHost):
        self.serverHost = serverHost

    def getServerPort(self):
        return self.serverPort

    def setServerPort(self, serverPort):
        self.serverPort = serverPort

    def getServerProtocol(self):
        return self.serverProtocol

    def setServerProtocol(self, serverProtocol):
        self.serverProtocol = serverProtocol

    def getServerSite(self):
        return self.serverSite

    def setServerSite(self, serverSite):
        self.serverSite = serverSite

    def getMhsId(self):
        return self.mhsId

    def setMhsId(self, mhsId):
        self.mhsId = mhsId

    def getSites(self):
        return self.sites

    def setSites(self, sites):
        self.sites = sites

    def getFilterSites(self):
        return self.filterSites

    def setFilterSites(self, filterSites):
        self.filterSites = filterSites

    def getMhsSites(self):
        return self.mhsSites

    def setMhsSites(self, mhsSites):
        self.mhsSites = mhsSites

    def getIssueTime(self):
        return self.issueTime

    def setIssueTime(self, issueTime):
        self.issueTime = issueTime

    def getCountDict(self):
        return self.countDict

    def setCountDict(self, countDict):
        self.countDict = countDict

    def getFileName(self):
        return self.fileName

    def setFileName(self, fileName):
        self.fileName = fileName

    def getXmlIncoming(self):
        return self.xmlIncoming

    def setXmlIncoming(self, xmlIncoming):
        self.xmlIncoming = xmlIncoming

    def getTransmitScript(self):
        return self.transmitScript

    def setTransmitScript(self, transmitScript):
        self.transmitScript = transmitScript

