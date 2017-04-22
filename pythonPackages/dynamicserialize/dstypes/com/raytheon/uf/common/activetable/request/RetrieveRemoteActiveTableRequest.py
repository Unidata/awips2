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

class RetrieveRemoteActiveTableRequest(object):

    def __init__(self, serverHost=None, serverPort=0, serverProtocol=None, 
                 mhsId=None, siteId=None, ancfAddress=None, bncfAddress=None, 
                 transmitScript=None):
        self.serverHost = serverHost
        self.serverPort = int(serverPort)
        self.serverProtocol = serverProtocol
        self.mhsId = mhsId
        self.siteId = siteId
        self.ancfAddress = ancfAddress
        self.bncfAddress = bncfAddress
        self.transmitScript = transmitScript
        
    def __repr__(self):
        retVal = "RetrieveRemoteActiveTableRequest(" 
        retVal += repr(self.serverHost) + ", "
        retVal += repr(self.serverPort) + ", " 
        retVal += repr(self.serverProtocol) + ", " 
        retVal += repr(self.mhsId) + ", " 
        retVal += repr(self.siteId) + ", " 
        retVal += repr(self.ancfAddress) + ", " 
        retVal += repr(self.bncfAddress) + ", " 
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
        self.serverPort = int(serverPort)

    def getServerProtocol(self):
        return self.serverProtocol

    def setServerProtocol(self, serverProtocol):
        self.serverProtocol = serverProtocol

    def getMhsId(self):
        return self.mhsId

    def setMhsId(self, mhsId):
        self.mhsId = mhsId

    def getSiteId(self):
        return self.siteId

    def setSiteId(self, siteId):
        self.siteId = siteId

    def getAncfAddress(self):
        return self.ancfAddress

    def setAncfAddress(self, ancfAddress):
        self.ancfAddress = ancfAddress

    def getBncfAddress(self):
        return self.bncfAddress

    def setBncfAddress(self, bncfAddress):
        self.bncfAddress = bncfAddress

    def getTransmitScript(self):
        return self.transmitScript

    def setTransmitScript(self, transmitScript):
        self.transmitScript = transmitScript

