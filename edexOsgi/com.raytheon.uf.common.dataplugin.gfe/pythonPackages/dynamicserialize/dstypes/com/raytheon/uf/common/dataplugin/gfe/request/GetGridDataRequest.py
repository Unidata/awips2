##
##

# File auto-generated against equivalent DynamicSerialize Java class

import abc

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.request import GetGridRequest


class GetGridDataRequest(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __init__(self):
        self.requests = []
        self.workstationID = None
        self.siteID = None
    
    def addRequest(self, gridDataReq):
        if not isinstance(gridDataReq, GetGridRequest):
            raise TypeError("Invalid request specified: " + str(type(gridDataReq)) + \
                            ". Only GetGridRequests are supported.")
        else:
            self.requests.append(gridDataReq)

    def getRequests(self):
        return self.requests

    def setRequests(self, requests):
        del self.requests[:]
        for req in requests:
            self.addRequest(req)

    def getWorkstationID(self):
        return self.workstationID

    def setWorkstationID(self, workstationID):
        self.workstationID = workstationID

    def getSiteID(self):
        return self.siteID

    def setSiteID(self, siteID):
        self.siteID = siteID

