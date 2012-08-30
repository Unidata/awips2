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

from ufpy import ThriftClient

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import CommitGridsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetGridInventoryRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetParmListRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetSelectTimeRangeRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.request import CommitGridRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import GetActiveSitesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.message import ServerResponse


#
# Provides a Python-based interface for executing GFE requests.
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/26/12                      dgilling       Initial Creation.
#    
# 
#


class IFPClient(object):
    def __init__(self, host, port, user, site=None, progName=None):
        self.__thrift = ThriftClient.ThriftClient(host, port)
        self.__wsId = WsId(userName=user, progName=progName)
        # retrieve default site
        if site is None:
            sr = self.getSiteID()
            if len(sr.getPayload()) > 0:
                site = sr.getPayload()[0]
        self.__siteId = site
        
    def commitGrid(self, request):
        if type(request) is CommitGridRequest:
            return self.__commitGrid([request])
        elif self.__isHomogenousIterable(request, CommitGridRequest):
            return self.__commitGrid([cgr for cgr in request])
        raise TypeError("Invalid type: " + str(type(request)) + " specified to commitGrid(). Only accepts CommitGridRequest or lists of CommitGridRequest.")
    
    def __commitGrid(self, requests):
        ssr = ServerResponse()
        request = CommitGridsRequest()
        request.setCommits(requests)
        sr = self.__makeRequest(request)
        ssr.setMessages(sr.getMessages())
        return ssr
    
    def getParmList(self, id):
        argType = type(id)
        if argType is DatabaseID:
            return self.__getParmList([id])
        elif self.__isHomogenousIterable(id, DatabaseID):
            return self.__getParmList([dbid for dbid in id])
        raise TypeError("Invalid type: " + str(argType) + " specified to getParmList(). Only accepts DatabaseID or lists of DatabaseID.")        
    
    def __getParmList(self, ids):
        ssr = ServerResponse()
        request = GetParmListRequest()
        request.setDbIds(ids)
        sr = self.__makeRequest(request)
        ssr.setMessages(sr.getMessages())
        list = sr.getPayload() if sr.getPayload() is not None else []
        ssr.setPayload(list)
        return ssr
    
    def __isHomogenousIterable(self, iterable, classType):
        try:
            iterator = iter(iterable)
            for item in iterator:
                if not isinstance(item, classType):
                    return False
        except TypeError:
            return False
        return True
    
    def getGridInventory(self, parmID):
        if type(parmID) is ParmID:
            sr = self.__getGridInventory([parmID])
            list = []
            try:
                list = sr.getPayload()[parmID]
            except KeyError:
                # no-op, we've already default the TimeRange list to empty
                pass
            sr.setPayload(list)
            return sr
        elif self.__isHomogenousIterable(parmID, ParmID):
            return self.__getGridInventory([id for id in parmID])
        raise TypeError("Invalid type: " + str(type(parmID)) + " specified to getGridInventory(). Only accepts ParmID or lists of ParmID.")
    
    def __getGridInventory(self, parmIDs):
        ssr = ServerResponse()
        request = GetGridInventoryRequest()
        request.setParmIds(parmIDs)
        sr = self.__makeRequest(request)
        ssr.setMessages(sr.getMessages())
        trs = sr.getPayload() if sr.getPayload() is not None else {}
        ssr.setPayload(trs)
        return ssr
    
    def getSelectTR(self, name):
        request = GetSelectTimeRangeRequest()
        request.setName(name)
        sr = self.__makeRequest(request)
        ssr = ServerResponse()
        ssr.setMessages(sr.getMessages())
        ssr.setPayload(sr.getPayload())
        return ssr
        
    def getSiteID(self):
        ssr = ServerResponse()
        request = GetActiveSitesRequest()
        sr = self.__makeRequest(request)
        ssr.setMessages(sr.getMessages())
        ids = sr.getPayload() if sr.getPayload() is not None else []
        sr.setPayload(ids)
        return sr
    
    def __makeRequest(self, request):
        try:
            request.setSiteID(self.__siteId)
        except AttributeError:
            pass
        try:
            request.setWorkstationID(self.__wsId)
        except AttributeError:
            pass
        
        sr = ServerResponse()
        response = None
        try:
            response = self.__thrift.sendRequest(request)
        except ThriftClient.ThriftRequestException as e:
            sr.setMessages([str(e)])
        try:
            sr.setPayload(response.getPayload())
        except AttributeError:
            sr.setPayload(response)
        try:
            sr.setMessages(response.getMessages())
        except AttributeError:
            # not a server response, nothing else to do
            pass
            
        return sr
