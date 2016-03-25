##
# This script is used to send GPD request to EDEX.
# 
# Users can override the default EDEX server and port name by specifying them
# in the $DEFAULT_HOST and $DEFAULT_PORT shell environment variables.
# 
# 5/22/2013 Chin J. Chen
##
import os
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.gpd.query import GenericPointDataReqMsg
from awips import ThriftClient


class GpdCliRequestHandler:
    """ Request Activity to EDEX."""

    def __init__(self):
         self.host = os.getenv("DEFAULT_HOST", "localhost")
         self.port = os.getenv("DEFAULT_PORT", "9581")
         self.client = ThriftClient.ThriftClient(self.host, self.port)

    def getGpdProdInfo(self, prodName, reqType):
         req = GenericPointDataReqMsg()
         req.setReqType(reqType)
         req.setProductName(prodName)
         resp = self.client.sendRequest(req)
         return resp
     
    def getGpdProduct(self, refTime,prodName , reqType,querySpecifiedProductVersion=None,versionNum=None ):
         req = GenericPointDataReqMsg()
         req.setReqType(reqType)
         req.setRefTime(refTime)
         req.setProductName(prodName)
         if (querySpecifiedProductVersion!= None and versionNum !=None):
             req.setQuerySpecifiedProductVersion(querySpecifiedProductVersion)
             req.setProductVersion(versionNum)
         resp = self.client.sendRequest(req)
         return resp

     
    def getGpdStationProduct(self, stnId, refTime,prodName , reqType,querySpecifiedProductVersion=None,versionNum=None ):
         req = GenericPointDataReqMsg()
         req.setReqType(reqType)
         req.setStnId(stnId)
         req.setRefTime(refTime)
         req.setProductName(prodName)
         if (querySpecifiedProductVersion!= None and versionNum !=None):
             req.setQuerySpecifiedProductVersion(querySpecifiedProductVersion)
             req.setProductVersion(versionNum)
         resp = self.client.sendRequest(req)
         return resp

    def getGpdMovingProduct(self, slat, slon, refTime,prodName , reqType,querySpecifiedProductVersion=None,versionNum=None ):
         req = GenericPointDataReqMsg()
         req.setReqType(reqType)
         req.setSlon(slon)
         req.setSlat(slat)
         req.setRefTime(refTime)
         req.setProductName(prodName)
         if (querySpecifiedProductVersion!= None and versionNum !=None):
             req.setQuerySpecifiedProductVersion(querySpecifiedProductVersion)
             req.setProductVersion(versionNum)
         resp = self.client.sendRequest(req)
         return resp

    def storeGpdGempakProduct(self, gpdGempak, prodName, maxNumLevel,versionNum):
        req = GenericPointDataReqMsg()
        req.setReqType("STORE_GPD_PRODUCT_FROM_GEMPAK_TBL")
        req.setGpdDataString(gpdGempak)
        req.setProductName(prodName)
        req.setProductVersion(versionNum)
        req.setMaxNumLevel(maxNumLevel)
        resp = self.client.sendRequest(req)
        return resp
 
    def storeGpdXmlProduct(self, gpdXml):
        req = GenericPointDataReqMsg()
        req.setReqType("STORE_GPD_PRODUCT_FROM_XML")
        req.setGpdDataString(gpdXml)
        resp = self.client.sendRequest(req)
        return resp
    
    def storeGpdProductInfo(self, gpdXml):
        req = GenericPointDataReqMsg()
        req.setReqType("STORE_GPD_PRODUCT_INFO_FROM_XML")
        req.setGpdDataString(gpdXml)
        resp = self.client.sendRequest(req)
        return resp
    '''
    def purgeGpdProd(self, prodName, refTime=None, all=None):
        req = GenericPointDataReqMsg()
        if(all == "yes"):
            req.setReqType("PURGE_GPD_PRODUCT_ALLTIME")
        else:
            req.setReqType("PURGE_GPD_PRODUCT_ONETIME")
            if(refTime!=None):
                req.setRefTime(refTime)
            else:
                return "reference time required for purging"
        req.setProductName(prodName)
        
        resp = self.client.sendRequest(req)
        return resp
    '''
    def purgeGpdExpired(self):
        req = GenericPointDataReqMsg()
        req.setReqType("PURGE_GPD_EXPIRED_PRODUCT")            
        resp = self.client.sendRequest(req)
        return resp
    
    def purgeGpdAll(self):
        req = GenericPointDataReqMsg()
        req.setReqType("PURGE_GPD_ALL_PRODUCTS")            
        resp = self.client.sendRequest(req)
        return resp
    
    