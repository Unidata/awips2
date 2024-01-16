import os
from awips import ThriftClient
#from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage.records import ByteDataRecord
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.pgen.request import StoreActivityRequest
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.pgen import ResponseMessageValidate


class ProductStorer:
    """ Store a PGEN Activity (XML) to EDEX."""

    def __init__(self, activityInfo, activityXML):
         self.activityInfo = activityInfo
         self.activityXML = activityXML
         self.host = os.getenv("DEFAULT_HOST", "localhost")
         self.port = os.getenv("DEFAULT_PORT", "9581")
         self.client = ThriftClient.ThriftClient(self.host, self.port)

    def storeActivity(self):
         """ Sends ThriftClient request to store Activity."""
         req = StoreActivityRequest()
         req.setActivityInfo(self.activityInfo)
         req.setActivityXML(self.activityXML)
         resp = self.client.sendRequest(req)
         if resp.getResult:
             return resp.getDataURI()
         else:
             return None
             
