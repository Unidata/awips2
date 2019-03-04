##
##

# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import GFERecord
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId


class GetGridRequest(object):

    def __init__(self, parmId=None, trs=[]):
        self.convertUnit = False
        self.records = []
        self.parmId = parmId
        if self.parmId is not None:
            for tr in trs:
                self.records.append(GFERecord(parmId, tr))

    def getRecords(self):
        return self.records

    def setRecords(self, records):
        self.records = records

    def getParmId(self):
        return self.parmId

    def setParmId(self, parmId):
        self.parmId = parmId

    def getConvertUnit(self):
        return self.convertUnit

    def setConvertUnit(self, convertUnit):
        self.convertUnit = convertUnit

