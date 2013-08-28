
# File auto-generated against equivalent DynamicSerialize Java class

class GenericPointDataReqMsg(object):

    def __init__(self):
        self.reqType = None
        self.refTime = None
        self.productName = None
        self.stnId = None
        self.slat = None
        self.slon = None
        self.productVersion = None
        self.querySpecifiedProductVersion = False
        self.queryKey = None
        self.gpdDataString = None
        self.maxNumLevel = 1
        
    def getReqType(self):
        return self.reqType

    def setReqType(self, reqType):
        self.reqType = reqType

    def getRefTime(self):
        return self.refTime

    def setRefTime(self, refTime):
        self.refTime = refTime

    def getProductName(self):
        return self.productName

    def setProductName(self, productName):
        self.productName = productName

    def getStnId(self):
        return self.stnId

    def setStnId(self, stnId):
        self.stnId = stnId

    def getSlat(self):
        return self.slat

    def setSlat(self, slat):
        self.slat = slat

    def getSlon(self):
        return self.slon

    def setSlon(self, slon):
        self.slon = slon

    def getMaxNumLevel(self):
        return self.maxNumLevel

    def setMaxNumLevel(self, maxNumLevel):
        self.maxNumLevel = maxNumLevel

    def getProductVersion(self):
        return self.productVersion

    def setProductVersion(self, productVersion):
        self.productVersion = productVersion

    def getQuerySpecifiedProductVersion(self):
        return self.querySpecifiedProductVersion

    def setQuerySpecifiedProductVersion(self, querySpecifiedProductVersion):
        self.querySpecifiedProductVersion = querySpecifiedProductVersion

    def getQueryKey(self):
        return self.queryKey

    def setQueryKey(self, queryKey):
        self.queryKey = queryKey

    def getGpdDataString(self):
        return self.gpdDataString
    
    def setGpdDataString(self, gpdDataString):
        self.gpdDataString = gpdDataString
    