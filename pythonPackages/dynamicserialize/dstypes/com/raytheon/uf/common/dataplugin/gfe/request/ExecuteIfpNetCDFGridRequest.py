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
# and then modified post-generation to use AbstractGfeRequest and
# implement str(), repr()
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    xx/xx/??                      dgilling       Initial Creation.
#    03/13/13         1759         dgilling       Add software history header.
#    
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import AbstractGfeRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId

class ExecuteIfpNetCDFGridRequest(AbstractGfeRequest):

    def __init__(self, outputFilename=None, parmList=[], databaseID=None,
                 startTime=None, endTime=None, mask=None, geoInfo=False, 
                 compressFile=False, configFileName=None, compressFileFactor=0, 
                 trim=False, krunch=False, userID=None, logFileName=None):
        super(ExecuteIfpNetCDFGridRequest, self).__init__()
        self.outputFilename = outputFilename
        self.parmList = parmList
        self.databaseID = databaseID
        self.startTime = startTime
        self.endTime = endTime
        self.mask = mask
        self.geoInfo = geoInfo
        self.compressFile = compressFile
        self.configFileName = configFileName
        self.compressFileFactor = compressFileFactor
        self.trim = trim
        self.krunch = krunch
        self.userID = userID
        self.logFileName = logFileName
        if self.userID is not None:
            self.workstationID = WsId(progName='ifpnetCDF', userName=self.userID)
        if self.databaseID is not None:
            self.siteID = self.databaseID.getSiteId()
        
    def __str__(self):
        retVal = "ExecuteIfpNetCDFGridRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "outputFilename: " + str(self.outputFilename) + ", "
        retVal += "parmList: " + str(self.parmList) + ", "
        retVal += "databaseID: " + str(self.databaseID) + ", "
        retVal += "startTime: " + str(self.startTime) + ", "
        retVal += "endTime: " + str(self.endTime) + ", "
        retVal += "mask: " + str(self.mask) + ", "
        retVal += "geoInfo: " + str(self.geoInfo) + ", "
        retVal += "compressFile: " + str(self.compressFile) + ", "
        retVal += "configFileName: " + str(self.configFileName) + ", "
        retVal += "compressFileFactor: " + str(self.compressFileFactor) + ", "
        retVal += "trim: " + str(self.trim) + ", "
        retVal += "krunch: " + str(self.krunch) + ", "
        retVal += "userID: " + str(self.userID) + ", "
        retVal += "logFileName: " + str(self.logFileName) + "]"
        return retVal
    
    def __repr__(self):
        retVal = "ExecuteIfpNetCDFGridRequest(" 
        retVal += "wokstationID=" + repr(self.workstationID) + ", "
        retVal += "siteID=" + repr(self.siteID) + ", "
        retVal += "outputFilename=" + repr(self.outputFilename) + ", "
        retVal += "parmList=" + repr(self.parmList) + ", "
        retVal += "databaseID=" + repr(self.databaseID) + ", "
        retVal += "startTime=" + repr(self.startTime) + ", "
        retVal += "endTime=" + repr(self.endTime) + ", "
        retVal += "mask=" + repr(self.mask) + ", "
        retVal += "geoInfo=" + repr(self.geoInfo) + ", "
        retVal += "compressFile=" + repr(self.compressFile) + ", "
        retVal += "configFileName=" + repr(self.configFileName) + ", "
        retVal += "compressFileFactor=" + repr(self.compressFileFactor) + ", "
        retVal += "trim=" + repr(self.trim) + ", "
        retVal += "krunch=" + repr(self.krunch) + ", "
        retVal += "userID=" + repr(self.userID) + ", "
        retVal += "logFileName=" + repr(self.logFileName) + ")"
        return retVal

    def getOutputFilename(self):
        return self.outputFilename

    def setOutputFilename(self, outputFilename):
        self.outputFilename = outputFilename

    def getParmList(self):
        return self.parmList

    def setParmList(self, parmList):
        self.parmList = parmList

    def getDatabaseID(self):
        return self.databaseID

    def setDatabaseID(self, databaseID):
        self.databaseID = databaseID

    def getStartTime(self):
        return self.startTime

    def setStartTime(self, startTime):
        self.startTime = startTime

    def getEndTime(self):
        return self.endTime

    def setEndTime(self, endTime):
        self.endTime = endTime

    def getMask(self):
        return self.mask

    def setMask(self, mask):
        self.mask = mask

    def getGeoInfo(self):
        return self.geoInfo

    def setGeoInfo(self, geoInfo):
        self.geoInfo = geoInfo

    def getCompressFile(self):
        return self.compressFile

    def setCompressFile(self, compressFile):
        self.compressFile = compressFile

    def getConfigFileName(self):
        return self.configFileName

    def setConfigFileName(self, configFileName):
        self.configFileName = configFileName

    def getCompressFileFactor(self):
        return self.compressFileFactor

    def setCompressFileFactor(self, compressFileFactor):
        self.compressFileFactor = compressFileFactor

    def getTrim(self):
        return self.trim

    def setTrim(self, trim):
        self.trim = trim

    def getKrunch(self):
        return self.krunch

    def setKrunch(self, krunch):
        self.krunch = krunch

    def getUserID(self):
        return self.userID

    def setUserID(self, userID):
        self.userID = userID

    def getLogFileName(self):
        return self.logFileName

    def setLogFileName(self, logFileName):
        self.logFileName = logFileName

