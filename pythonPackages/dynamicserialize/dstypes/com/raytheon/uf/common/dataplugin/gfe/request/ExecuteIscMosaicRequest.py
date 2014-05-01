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

class ExecuteIscMosaicRequest(AbstractGfeRequest):

    def __init__(self, userID=None, databaseID=None, parmsToProcess=[], 
                 blankOtherPeriods=False, startTime=None, endTime=None,
                 altMask=None, replaceOnly=False, eraseFirst=False, announce="",
                 renameWE=False, iscSends=False, inFiles=[], ignoreMask=False,
                 adjustTranslate=False, deleteInput=False, parmsToIgnore=[],
                 gridDelay=0.0, logFileName=None):
        super(ExecuteIscMosaicRequest, self).__init__()
        self.userID = userID
        self.databaseID = databaseID
        self.parmsToProcess = parmsToProcess
        self.blankOtherPeriods = blankOtherPeriods
        self.startTime = startTime
        self.endTime = endTime
        self.altMask = altMask
        self.replaceOnly = replaceOnly
        self.eraseFirst = eraseFirst
        self.announce = announce
        self.renameWE = renameWE
        self.iscSends = iscSends
        self.inFiles = inFiles
        self.ignoreMask = ignoreMask
        self.adjustTranslate = adjustTranslate
        self.deleteInput = deleteInput
        self.parmsToIgnore = parmsToIgnore
        self.gridDelay = gridDelay
        self.logFileName = logFileName
        if self.userID is not None:
            self.workstationID = WsId(progName='iscMosaic', userName=self.userID)
        if self.databaseID is not None:
            self.siteID = self.databaseID.getSiteId()
        
    def __str__(self):
        retVal = "ExecuteIscMosaicRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "userID: " + str(self.userID) + ", "
        retVal += "databaseID: " + str(self.databaseID) + ", "
        retVal += "parmsToProcess: " + str(self.parmsToProcess) + ", "
        retVal += "blankOtherPeriods: " + str(self.blankOtherPeriods) + ", "
        retVal += "startTime: " + str(self.startTime) + ", "
        retVal += "endTime: " + str(self.endTime) + ", "
        retVal += "altMask: " + str(self.altMask) + ", "
        retVal += "replaceOnly: " + str(self.replaceOnly) + ", "
        retVal += "eraseFirst: " + str(self.eraseFirst) + ", "
        retVal += "announce: " + str(self.announce) + ", "
        retVal += "renameWE: " + str(self.renameWE) + ", "
        retVal += "iscSends: " + str(self.iscSends) + ", "
        retVal += "inFiles: " + str(self.inFiles) + ", "
        retVal += "ignoreMask: " + str(self.ignoreMask) + ", "
        retVal += "adjustTranslate: " + str(self.adjustTranslate) + ", "
        retVal += "deleteInput: " + str(self.deleteInput) + ", "
        retVal += "parmsToIgnore: " + str(self.parmsToIgnore) + ", "
        retVal += "gridDelay: " + str(self.gridDelay) + ", "
        retVal += "logFileName: " + str(self.logFileName) + "]"
        return retVal
    
    def __repr__(self):
        retVal = "ExecuteIscMosaicRequest(" 
        retVal += "wokstationID= " + str(self.workstationID) + ", "
        retVal += "siteID= " + str(self.siteID) + ", "
        retVal += "userID= " + str(self.userID) + ", "
        retVal += "databaseID= " + str(self.databaseID) + ", "
        retVal += "parmsToProcess= " + str(self.parmsToProcess) + ", "
        retVal += "blankOtherPeriods= " + str(self.blankOtherPeriods) + ", "
        retVal += "startTime= " + str(self.startTime) + ", "
        retVal += "endTime= " + str(self.endTime) + ", "
        retVal += "altMask= " + str(self.altMask) + ", "
        retVal += "replaceOnly= " + str(self.replaceOnly) + ", "
        retVal += "eraseFirst= " + str(self.eraseFirst) + ", "
        retVal += "announce= " + str(self.announce) + ", "
        retVal += "renameWE= " + str(self.renameWE) + ", "
        retVal += "iscSends= " + str(self.iscSends) + ", "
        retVal += "inFiles= " + str(self.inFiles) + ", "
        retVal += "ignoreMask= " + str(self.ignoreMask) + ", "
        retVal += "adjustTranslate= " + str(self.adjustTranslate) + ", "
        retVal += "deleteInput= " + str(self.deleteInput) + ", "
        retVal += "parmsToIgnore= " + str(self.parmsToIgnore) + ", "
        retVal += "gridDelay= " + str(self.gridDelay) + ", "
        retVal += "logFileName= " + str(self.logFileName) + ")"
        return retVal

    def getUserID(self):
        return self.userID

    def setUserID(self, userID):
        self.userID = userID

    def getDatabaseID(self):
        return self.databaseID

    def setDatabaseID(self, databaseID):
        self.databaseID = databaseID

    def getParmsToProcess(self):
        return self.parmsToProcess

    def setParmsToProcess(self, parmsToProcess):
        self.parmsToProcess = parmsToProcess

    def getBlankOtherPeriods(self):
        return self.blankOtherPeriods

    def setBlankOtherPeriods(self, blankOtherPeriods):
        self.blankOtherPeriods = blankOtherPeriods

    def getStartTime(self):
        return self.startTime

    def setStartTime(self, startTime):
        self.startTime = startTime

    def getEndTime(self):
        return self.endTime

    def setEndTime(self, endTime):
        self.endTime = endTime

    def getAltMask(self):
        return self.altMask

    def setAltMask(self, altMask):
        self.altMask = altMask

    def getReplaceOnly(self):
        return self.replaceOnly

    def setReplaceOnly(self, replaceOnly):
        self.replaceOnly = replaceOnly

    def getEraseFirst(self):
        return self.eraseFirst

    def setEraseFirst(self, eraseFirst):
        self.eraseFirst = eraseFirst

    def getAnnounce(self):
        return self.announce

    def setAnnounce(self, announce):
        self.announce = announce

    def getRenameWE(self):
        return self.renameWE

    def setRenameWE(self, renameWE):
        self.renameWE = renameWE

    def getIscSends(self):
        return self.iscSends

    def setIscSends(self, iscSends):
        self.iscSends = iscSends

    def getInFiles(self):
        return self.inFiles

    def setInFiles(self, inFiles):
        self.inFiles = inFiles

    def getIgnoreMask(self):
        return self.ignoreMask

    def setIgnoreMask(self, ignoreMask):
        self.ignoreMask = ignoreMask

    def getAdjustTranslate(self):
        return self.adjustTranslate

    def setAdjustTranslate(self, adjustTranslate):
        self.adjustTranslate = adjustTranslate

    def getDeleteInput(self):
        return self.deleteInput

    def setDeleteInput(self, deleteInput):
        self.deleteInput = deleteInput

    def getParmsToIgnore(self):
        return self.parmsToIgnore

    def setParmsToIgnore(self, parmsToIgnore):
        self.parmsToIgnore = parmsToIgnore

    def getGridDelay(self):
        return self.gridDelay

    def setGridDelay(self, gridDelay):
        self.gridDelay = gridDelay

    def getLogFileName(self):
        return self.logFileName

    def setLogFileName(self, logFileName):
        self.logFileName = logFileName
