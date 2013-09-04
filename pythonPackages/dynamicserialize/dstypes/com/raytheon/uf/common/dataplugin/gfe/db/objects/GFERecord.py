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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.time import DataTime
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange


class GFERecord(object):

    def __init__(self, parmId=None, timeRange=None):
        self.gridHistory = []
        self.dataURI = None
        self.pluginName = "gfe"
        self.insertTime = None
        self.messageData = None
        self.identifier = None
        self.dataTime = None
        self.parmId = None
        if timeRange is not None:
            if type(timeRange) is TimeRange:
                self.dataTime = DataTime(refTime=timeRange.getStart(), validPeriod=timeRange)
            else:
                raise TypeError("Invalid TimeRange object specified.")
        if parmId is not None:
            if type(parmId) is ParmID.ParmID:
                self.parmId = parmId
                self.parmName = parmId.getParmName()
                self.parmLevel = parmId.getParmLevel()
                self.dbId = parmId.getDbId()
            else:
                raise TypeError("Invalid ParmID object specified. Type:" + str(type(parmId)))

    def getParmName(self):
        return self.parmName

    def setParmName(self, parmName):
        self.parmName = parmName

    def getParmLevel(self):
        return self.parmLevel

    def setParmLevel(self, parmLevel):
        self.parmLevel = parmLevel

    def getParmId(self):
        return self.parmId

    def setParmId(self, parmId):
        self.parmId = parmId

    def getDbId(self):
        return self.dbId

    def setDbId(self, dbId):
        self.dbId = dbId

    def getGridHistory(self):
        return self.gridHistory

    def setGridHistory(self, gridHistory):
        self.gridHistory = gridHistory

    def getDataURI(self):
        return self.dataURI

    def setDataURI(self, dataURI):
        self.dataURI = dataURI

    def getPluginName(self):
        return "gfe"

    def getDataTime(self):
        return self.dataTime

    def setDataTime(self, dataTime):
        self.dataTime = dataTime

    def getInsertTime(self):
        return self.insertTime

    def setInsertTime(self, insertTime):
        self.insertTime = insertTime

    def getMessageData(self):
        return self.messageData

    def setMessageData(self, messageData):
        self.messageData = messageData

    def getIdentifier(self):
        return self.identifier

    def setIdentifier(self, identifier):
        self.identifier = identifier

