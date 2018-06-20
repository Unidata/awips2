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

