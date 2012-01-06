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
# Modified by njensen to add __repr__

class LockTable(object):

    def __init__(self):
        self.locks = None
        self.wsId = None
        self.parmId = None

    def getLocks(self):
        return self.locks

    def setLocks(self, locks):
        self.locks = locks

    def getWsId(self):
        return self.wsId

    def setWsId(self, wsId):
        self.wsId = wsId

    def getParmId(self):
        return self.parmId

    def setParmId(self, parmId):
        self.parmId = parmId
        
    def __repr__(self):
        msg = "ParmID: " + str(self.parmId)        
        msg += " LockTable WsId: " + self.wsId.toString()
        for i in self.locks:                
            msg += "\n  Lock: " + str(i)            
        return msg

