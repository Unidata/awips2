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

import time

class Lock(object):

    def __init__(self):
        self.parmId = None
        self.wsId = None
        self.startTime = None
        self.endTime = None
        self.identifier = None

    def getParmId(self):
        return self.parmId

    def setParmId(self, parmId):
        self.parmId = parmId

    def getWsId(self):
        return self.wsId

    def setWsId(self, wsId):
        self.wsId = wsId

    def getStartTime(self):
        return self.startTime

    def setStartTime(self, startTime):
        self.startTime = startTime

    def getEndTime(self):
        return self.endTime

    def setEndTime(self, endTime):
        self.endTime = endTime

    def getIdentifier(self):
        return self.identifier

    def setIdentifier(self, identifier):
        self.identifier = identifier
    
    def __repr__(self):
        t0 = time.gmtime(self.getStartTime() / 1000.0)
        t1 = time.gmtime(self.getEndTime() / 1000.0)
        format = '%b %d %y %H:%M:%S %Z'
        msg = 'TR: (' + time.strftime(format, t0) + ', ' + time.strftime(format, t1)   
        msg += " WsId: " + str(self.wsId)
        return msg

