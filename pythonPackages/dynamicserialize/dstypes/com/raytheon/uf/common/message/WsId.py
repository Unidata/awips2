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

# File auto-generated against equivalent DynamicSerialize Java class
# Modified by njensen to add __repr__

import struct
import socket
import os
import pwd

class WsId(object):

    def __init__(self, networkId=None, userName=None, progName=None, pid=None, lockKey=None):
        self.networkId = networkId
        if networkId is None:
            self.networkId = str(struct.unpack('<L',socket.inet_aton(socket.gethostbyname(socket.gethostname())))[0])
        
        self.userName = userName
        if userName is None:
            self.userName = pwd.getpwuid(os.getuid()).pw_name
            
        self.progName = progName
        if progName is None:
            self.progName = "unknown"
        
        self.pid = pid
        if pid is None:
            self.pid = os.getpid()
        
        self.lockKey = lockKey
        if lockKey is None:
            self.lockKey = 0

    def getNetworkId(self):
        return self.networkId

    def setNetworkId(self, networkId):
        self.networkId = networkId

    def getUserName(self):
        return self.userName

    def setUserName(self, userName):
        self.userName = userName

    def getProgName(self):
        return self.progName

    def setProgName(self, progName):
        self.progName = progName

    def getPid(self):
        return self.pid

    def setPid(self, pid):
        self.pid = pid

    def getLockKey(self):
        return self.lockKey

    def setLockKey(self, lockKey):
        self.lockKey = lockKey
        
    def toString(self):
        return self.networkId + ":" + self.userName + ":" + self.progName + ":" + str(self.pid) + ":" + str(self.lockKey)
    
    def __str__(self):
        return self.toString()
    
    def __repr__(self):
        return self.toString()