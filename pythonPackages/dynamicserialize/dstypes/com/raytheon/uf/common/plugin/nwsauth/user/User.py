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

from dynamicserialize.dstypes.com.raytheon.uf.common.plugin.nwsauth.user import UserId

class User(object):

    def __init__(self, userId = None):
        if userId is None:
            self.userId = UserId.UserId()
        else:
            self.userId = userId
        self.authenticationData = None

    def getUserId(self):
        return self.userId

    def setUserId(self, userId):
        self.userId = userId

    def getAuthenticationData(self):
        return self.authenticationData

    def setAuthenticationData(self, authenticationData):
        self.authenticationData = authenticationData

