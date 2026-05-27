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
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/10/14         #3623        randerso       Manually created, do not regenerate
#
## 

class SiteActivationNotification(object):

    def __init__(self):
        self.type = None
        self.status = None
        self.primarySite = None
        self.modifiedSite = None
        self.runMode = None
        self.serverName = None
        self.pluginName = None

    def getType(self):
        return self.type

    def setType(self, type):
        self.type = type

    def getStatus(self):
        return self.status

    def setStatus(self, status):
        self.status = status

    def getPrimarySite(self):
        return self.primarysite

    def setPrimarySite(self, primarysite):
        self.primarysite = primarysite

    def getModifiedSite(self):
        return self.modifiedSite

    def setModifiedSite(self, modifiedSite):
        self.modifiedSite = modifiedSite

    def getRunMode(self):
        return self.runMode

    def setRunMode(self, runMode):
        self.runMode = runMode

    def getServerName(self):
        return self.serverName

    def setServerName(self, serverName):
        self.serverName = serverName

    def getPluginName(self):
        return self.pluginName

    def setPluginName(self, pluginName):
        self.pluginName = pluginName
        
    def __str__(self):
        return self.pluginName.upper() + ":" \
             + self.status + ":" \
             + self.type + " " \
             + self.modifiedSite.upper() + " on " \
             + self.serverName + ":" \
             + self.runMode
            