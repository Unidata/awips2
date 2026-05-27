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

class ActivateSiteRequest(object):

    def __init__(self, siteID=None, plugin=None):
        self.siteID = siteID
        self.plugin = plugin

    def getSiteID(self):
        return self.siteID

    def setSiteID(self, siteID):
        self.siteID = siteID

    def getPlugin(self):
        return self.plugin

    def setPlugin(self, plugin):
        self.plugin = plugin
