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

# File auto-generated against equivalent DynamicSerialize Java class
from SiteActivationNotification import SiteActivationNotification
class ClusterActivationNotification(SiteActivationNotification):

    def __init__(self):
        self.clusterActive = False
        SiteActivationNotification.__init__(self)
        
    def isClusterActive(self):
        return self.clusterActive
    
    def setClusterActive(self, clusterActive):
        self.clusterActive = clusterActive

    def __str__(self):
        s = self.modifiedSite

        if self.type == 'ACTIVATE':
            if self.status == 'FAILURE':
                s += " has failed to activate on some or all cluster members.  See logs for details"
            else:
                s += " has been successfully activated on all cluster members"
        
        else:
            if self.status == 'FAILURE':
                s += " has failed to deactivate on some or all cluster members.  See logs for details"
            else:
                s += " has been successfully deactivated on all cluster members"

        return s