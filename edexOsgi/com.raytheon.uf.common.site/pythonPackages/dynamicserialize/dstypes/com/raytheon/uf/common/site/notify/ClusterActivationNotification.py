##
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