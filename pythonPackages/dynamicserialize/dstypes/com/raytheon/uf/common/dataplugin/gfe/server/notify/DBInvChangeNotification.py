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

class DBInvChangeNotification(object):

    def __init__(self):
        self.inventory = None
        self.additions = None
        self.deletions = None
        self.siteID = None

    def getInventory(self):
        return self.inventory

    def setInventory(self, inventory):
        self.inventory = inventory

    def getAdditions(self):
        return self.additions

    def setAdditions(self, additions):
        self.additions = additions

    def getDeletions(self):
        return self.deletions

    def setDeletions(self, deletions):
        self.deletions = deletions

    def getSiteID(self):
        return self.siteID

    def setSiteID(self, siteID):
        self.siteID = siteID
    
    def __repr__(self):
        msg = 'Inventory' + str(self.inventory) + '\n'
        msg += 'Additions' + str(self.additions) + '\n'
        msg += 'Deletions' + str(self.deletions)
        return msg

