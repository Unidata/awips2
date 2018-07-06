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
#    06/22/2015       4573         randerso       Initial creation (hand generated)
#
##    

import GfeNotification

class ServiceBackupJobStatusNotification(GfeNotification.GfeNotification):

    def __init__(self):
        super(ServiceBackupJobStatusNotification, self).__init__()
        self.name = None
        self.state = "UNKNOWN"

    def __str__(self):
        msg = "name: " + str(self.name)
        msg += '\n' + "state: " + str(self.state)
        return msg

    def getName(self):
        return self.name

    def setName(self, name):
        self.name = name

    def getState(self):
        return self.state
    
    def setState(self, state):
        self.state = state
        