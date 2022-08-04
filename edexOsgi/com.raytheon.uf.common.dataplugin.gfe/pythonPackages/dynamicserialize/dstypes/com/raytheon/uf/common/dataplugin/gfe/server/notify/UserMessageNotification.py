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
#    06/22/2015       4573         randerso       Change to extend GfeNotification
#
##    

from .GfeNotification import GfeNotification

class UserMessageNotification(GfeNotification):

    def __init__(self):
        super(UserMessageNotification, self).__init__()
        self.category = None
        self.priority = None
        self.message = None

    def getCategory(self):
        return self.category

    def setCategory(self, category):
        self.category = category

    def getPriority(self):
        return self.priority

    def setPriority(self, priority):
        self.priority = priority

    def getMessage(self):
        return self.message

    def setMessage(self, message):
        self.message = message

    def __str__(self):
        msg = 'Message: ' + str(self.message) + '\n'
        msg += 'Priority: ' + str(self.priority) + '\n'
        msg += 'Category: ' + str(self.category) + '\n'
        return msg

