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
# and then modified post-generation to sub-class IDataRequest.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/03/16        2416          rjpeter        Initial Creation.
#    08/01/16        2416          tgurney        Implement accept()
# 
#


from awips.dataaccess import INotificationFilter
import sys

if sys.version_info.major == 2:
    from itertools import izip
    # shadowing built-in zip
    zip = izip

class DefaultNotificationFilter(INotificationFilter):

    def __init__(self):
        self.constraints = None

    def getConstraints(self):
        return self.constraints

    def setConstraints(self, constraints):
        self.constraints = constraints

    def accept(self, dataUri):
        tokens = dataUri.split('/')[1:]
        if len(self.constraints) != len(tokens):
            return False
        for constraint, token in zip(self.constraints, tokens):
            if not constraint.evaluate(token):
                return False
        return True
