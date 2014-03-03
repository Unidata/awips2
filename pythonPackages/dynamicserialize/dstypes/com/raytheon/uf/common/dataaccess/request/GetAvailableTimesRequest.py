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
# and then modified post-generation to make it sub class
# AbstractDataAccessRequest.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/28/13         #2023        dgilling       Initial Creation.    
#    03/03/14         #2673        bsteffen       Add ability to query only ref times.
#
#


from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import AbstractDataAccessRequest


class GetAvailableTimesRequest(AbstractDataAccessRequest):

    def __init__(self):
        super(GetAvailableTimesRequest, self).__init__()
        self.refTimeOnly = False
        
    def getRefTimeOnly(self):
        return self.refTimeOnly

    def setRefTimeOnly(self, refTimeOnly):
        self.refTimeOnly = refTimeOnly
