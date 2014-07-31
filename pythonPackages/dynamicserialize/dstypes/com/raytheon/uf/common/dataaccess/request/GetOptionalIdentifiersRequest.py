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
# AbstractIdentifierRequest.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/23/14         #3185        njensen        Initial Creation.    
#    07/30/14         #3185        njensen        Renamed valid to optional
#
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import AbstractIdentifierRequest

class GetOptionalIdentifiersRequest(AbstractIdentifierRequest):

    def __init__(self):
        super(GetOptionalIdentifiersRequest, self).__init__()    

