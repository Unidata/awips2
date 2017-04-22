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
# Adapter for CommutativeTimestamp
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    9/21/2015       4486          rjpeter        Initial creation.
#    Jun 23, 2016    5696          rjpeter        Handle CommutativeTimestamp.
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.time import CommutativeTimestamp


ClassAdapter = 'com.raytheon.uf.common.time.CommutativeTimestamp'

def serialize(context, date):
    context.writeI64(date.getTime())
    
def deserialize(context):
    result = CommutativeTimestamp()
    result.setTime(context.readI64())
    return result