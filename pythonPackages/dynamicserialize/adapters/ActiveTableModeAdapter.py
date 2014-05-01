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
# Adapter for com.raytheon.uf.common.activetable.ActiveTableMode
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/29/10                      wldougher     Initial Creation.
#    
# 
#

from thrift.Thrift import TType
from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import ActiveTableMode

ClassAdapter = 'com.raytheon.uf.common.activetable.ActiveTableMode'

def serialize(context, mode):
    context.protocol.writeFieldBegin('__enumValue__', TType.STRING, 0)
    context.writeString(mode.value)
    
def deserialize(context):
    result = ActiveTableMode()
    # Read the TType.STRING, "__enumValue__", and id.
    # We're not interested in any of those, so just discard them.
    context.protocol.readFieldBegin()
    # now get the actual enum value
    result.value = context.readString()
    return result
