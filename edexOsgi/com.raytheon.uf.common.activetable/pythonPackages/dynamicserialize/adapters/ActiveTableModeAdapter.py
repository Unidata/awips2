##
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
