##
##


#
# Adapter for com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/20/13           #1774      randerso       Initial Creation.
#    
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import TimeConstraints

ClassAdapter = 'com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints'

def serialize(context, timeConstraints):
    context.writeI32(timeConstraints.getDuration());
    context.writeI32(timeConstraints.getRepeatInterval());
    context.writeI32(timeConstraints.getStartTime());
    
def deserialize(context):
    result = TimeConstraints(context.readI32(), context.readI32(), context.readI32()) 
    return result