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
#    03/29/11                      dgilling      Initial Creation.
#    
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID

ClassAdapter = 'com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID'

def serialize(context, parmId):
    context.writeString(str(parmId))
    
def deserialize(context):
    result = ParmID(context.readString()) 
    return result