##
##


#
# Adapter for com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID
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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID

ClassAdapter = 'com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID'

def serialize(context, dbId):
    context.writeString(str(dbId))
    
def deserialize(context):
    result = DatabaseID(context.readString()) 
    return result