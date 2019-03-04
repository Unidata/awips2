##
##


#
# Adapter for com.raytheon.uf.common.dataplugin.gfe.GridDataHistory
#
# TODO: REWRITE THIS ADAPTER when serialization/deserialization of this
# class has been finalized.
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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe import GridDataHistory

ClassAdapter = 'com.raytheon.uf.common.dataplugin.gfe.GridDataHistory'

def serialize(context, history):
    context.writeString(history.getCodedString())
    
def deserialize(context):
    result = GridDataHistory(context.readString()) 
    return result