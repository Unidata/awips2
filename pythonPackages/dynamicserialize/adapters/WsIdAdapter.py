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
# Adapter for com.raytheon.uf.common.message.WsId
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/16/10                      dgilling       Initial Creation.
#    04/25/12              545     randerso       Repurposed the lockKey field as threadId
#    
# 
#



from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId

ClassAdapter = 'com.raytheon.uf.common.message.WsId'


def serialize(context, wsId):
    context.writeString(wsId.toString())

def deserialize(context):
    wsIdString = context.readString()
    wsIdParts = wsIdString.split(":", 5)
    
    wsId = WsId()
    wsId.setNetworkId(wsIdParts[0])
    wsId.setUserName(wsIdParts[1])
    wsId.setProgName(wsIdParts[2])
    wsId.setPid(wsIdParts[3])
    wsId.setThreadId(long(wsIdParts[4]))
    
    return wsId

