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
# Adapter for java.util.EnumSet
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/28/11                      dgilling       Initial Creation.
#    12/02/13        2537          bsteffen       Serialize empty enum sets.
#    
# 
#



from dynamicserialize.dstypes.java.util import EnumSet

ClassAdapter = ['java.util.EnumSet', 'java.util.RegularEnumSet'] 


def serialize(context, set):
    setSize = len(set)
    context.writeI32(setSize)
    context.writeString(set.getEnumClass())
    for val in set:
        context.writeString(val)
    

def deserialize(context):
    setSize = context.readI32()
    enumClassName = context.readString()
    valList = []
    for i in xrange(setSize):
        valList.append(context.readString())
    return EnumSet(enumClassName, valList)     
