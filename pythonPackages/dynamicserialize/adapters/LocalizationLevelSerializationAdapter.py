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
# Adapter for com.raytheon.uf.common.localization.LocalizationContext$LocalizationLevel
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/11/11                      dgilling       Initial Creation.
#    
# 
#



from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationLevel

ClassAdapter = [
                 'com.raytheon.uf.common.localization.LocalizationContext$LocalizationLevel',
                 'com.raytheon.uf.common.localization.LocalizationLevel'
                ]

def serialize(context, level):
    context.writeString(level.getText())
    context.writeI32(level.getOrder())
    context.writeBool(level.isSystemLevel());

def deserialize(context):
    text = context.readString()
    order = context.readI32()
    systemLevel = context.readBool()
    level = LocalizationLevel(text, order, systemLevel=systemLevel)
    return level
    

