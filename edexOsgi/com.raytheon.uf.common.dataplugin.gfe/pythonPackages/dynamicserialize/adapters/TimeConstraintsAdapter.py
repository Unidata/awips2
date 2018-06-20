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