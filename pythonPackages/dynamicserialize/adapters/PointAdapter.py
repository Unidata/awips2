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
# Adapter for java.awt.Point
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/31/10                      njensen       Initial Creation.
#    
# 
#

from dynamicserialize.dstypes.java.awt import Point

ClassAdapter = 'java.awt.Point'

def serialize(context, point):
    context.writeI32(point.getX())
    context.writeI32(point.getY())

def deserialize(context):
    x = context.readI32()
    y = context.readI32()
    point = Point()
    point.setX(x)
    point.setY(y)
    return point

