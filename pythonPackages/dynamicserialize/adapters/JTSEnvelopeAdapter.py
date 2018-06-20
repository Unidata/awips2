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
# Adapter for com.vividsolutions.jts.geom.Envelope
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/29/13         2023         dgilling       Initial Creation.
#
#

from dynamicserialize.dstypes.com.vividsolutions.jts.geom import Envelope

ClassAdapter = 'com.vividsolutions.jts.geom.Envelope'

def serialize(context, envelope):
    context.writeDouble(envelope.getMinX())
    context.writeDouble(envelope.getMaxX())
    context.writeDouble(envelope.getMinY())
    context.writeDouble(envelope.getMaxY())

def deserialize(context):
    env = Envelope()
    env.setMinX(context.readDouble())
    env.setMaxX(context.readDouble())
    env.setMinY(context.readDouble())
    env.setMaxY(context.readDouble())
    return env

