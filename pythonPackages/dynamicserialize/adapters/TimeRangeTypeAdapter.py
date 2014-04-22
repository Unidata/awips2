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
#    01/22/14        2667          bclement       use method to get millis from time range 
#    02/28/14        2667          bclement       deserialize now converts millis to micros 
#    
# 
#


from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange


ClassAdapter = 'com.raytheon.uf.common.time.TimeRange'

MICROS_IN_MILLISECOND = 1000
MILLIS_IN_SECOND = 1000

def serialize(context, timeRange):
    context.writeI64(timeRange.getStartInMillis())
    context.writeI64(timeRange.getEndInMillis())

def deserialize(context):
    startTime = context.readI64()
    endTime  = context.readI64()
    
    timeRange = TimeRange()
    # java uses milliseconds, python uses microseconds
    startSeconds = startTime // MILLIS_IN_SECOND
    endSeconds = endTime // MILLIS_IN_SECOND
    startExtraMicros = (startTime % MILLIS_IN_SECOND) * MICROS_IN_MILLISECOND
    endExtraMicros = (endTime % MILLIS_IN_SECOND) * MICROS_IN_MILLISECOND
    timeRange.setStart(startSeconds, startExtraMicros)
    timeRange.setEnd(endSeconds, endExtraMicros)
    
    return timeRange
