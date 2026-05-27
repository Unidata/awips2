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
# File auto-generated against equivalent DynamicSerialize Java class
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    xx/xx/xxxx                    xxxxxxx        Initial Creation.
#    xx/xx/xxxx      xxxx          njensen        Implemented __repr__.
#    06/12/2013      2099          dgilling       Make class immutable, 
#                                                 add getTimeRange().
# 
#

import time

from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange


class Lock(object):

    def __init__(self, parmId, wsId, startTime, endTime):
        self.parmId = parmId
        self.wsId = wsId
        self.startTime = startTime
        self.endTime = endTime
        self.timeRange = None

    def getParmId(self):
        return self.parmId

    def getWsId(self):
        return self.wsId

    def getStartTime(self):
        return self.startTime

    def getEndTime(self):
        return self.endTime
    
    def getTimeRange(self):
        if not self.timeRange:
            start = self.startTime / 1000.0
            end = self.endTime / 1000.0
            self.timeRange = TimeRange(start, end)
        return self.timeRange
    
    def __repr__(self):
        t0 = time.gmtime(self.getStartTime() / 1000.0)
        t1 = time.gmtime(self.getEndTime() / 1000.0)
        format = '%b %d %y %H:%M:%S %Z'
        msg = 'TR: (' + time.strftime(format, t0) + ', ' + time.strftime(format, t1)   
        msg += " WsId: " + str(self.wsId)
        return msg

