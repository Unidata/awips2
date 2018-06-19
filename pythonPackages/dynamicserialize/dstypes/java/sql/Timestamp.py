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

# File auto-generated against equivalent DynamicSerialize Java class
# and then modified post-generation to add additional features to better
# match Java implementation. Unlike real timestamp, does not support nanos precision.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/??                      xxxxxxxx       Initial Creation.
#    06/24/15        4480          dgilling       implement based on Date class.
#    Jun 23, 2016    5696          rjpeter        Make String version match java.
#

from dynamicserialize.dstypes.java.util import Date
from time import gmtime, strftime

class Timestamp(Date):

    def __init__(self, time=None):
        super(Timestamp, self).__init__(time)

    def __repr__(self):
        return strftime("%Y-%m-%d %H:%M:%S.", gmtime(self.time/1000.0)) + '{:03d}'.format(self.time%1000)