# #
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
# #

#
# Notification object that produces geometry data
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/22/2016      2416          tgurney        Initial creation
#    09/07/2017      6175          tgurney        Override messageReceived
#    11/05/2019      7884          tgurney        Add missing import
#

import dynamicserialize
import traceback
from ufpy.dataaccess.PyNotification import PyNotification

class PyGeometryNotification(PyNotification):

    def messageReceived(self, msg):
        dataUriMsg = dynamicserialize.deserialize(msg)
        dataUris = dataUriMsg.getDataURIs()
        dataTimes = set()
        for dataUri in dataUris:
            if self.notificationFilter.accept(dataUri):
                dataTimes.add(self.getDataTime(dataUri))
        if dataTimes:
            try:
                data = self.getData(self.request, list(dataTimes))
                self.callback(data)
            except Exception as e:
                traceback.print_exc()

    def getData(self, request, dataTimes):
        return self.DAL.getGeometryData(request, dataTimes)
