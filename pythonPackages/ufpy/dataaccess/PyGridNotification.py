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
# Notification object that produces grid data
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/03/16        2416          rjpeter        Initial Creation.
#    09/06/17        6175          tgurney        Override messageReceived
#

import dynamicserialize
from ufpy.dataaccess.PyNotification import PyNotification
from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint

class PyGridNotification(PyNotification):

    def messageReceived(self, msg):
        dataUriMsg = dynamicserialize.deserialize(msg)
        dataUris = dataUriMsg.getDataURIs()
        for dataUri in dataUris:
            if not self.notificationFilter.accept(dataUri):
                continue
            try:
                # This improves performance over requesting by datatime since it requests only the
                # parameter that the notification was received for (instead of this and all previous
                # parameters for the same forecast hour)
                # TODO: This utterly fails for derived requests
                newReq = self.DAL.newDataRequest(self.request.getDatatype())
                newReq.addIdentifier("dataURI", dataUri)
                newReq.setParameters(self.request.getParameters())
                data = self.getData(newReq, [])
                self.callback(data)
            except Exception as e:
                traceback.print_exc()

    def getData(self, request, dataTimes):
        return self.DAL.getGridData(request, dataTimes)
