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
# Implements IData for use by native Python clients to the Data Access
# Framework.
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer      Description
#    ------------    ----------    -----------   --------------------------
#    Jun 22, 2016    2416          rjpeter       Initial creation
#    Jul 22, 2016    2416          tgurney       Finish implementation
#    Sep 07, 2017    6175          tgurney       Override messageReceived in subclasses
#


import abc
import time
import traceback

import dynamicserialize
from ufpy.dataaccess import DataAccessLayer
from ufpy.dataaccess import INotificationSubscriber
from ufpy.QpidSubscriber import QpidSubscriber
from ufpy.ThriftClient import ThriftRequestException
from dynamicserialize.dstypes.com.raytheon.uf.common.time import DataTime


class PyNotification(INotificationSubscriber):
    """
    Receives notifications for new data and retrieves the data that meets
    specified filtering criteria.
    """

    __metaclass__ = abc.ABCMeta

    def __init__(self, request, filter, host='localhost', port=5672, requestHost='localhost'):
        self.DAL = DataAccessLayer
        self.DAL.changeEDEXHost(requestHost)
        self.request = request
        self.notificationFilter = filter
        self.__topicSubscriber = QpidSubscriber(host, port, decompress=True)
        self.__topicName = "edex.alerts"
        self.callback = None

    def subscribe(self, callback):
        """
        Start listening for notifications.

        Args:
            callback: Function to call with a list of received data objects.
              Will be called once for each request made for data.
        """
        assert hasattr(callback, '__call__'), 'callback arg must be callable'
        self.callback = callback
        self.__topicSubscriber.topicSubscribe(self.__topicName, self.messageReceived)
        # Blocks here

    def close(self):
        if self.__topicSubscriber.subscribed:
            self.__topicSubscriber.close()

    def getDataTime(self, dataURI):
        dataTimeStr = dataURI.split('/')[2]
        return DataTime(dataTimeStr)

    @abc.abstractmethod
    def messageReceived(self, msg):
        """Called when a message is received from QpidSubscriber.

        This method must call self.callback once for each request made for data
        """
        pass

    @abc.abstractmethod
    def getData(self, request, dataTimes):
        """
        Retrieve and return data

        Args:
            request: IDataRequest to send to the server
            dataTimes: list of data times
        Returns:
            list of IData
        """
        pass

    @property
    def subscribed(self):
        """True if currently subscribed to notifications."""
        return self.__topicSubscriber.queueStarted
