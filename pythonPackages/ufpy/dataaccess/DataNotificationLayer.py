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
# Published interface for retrieving data updates via ufpy.dataaccess package
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer     Description
# ------------- -------- ------------ --------------------------------------------
# May 26, 2016  2416     rjpeter      Initial Creation.
# Aug 01, 2016  2416     tgurney      Finish implementation
# Nov 05, 2019  7884     tgurney      Python 3 fixes
# Jun 24, 2020  8187     randerso     Added program for qpid connection_id
#
#

"""
Interface for the DAF's data notification feature, which allows continuous
retrieval of new data as it is coming into the system.

There are two ways to access this feature:

1. The DataQueue module (ufpy.dataaccess.DataQueue) offers a collection that
automatically fills up with new data as it receives notifications. See that
module for more information.

2. Depending on the type of data you want, use either getGridDataUpdates() or
getGeometryDataUpdates() in this module. Either one will give you back an
object that will retrieve new data for you and will call a function you specify
each time new data is received.

Example code follows. This example prints temperature as observed from KOMA
each time a METAR is received from there.

  from ufpy.dataaccess import DataAccessLayer as DAL
  from ufpy.dataaccess import DataNotificationLayer as DNL

  def process_obs(list_of_data):
      for item in list_of_data:
          print(item.getNumber('temperature'))

  request = DAL.newDataRequest('obs')
  request.setParameters('temperature')
  request.setLocationNames('KOMA')
  
  notifier = DNL.getGeometryDataUpdates(request)
  notifier.subscribe(process_obs)
  # process_obs will called with a list of data each time new data comes in

"""

import sys
import subprocess
from ufpy.dataaccess.PyGeometryNotification import PyGeometryNotification
from ufpy.dataaccess.PyGridNotification import PyGridNotification


THRIFT_HOST = subprocess.check_output(
                    "source /awips2/fxa/bin/setup.env; echo $DEFAULT_HOST",
                    shell=True).decode().strip()

USING_NATIVE_THRIFT = False

if 'jep' in sys.modules:
    # intentionally do not catch if this fails to import, we want it to
    # be obvious that something is configured wrong when running from within
    # Java instead of allowing false confidence and fallback behavior
    import JepRouter
    router = JepRouter
else:
    from ufpy.dataaccess import ThriftClientRouter
    router = ThriftClientRouter.ThriftClientRouter(THRIFT_HOST)
    USING_NATIVE_THRIFT = True


def _getJmsConnectionInfo(notifFilterResponse):
    serverString = notifFilterResponse.getJmsConnectionInfo()
    try:
        host, port = serverString.split(':')
    except Exception as e:
        raise ValueError('Got bad JMS connection info from server: "' + serverString + '"') from e
    return {'host': host, 'port': port}


def getGridDataUpdates(request):
    """
    Get a notification object that receives updates to grid data.

    Args:
            request: the IDataRequest specifying the data you want to receive

    Returns:
            an update request object that you can listen for updates to by
            calling its subscribe() method
    """
    response = router.getNotificationFilter(request)
    filter = response.getNotificationFilter()
    jmsInfo = _getJmsConnectionInfo(response)
    notifier = PyGridNotification(request, filter, requestHost=THRIFT_HOST, program="daf-gridDataUpdates", **jmsInfo)
    return notifier


def getGeometryDataUpdates(request):
    """
    Get a notification object that receives updates to geometry data.

    Args:
            request: the IDataRequest specifying the data you want to receive

    Returns:
            an update request object that you can listen for updates to by
            calling its subscribe() method
    """
    response = router.getNotificationFilter(request)
    filter = response.getNotificationFilter()
    jmsInfo = _getJmsConnectionInfo(response)
    notifier = PyGeometryNotification(request, filter, requestHost=THRIFT_HOST, program="daf-geometryDataUpdates", **jmsInfo)
    return notifier


def changeEDEXHost(newHostName):
    """
    Changes the EDEX host the Data Access Framework is communicating with. Only
    works if using the native Python client implementation, otherwise, this
    method will throw a TypeError.

    Args:
            newHostHame: the EDEX host to connect to
    """
    if USING_NATIVE_THRIFT:
        global THRIFT_HOST
        THRIFT_HOST = newHostName
        global router
        router = ThriftClientRouter.ThriftClientRouter(THRIFT_HOST)
    else:
        raise TypeError("Cannot call changeEDEXHost when using JepRouter.")
