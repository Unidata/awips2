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
# Published interface for ufpy.dataaccess package
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen        Initial Creation.
#    Feb 14, 2013    1614          bsteffen       refactor data access framework
#                                                 to use single request.
#    04/10/13         1871         mnash          move getLatLonCoords to JGridData and add default args
#    05/29/13         2023         dgilling       Hook up ThriftClientRouter.
#    03/03/14         2673         bsteffen       Add ability to query only ref times.
#    
# 
#


import sys
import subprocess

THRIFT_HOST = subprocess.check_output(
                    "source /awips2/fxa/bin/setup.env; echo $DEFAULT_HOST", 
                    shell=True).strip()
USING_NATIVE_THRIFT = False

try:
    import JepRouter
    router = JepRouter
except ImportError:
    from ufpy.dataaccess import ThriftClientRouter
    router = ThriftClientRouter.ThriftClientRouter(THRIFT_HOST)
    USING_NATIVE_THRIFT = True
    

def getAvailableTimes(request, refTimeOnly=False):
    """
    Get the times of available data to the request.
    
    Args: 
            request: the IDataRequest to get data for
    
    Args: 
            refTimeOnly: True if only unique refTimes should be returned(without
            a forecastHr)
    Returns:
            a list of DataTimes    
    """
    return router.getAvailableTimes(request, refTimeOnly)

def getGridData(request, times=[]):
    """
    Gets the grid data that matches the request at the specified times.  Each
    combination of parameter, level, and dataTime will be returned as a
    separate IGridData.
    
    Args:
            request: the IDataRequest to get data for
            times: a list of DataTimes, a TimeRange, or None if the data is time
                    agnostic
    
    Returns:
            a list of IGridData
    """
    return router.getGridData(request, times)

def getGeometryData(request, times=[]):
    """
    Gets the geometry data that matches the request at the specified times.
    Each combination of geometry, level, and dataTime will be returned as a 
    separate IGeometryData.
    
    Args:
            request: the IDataRequest to get data for
            times: a list of DataTimes, a TimeRange, or None if the data is time
                    agnostic
    
    Returns:
            a list of IGeometryData
    """
    return router.getGeometryData(request, times)

def getAvailableLocationNames(request):
    """
    Gets the available location names that match the request without actually
    requesting the data.
    
    Args:
            request: the request to find matching location names for
            
    Returns:
            a list of strings of available location names.
    """
    return router.getAvailableLocationNames(request)

def newDataRequest():
    """"
    Creates a new instance of IDataRequest suitable for the runtime environment.
    
    Returns:
            a new IDataRequest
    """
    return router.newDataRequest()

def changeEDEXHost(newHostName):
    """"
    Changes the EDEX host the Data Access Framework is communicating with. Only
    works if using the native Python client implemenation, otherwise, this
    method will throw a TypeError.
    """
    if USING_NATIVE_THRIFT:
        global THRIFT_HOST
        THRIFT_HOST = newHostName
        global router
        router = ThriftClientRouter.ThriftClientRouter(THRIFT_HOST)
    else:
        raise TypeError("Cannot call changeEDEXHost when using JepRouter.")
