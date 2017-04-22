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
# Routes requests to the Data Access Framework through Python Thrift.
#
#
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/21/13        2023          dgilling       Initial Creation.
#    01/06/14        2537          bsteffen       Share geometry WKT.
#    03/03/14        2673          bsteffen       Add ability to query only ref times.
#    07/22/14        3185          njensen        Added optional/default args to newDataRequest
#    07/23/14        3185          njensen        Added new methods
#    07/30/14        3185          njensen        Renamed valid identifiers to optional
#    06/30/15        4569          nabowle        Use hex WKB for geometries.
#    04/13/15        5379          tgurney        Add getIdentifierValues()
#    06/01/16        5587          tgurney        Add new signatures for
#                                                 getRequiredIdentifiers() and
#                                                 getOptionalIdentifiers()
#    08/01/16        2416          tgurney        Add getNotificationFilter()
#    11/10/16        5900          bsteffen       Correct grid shape
#


import numpy
import shapely.wkb

from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.impl import DefaultDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableLocationNamesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableTimesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetGeometryDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetGridDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableParametersRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableLevelsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetRequiredIdentifiersRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetOptionalIdentifiersRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetIdentifierValuesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetSupportedDatatypesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetNotificationFilterRequest

from awips import ThriftClient
from awips.dataaccess import PyGeometryData
from awips.dataaccess import PyGridData


class ThriftClientRouter(object):

    def __init__(self, host='localhost'):
        self._client = ThriftClient.ThriftClient(host)

    def getAvailableTimes(self, request, refTimeOnly):
        timesRequest = GetAvailableTimesRequest()
        timesRequest.setRequestParameters(request)
        timesRequest.setRefTimeOnly(refTimeOnly)
        response = self._client.sendRequest(timesRequest)
        return response

    def getGridData(self, request, times):
        gridDataRequest = GetGridDataRequest()
        gridDataRequest.setRequestParameters(request)
        # if we have an iterable times instance, then the user must have asked
        # for grid data with the List of DataTime objects
        # else, we assume it was a single TimeRange that was meant for the
        # request
        try:
            iter(times)
            gridDataRequest.setRequestedTimes(times)
        except TypeError:
            gridDataRequest.setRequestedPeriod(times)
        response = self._client.sendRequest(gridDataRequest)

        locSpecificData = {}
        locNames = response.getSiteNxValues().keys()
        for location in locNames:
            nx = response.getSiteNxValues()[location]
            ny = response.getSiteNyValues()[location]
            latData = numpy.reshape(numpy.array(response.getSiteLatGrids()[location]), (ny, nx))
            lonData = numpy.reshape(numpy.array(response.getSiteLonGrids()[location]), (ny, nx))
            locSpecificData[location] = (nx, ny, (lonData, latData))

        retVal = []
        for gridDataRecord in response.getGridData():
            locationName = gridDataRecord.getLocationName()
            locData = locSpecificData[locationName]
            retVal.append(PyGridData.PyGridData(gridDataRecord, locData[0], locData[1], locData[2]))
        return retVal

    def getGeometryData(self, request, times):
        geoDataRequest = GetGeometryDataRequest()
        geoDataRequest.setRequestParameters(request)
        # if we have an iterable times instance, then the user must have asked
        # for geometry data with the List of DataTime objects
        # else, we assume it was a single TimeRange that was meant for the
        # request
        try:
            iter(times)
            geoDataRequest.setRequestedTimes(times)
        except TypeError:
            geoDataRequest.setRequestedPeriod(times)
        response = self._client.sendRequest(geoDataRequest)
        geometries = []
        for wkb in response.getGeometryWKBs():
            # convert the wkb to a bytearray with only positive values
            byteArrWKB = bytearray(map(lambda x: x % 256,wkb.tolist()))
            # convert the bytearray to a byte string and load it.
            geometries.append(shapely.wkb.loads(str(byteArrWKB)))

        retVal = []
        for geoDataRecord in response.getGeoData():
            geom = geometries[geoDataRecord.getGeometryWKBindex()]
            retVal.append(PyGeometryData.PyGeometryData(geoDataRecord, geom))
        return retVal

    def getAvailableLocationNames(self, request):
        locNamesRequest = GetAvailableLocationNamesRequest()
        locNamesRequest.setRequestParameters(request)
        response = self._client.sendRequest(locNamesRequest)
        return response

    def getAvailableParameters(self, request):
        paramReq = GetAvailableParametersRequest()
        paramReq.setRequestParameters(request)
        response = self._client.sendRequest(paramReq)
        return response

    def getAvailableLevels(self, request):
        levelReq = GetAvailableLevelsRequest()
        levelReq.setRequestParameters(request)
        response = self._client.sendRequest(levelReq)
        return response

    def getRequiredIdentifiers(self, request):
        if str(request) == request:
            # Handle old version getRequiredIdentifiers(str)
            request = self.newDataRequest(request)
        idReq = GetRequiredIdentifiersRequest()
        idReq.setRequest(request)
        response = self._client.sendRequest(idReq)
        return response

    def getOptionalIdentifiers(self, request):
        if str(request) == request:
            # Handle old version getOptionalIdentifiers(str)
            request = self.newDataRequest(request)
        idReq = GetOptionalIdentifiersRequest()
        idReq.setRequest(request)
        response = self._client.sendRequest(idReq)
        return response

    def getIdentifierValues(self, request, identifierKey):
        idValReq = GetIdentifierValuesRequest()
        idValReq.setIdentifierKey(identifierKey)
        idValReq.setRequestParameters(request)
        response = self._client.sendRequest(idValReq)
        return response

    def newDataRequest(self, datatype, parameters=[], levels=[], locationNames = [], envelope=None, **kwargs):
        req = DefaultDataRequest()
        if datatype:
            req.setDatatype(datatype)
        if parameters:
            req.setParameters(*parameters)
        if levels:
            req.setLevels(*levels)
        if locationNames:
            req.setLocationNames(*locationNames)
        if envelope:
            req.setEnvelope(envelope)
        if kwargs:
            # any args leftover are assumed to be identifiers
            req.identifiers = kwargs
        return req

    def getSupportedDatatypes(self):
        response = self._client.sendRequest(GetSupportedDatatypesRequest())
        return response
    
    def getNotificationFilter(self, request):
        notifReq = GetNotificationFilterRequest()
        notifReq.setRequestParameters(request)
        response = self._client.sendRequest(notifReq)
        return response