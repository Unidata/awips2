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
#    10/13/16        5916          bsteffen       Correct grid shape, allow lazy grid lat/lon
#    10/26/16        5919          njensen        Speed up geometry creation in getGeometryData()
#


import numpy
import shapely.wkb

from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.impl import DefaultDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableLocationNamesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableTimesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetGeometryDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetGridDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetGridLatLonRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableParametersRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetAvailableLevelsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetRequiredIdentifiersRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetOptionalIdentifiersRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetIdentifierValuesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetSupportedDatatypesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.request import GetNotificationFilterRequest

from ufpy import ThriftClient
from ufpy.dataaccess import PyGeometryData
from ufpy.dataaccess import PyGridData


class LazyGridLatLon(object):

    def __init__(self, client, nx, ny, envelope, crsWkt):
        self._latLonGrid = None
        self._client = client
        self._request = GetGridLatLonRequest()
        self._request.setNx(nx)
        self._request.setNy(ny)
        self._request.setEnvelope(envelope)
        self._request.setCrsWkt(crsWkt)

    def __call__(self):
        # Its important that the data is cached internally so that if multiple
        # GridData are sharing the same delegate then they can also share a
        # single request for the LatLon information.
        if self._latLonGrid is None:
            response = self._client.sendRequest(self._request)
            nx = response.getNx()
            ny = response.getNy()
            latData = numpy.reshape(numpy.array(response.getLats()), (ny, nx))
            lonData = numpy.reshape(numpy.array(response.getLons()), (ny, nx))
            self._latLonGrid = (lonData, latData)
        return self._latLonGrid


class ThriftClientRouter(object):

    def __init__(self, host='localhost'):
        self._client = ThriftClient.ThriftClient(host)
        self._lazyLoadGridLatLon = False

    def setLazyLoadGridLatLon(self, lazyLoadGridLatLon):
        self._lazyLoadGridLatLon = lazyLoadGridLatLon

    def getAvailableTimes(self, request, refTimeOnly):
        timesRequest = GetAvailableTimesRequest()
        timesRequest.setRequestParameters(request)
        timesRequest.setRefTimeOnly(refTimeOnly)
        response = self._client.sendRequest(timesRequest)
        return response

    def getGridData(self, request, times):
        gridDataRequest = GetGridDataRequest()
        gridDataRequest.setIncludeLatLonData(not self._lazyLoadGridLatLon)
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
            if self._lazyLoadGridLatLon:
                envelope = response.getSiteEnvelopes()[location]
                crsWkt = response.getSiteCrsWkt()[location]
                delegate = LazyGridLatLon(
                    self._client, nx, ny, envelope, crsWkt)
                locSpecificData[location] = (nx, ny, delegate)
            else:
                latData = numpy.reshape(numpy.array(
                    response.getSiteLatGrids()[location]), (ny, nx))
                lonData = numpy.reshape(numpy.array(
                    response.getSiteLonGrids()[location]), (ny, nx))
                locSpecificData[location] = (nx, ny, (lonData, latData))
        retVal = []
        for gridDataRecord in response.getGridData():
            locationName = gridDataRecord.getLocationName()
            locData = locSpecificData[locationName]
            if self._lazyLoadGridLatLon:
                retVal.append(PyGridData.PyGridData(gridDataRecord, locData[
                              0], locData[1], latLonDelegate=locData[2]))
            else:
                retVal.append(PyGridData.PyGridData(
                    gridDataRecord, locData[0], locData[1], locData[2]))
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
            # the wkb is a numpy.ndarray of dtype int8
            # convert the bytearray to a byte string and load it
            geometries.append(shapely.wkb.loads(wkb.tostring()))

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

    def newDataRequest(self, datatype, parameters=[], levels=[], locationNames=[], envelope=None, **kwargs):
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
