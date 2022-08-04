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
# Routes requests to the Data Access Framework through JEP to the Java classes.
# Returns Python objects that wrap Java objects.
#
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen        Initial Creation.
#    02/14/13        1614          bsteffen       refactor data access framework
#                                                 to use single request.
#    03/03/14        2673          bsteffen       Add ability to query only ref times.
#    07/22/14        3185          njensen        Added optional/default args to newDataRequest
#                                                  and added new methods
#    07/30/14        3185          njensen        Renamed valid identifiers to optional
#    05/13/15        4259          njensen        Updated for new JEP API
#    04/13/16        5379          tgurney        Add getIdentifierValues()
#    Jun 01, 2016    5587          tgurney        Add new signatures for
#                                                 getRequiredIdentifiers(),
#                                                 getOptionalIdentifiers()
#
#

##
# This is a base file that is not intended to be overridden.
##



from com.raytheon.uf.common.dataaccess import DataAccessLayer as JavaDataAccessLayer
from com.raytheon.uf.common.dataaccess.impl import DefaultDataRequest
from com.raytheon.uf.common.time import DataTime as JavaDataTime

import jep
import DataTime
import JGeometryData, JGridData, JDataRequest
import JUtil


def getAvailableTimes(request, refTimeOnly):
    javaTimes = JavaDataAccessLayer.getAvailableTimes(request.toJavaObj(), refTimeOnly)
    times = []
    for jt in javaTimes:
        times.append(DataTime.DataTime(jt))
    return times


def getGridData(request, times):
    if type(times) is list:
        # presuming list of DataTimes
        jtimes = jep.jarray(len(times), JavaDataTime)
        for i in range(len(times)):
            jtimes[i] = times[i].toJavaObj()
        javaData = JavaDataAccessLayer.getGridData(request.toJavaObj(), jtimes)
    else:
        # presuming TimeRange
        javaData = JavaDataAccessLayer.getGridData(request.toJavaObj(), times.toJavaObj())
    data = []
    for jd in javaData:
        data.append(JGridData.JGridData(jd))
    return data

def getGeometryData(request, times):
    if type(times) is list:
        # presuming list of DataTimes
        jtimes = jep.jarray(len(times), JavaDataTime)
        for i in range(len(times)):
            jtimes[i] = times[i].toJavaObj()
        javaData = JavaDataAccessLayer.getGeometryData(request.toJavaObj(), jtimes)
    else:
        # presuming TimeRange
        javaData = JavaDataAccessLayer.getGeometryData(request.toJavaObj(), times.toJavaObj())
    data = []
    for jd in javaData:
        data.append(JGeometryData.JGeometryData(jd))
    return data

def getAvailableLocationNames(request):
    jlocs = JavaDataAccessLayer.getAvailableLocationNames(request.toJavaObj())
    return JUtil.javaObjToPyVal(jlocs)

def getAvailableParameters(request):
    jparams = JavaDataAccessLayer.getAvailableParameters(request.toJavaObj())
    return JUtil.javaObjToPyVal(jparams)

def getAvailableLevels(request):
    jlevels = JavaDataAccessLayer.getAvailableLevels(request.toJavaObj())
    pylevs = []
    for jlev in jlevels:
        pylevs.append(str(jlev))
    return pylevs

def getRequiredIdentifiers(request):
    if str(request) == request:
        # Handle old version getRequiredIdentifiers(str)
        request = newDataRequest(request)
    jids = JavaDataAccessLayer.getRequiredIdentifiers(request.toJavaObj())
    return JUtil.javaObjToPyVal(jids)

def getOptionalIdentifiers(request):
    if str(request) == request:
        # Handle old version getOptionalIdentifiers(str)
        request = newDataRequest(request)
    jids = JavaDataAccessLayer.getOptionalIdentifiers(request.toJavaObj())
    return JUtil.javaObjToPyVal(jids)

def getIdentifierValues(request, identifierKey):
    jvals = JavaDataAccessLayer.getIdentifierValues(request.toJavaObj(), identifierKey)
    return JUtil.javaObjToPyVal(jvals)

def newDataRequest(datatype, parameters=[], levels=[], locationNames = [], envelope=None, **kwargs):
    req = JDataRequest.JDataRequest(DefaultDataRequest())
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
        for key in kwargs:
            req.addIdentifier(key, kwargs[key])
    return req

def getSupportedDatatypes():
    jsupported = JavaDataAccessLayer.getSupportedDatatypes()
    return JUtil.javaObjToPyVal(jsupported)

