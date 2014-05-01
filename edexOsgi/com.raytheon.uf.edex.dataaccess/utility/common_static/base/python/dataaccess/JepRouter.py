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
#    12/10/12                      njensen       Initial Creation.
#    Feb 14, 2013    1614          bsteffen       refactor data access framework
#                                                 to use single request.
#    
# 
#

from ufpy.dataaccess import IDataRequest

from com.raytheon.uf.common.dataaccess import DataAccessLayer as JavaDataAccessLayer
from com.raytheon.uf.common.dataaccess.impl import DefaultDataRequest
from com.raytheon.uf.common.time import DataTime as JavaDataTime
from com.raytheon.uf.common.geospatial import LatLonReprojection
from com.raytheon.uf.common.python import PythonNumpyFloatArray

import jep
import DataTime
import JGeometryData, JGridData, JDataRequest


def getAvailableTimes(request):
    javaTimes = JavaDataAccessLayer.getAvailableTimes(request.toJavaObj())
    times = []
    for jt in javaTimes:
        times.append(DataTime.DataTime(jt))
    return times


def getGridData(request, times):
    if type(times) is list:
        # presuming list of DataTimes
        jtimes = jep.jarray(len(times), JavaDataTime)
        for i in xrange(len(times)):
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
        for i in xrange(len(times)):
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
    return JavaDataAccessLayer.getAvailableLocationNames(request.toJavaObj())

def newDataRequest():
    return JDataRequest.JDataRequest(DefaultDataRequest())

