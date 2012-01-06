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


import PointDataView, PointDataContainer, NoDataException

#
# Python module to request point data.  Split out of
# PointDataContainer.py.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/11/11                      njensen       Initial Creation.
#    
# 
#
    
class PointDataRetrieve:

    def __init__(self, pluginName, site, parameters, keyId='forecastHr', refTime=None, constraint={}, forecast=True, maxSize=99):        
        """Initializes a python PointDataContainer which wraps the Java PointDataContainer capabilities.
            @pluginName the name of the type of data, e.g. bufrmos
            @site the name of the station, e.g. KOMA
            @parameters a python list of parameter names as specified in the pointdata xml
            @keyId how to organize views into the point data, defaults to forecastHr
            @refTime the reference time to request data for, if None will default to the newest time
            @constraint a dictionary of extra string constraints to narrow the data type returned,
                                e.g. {'type':'LAMP'}        
        """        
        
        self.pluginName = pluginName        
        self.site = site
        self.constraint = constraint
        if not refTime:            
            refTime = self.__queryNewestRefTime()
            import time
            if refTime < time.time() - 86400:
                raise NoDataException.NoDataException("Newest data in system is more than 24 hours old")
        if type(refTime) is int or type(refTime) is long:
            from java.util import Date
            refTime = Date(refTime * 1000)
        self.refTime = refTime # should be a java.util.Date
        if type(parameters) is str:
            parameters = [parameters]
        if not parameters.__contains__(keyId):
            parameters.append(keyId)
        self.__keyId = keyId
        self.__initialize(parameters, forecast, int(maxSize))
    
    def __initialize(self, parameters, forecast, maxSize):
        if forecast:
            times = self.__queryForecastTimes()
            self.__javaPdc = self.__requestForecastData(times, parameters)
        else:            
            times = self.__queryRefTimes()
            self.__javaPdc = self.__requestData(times, parameters, maxSize)            
        pdvDict = self.__organizeData(self.__javaPdc)        
        self.pdc = PointDataContainer.PointDataContainer(pdvDict, self.__javaPdc, self.refTime)            
    
    def __queryForecastTimes(self):        
        from com.raytheon.uf.viz.core.catalog import CatalogQuery
        return CatalogQuery.performQuery('dataTime.fcstTime', self.__buildConstraints(self.refTime))

    def __queryRefTimes(self):
        from com.raytheon.uf.viz.core.catalog import CatalogQuery                        
        return CatalogQuery.performQuery('dataTime.refTime', self.__buildConstraints(None))
    
    def __queryNewestRefTime(self):
        from com.raytheon.uf.viz.core.catalog import CatalogQuery
        from java.util import Arrays
        from com.raytheon.uf.common.time import DataTime                 
        results = CatalogQuery.performQuery('dataTime.refTime', self.__buildConstraints(None))
        Arrays.sort(results)
        if len(results) == 0:
            if self.site:
                raise NoDataException.NoDataException("No data available for site " + self.site)
            else:
                raise NoDataException.NoDataException("No data available")
        dt = DataTime(results[len(results)-1])        
        return dt.getRefTime().getTime() / 1000
    
    def __buildConstraints(self, refTime):
        from java.util import HashMap        
        from com.raytheon.uf.common.dataquery.requests import RequestConstraint        
        queryTerms = HashMap()
        queryTerms.put("pluginName", RequestConstraint(self.pluginName))
        if self.site:
            queryTerms.put("location.stationId", RequestConstraint(self.site))
        if refTime:                        
            from com.raytheon.uf.common.time.util import TimeUtil            
            queryTerms.put('dataTime.refTime', RequestConstraint(TimeUtil.formatToSqlTimestamp(refTime)))
        if self.constraint:
            for k in self.constraint.keys():
                queryTerms.put(k, RequestConstraint(self.constraint[k]))
        return queryTerms            
    
    def __requestForecastData(self, availableHours, parameters):
        from com.raytheon.viz.pointdata import PointDataRequest
        from com.raytheon.uf.common.time import DataTime
        from java.lang import String
        import jep
        dts = jep.jarray(len(availableHours), DataTime)
        for i in range(len(availableHours)):
            dts[i] = DataTime(self.refTime, int(availableHours[i]))
        constraints = self.__buildConstraints(None) #times are explicitly set so we don't need to constrain those
        params = jep.jarray(len(parameters), String)
        for i in range(len(parameters)):
            params[i] = String(parameters[i])
        if self.site:
            stations = jep.jarray(1, String)
            stations[0] = String(self.site)
        else:
            stations = None
        return PointDataRequest.requestPointData(dts,
            self.pluginName, params, stations,
            constraints)        
    
    def __requestData(self, availableTimes, parameters, maxSize):
        from com.raytheon.viz.pointdata import PointDataRequest
        from com.raytheon.uf.common.time import DataTime
        from java.lang import String
        import jep
        length = len(availableTimes)
        if maxSize > length:
            sz = length
        else:
            sz = maxSize
        dts = jep.jarray(sz, DataTime)
        for i in range(sz):
            dts[i] = DataTime(availableTimes[length-1-i])
        constraints = self.__buildConstraints(None) #times are explicitly set so we don't need to constrain those
        params = jep.jarray(len(parameters), String)
        for i in range(len(parameters)):
            params[i] = String(parameters[i])
        if self.site:
            stations = jep.jarray(1, String)
            stations[0] = String(self.site)
        else:
            stations = None
        return PointDataRequest.requestPointData(dts,
            self.pluginName, params, stations,
            constraints)
    
    def __organizeData(self, container):
        import PointDataView
        organizedData = {}
        for i in range(container.getCurrentSz()):
            pdv = PointDataView.PointDataView(container.readRandom(i))
            fcstHr = pdv[self.__keyId]
            organizedData[fcstHr] = pdv
        return organizedData
    
def retrieve(pluginName, site, parameters, keyId='forecastHr', refTime=None, constraint={}, forecast=True, maxSize=99):
    ret = PointDataRetrieve(pluginName, site, parameters, keyId, refTime, constraint, forecast, maxSize)
    return ret.pdc

    
    