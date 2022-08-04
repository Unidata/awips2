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


import PointDataView, PointDataContainer, NoDataException, PointDataRetrieve

#
# Python module to request reference time point data.  Split out of
# PointDataContainer.py.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    25Apr2012       14688         rferrel        Initial Creation.
#    06JUL2012       15153         zhao           retrieve latest Metar for MetarMonitor & PersistMonitor
#    16JUL2012       14655         zhao           fixed a problem that occurs when there is no Metar 
#                                                 record within a selected number of hours
# 
#

##
# This is a base file that is not intended to be overridden.
##
    
class RefTimePointDataRetrieve(PointDataRetrieve.PointDataRetrieve):    

    def __init__(self, pluginName, site, parameters, keyId='forecastHr', refTime=None, constraint={}, maxSize=99):
        super(RefTimePointDataRetrieve, self).__init__(pluginName, site, parameters, keyId, refTime, constraint, maxSize)     
    
    def _query(self, parameters, maxSize):
        times = self.__queryRefTimes()
        self.__javaPdc = self.__requestData(times, parameters, maxSize)            
        pdvDict = self._organizeData(self.__javaPdc)        
        self.pdc = PointDataContainer.PointDataContainer(pdvDict, self.__javaPdc, self.refTime)            

    def __queryRefTimes(self):
        from com.raytheon.uf.common.dataquery.requests import DbQueryRequest
        from com.raytheon.uf.viz.core.requests import ThriftClient
        request = DbQueryRequest()
        request.setConstraints(self._buildConstraints(None))
        request.addRequestField('dataTime.refTime')
        request.setOrderByField('dataTime.refTime')
        request.setDistinct(True)
        response = ThriftClient.sendRequest(request).getResults()
        timeList = []
        for i in range(0,response.size()):
            timeList.append(response.get(i).get('dataTime.refTime'))
        return timeList

    def __requestData(self, availableTimes, parameters, maxSize):
        from com.raytheon.viz.pointdata import PointDataRequest
        from java.lang import String
        import jep
        from com.raytheon.uf.common.time import DataTime
        length = len(availableTimes)
        dts = jep.jarray(length,DataTime)

        if maxSize==0 : #DR15153: retrive latest Metar for Metarmonitor & PersistMonitor
            dts = jep.jarray(1,DataTime)
            dts[0] = DataTime(availableTimes[length-1])
        else : # for maxSize >= 1
            dts = self._createJarray(availableTimes, maxSize)
            if len(dts)==0 : 
                dts = jep.jarray(1,DataTime)
                dts[0] = DataTime(availableTimes[length-1])

        constraints = self._buildConstraints(None) #times are explicitly set so we don't need to constrain those
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
        
    def _createJarray(self, availableTimes, maxSize): 
        from com.raytheon.uf.common.time import DataTime
        import jep 
        length = len(availableTimes)
        if maxSize > length:
            sz = length
        else:
            sz = maxSize
        dts = jep.jarray(sz, DataTime)
        for i in range(sz):
            dts[i] = DataTime(availableTimes[length-1-i])
        
        return dts
        
         
def retrieve(pluginName, site, parameters, keyId='forecastHr', refTime=None, constraint={}, maxSize=99):
    ret = RefTimePointDataRetrieve(pluginName, site, parameters, keyId, refTime, constraint, maxSize)
    return ret.pdc

    
    