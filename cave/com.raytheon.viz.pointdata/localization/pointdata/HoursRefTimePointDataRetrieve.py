##
##


import PointDataView, PointDataContainer, NoDataException, RefTimePointDataRetrieve

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
#    
# 
#
    
##
# This is a base file that is not intended to be overridden.
##

class HoursRefTimePointDataRetrieve(RefTimePointDataRetrieve.RefTimePointDataRetrieve):    

    def __init__(self, pluginName, site, parameters, keyId='forecastHr', refTime=None, constraint={}, maxSize=99):
        super(HoursRefTimePointDataRetrieve, self).__init__(pluginName, site, parameters, keyId, refTime, constraint, maxSize)     

    def _createJarray(self, availableTimes, numHours):  
        from java.util import Date
        from com.raytheon.uf.common.time import DataTime
        import jep, time
        #Get a DataTime numHours from current time
        stTime = long(time.time()) * 1000
        stTime -= numHours * (60 * 60 * 1000)
        stDateTime = DataTime(Date(stTime))
        length = len(availableTimes)
        xdts = []
        for i in range(length) :
            d = DataTime(availableTimes[length-1-i])
            if d.greaterThan(stDateTime) :
                xdts.append(d)
            else :
                 break
        sz = len(xdts)
        dts = jep.jarray(sz, DataTime)
        i = 0
        for d in xdts:
            dts[i] = d
            i += 1
        return dts
   
def retrieve(pluginName, site, parameters, keyId='forecastHr', refTime=None, constraint={}, maxSize=99):
    ret = HoursRefTimePointDataRetrieve(pluginName, site, parameters, keyId, refTime, constraint, maxSize)
    return ret.pdc

    
    