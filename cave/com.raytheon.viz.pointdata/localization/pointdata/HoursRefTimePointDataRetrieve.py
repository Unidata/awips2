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

    
    