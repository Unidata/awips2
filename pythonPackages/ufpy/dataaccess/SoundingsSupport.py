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
# Classes for retrieving soundings based on gridded data from the Data Access
# Framework
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/24/15         #4480        dgilling       Initial Creation.
#

from collections import defaultdict
from shapely.geometry import Point

from awips import DateTimeConverter
from awips.dataaccess import DataAccessLayer

from dynamicserialize.dstypes.com.raytheon.uf.common.time import DataTime
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.level import Level


def getSounding(modelName, weatherElements, levels, samplePoint, refTime=None, timeRange=None):
    """"
    Performs a series of Data Access Framework requests to retrieve a sounding object
    based on the specified request parameters.
        
    Args:
            modelName: the grid model datasetid to use as the basis of the sounding.
            weatherElements: a list of parameters to return in the sounding.
            levels: a list of levels to sample the given weather elements at
            samplePoint: a lat/lon pair to perform the sampling of data at.
            refTime: (optional) the grid model reference time to use for the sounding. 
                     If not specified, the latest ref time in the system will be used.
            timeRange: (optional) a TimeRange to specify which forecast hours to use.
                     If not specified, will default to all forecast hours.
    
    Returns:
            A _SoundingCube instance, which acts a 3-tiered dictionary, keyed
            by DataTime, then by level and finally by weather element. If no
            data is available for the given request parameters, None is returned.
    """
    
    (locationNames, parameters, levels, envelope, refTime, timeRange) = \
        __sanitizeInputs(modelName, weatherElements, levels, samplePoint, refTime, timeRange)
        
    requestArgs = { 'datatype'      : 'grid',
                    'locationNames' : locationNames,
                    'parameters'    : parameters,
                    'levels'        : levels,
                    'envelope'      : envelope,
                   }
        
    req = DataAccessLayer.newDataRequest(**requestArgs)
    
    forecastHours = __determineForecastHours(req, refTime, timeRange)
    if not forecastHours:
        return None

    response = DataAccessLayer.getGeometryData(req, forecastHours)
    soundingObject = _SoundingCube(response)
    
    return soundingObject

def setEDEXHost(host):
    """
    Changes the EDEX host the Data Access Framework is communicating with.
    
    Args:
            host: the EDEX host to connect to
    """
    
    if host:
        DataAccessLayer.changeEDEXHost(str(host))

def __sanitizeInputs(modelName, weatherElements, levels, samplePoint, refTime, timeRange):
    locationNames = [str(modelName)]
    parameters = __buildStringList(weatherElements)
    levels = __buildStringList(levels)
    envelope = Point(samplePoint)
    if refTime is not None:
        refTime = DataTime(refTime=DateTimeConverter.convertToDateTime(refTime))
    if timeRange is not None:
        timeRange = DateTimeConverter.constructTimeRange(*timeRange)
    return (locationNames, parameters, levels, envelope, refTime, timeRange)
    
def __determineForecastHours(request, refTime, timeRange):
    dataTimes = DataAccessLayer.getAvailableTimes(request, False)
    timesGen = [(DataTime(refTime=dataTime.getRefTime()), dataTime) for dataTime in dataTimes]
    dataTimesMap = defaultdict(list)
    for baseTime, dataTime in timesGen:
        dataTimesMap[baseTime].append(dataTime)
    
    if refTime is None:
        refTime = max(dataTimesMap.keys())
    
    forecastHours = dataTimesMap[refTime]
    if timeRange is None:
        return forecastHours
    else:
        return [forecastHour for forecastHour in forecastHours if timeRange.contains(forecastHour.getValidPeriod())]

def __buildStringList(param):
    if __notStringIter(param):
        return [str(item) for item in param]
    else:
        return [str(param)]
    
def __notStringIter(iterable):
    if not isinstance(iterable, basestring):
        try:
            iter(iterable)
            return True
        except TypeError:
            return False



class _SoundingCube(object):
    """
    The top-level sounding object returned when calling SoundingsSupport.getSounding.
    
    This object acts as a 3-tiered dict which is keyed by time then level 
    then parameter name. Calling times() will return all valid keys into this 
    object.
    """
    
    def __init__(self, geometryDataObjects):
        self._dataDict = {}
        self._sortedTimes = []
        if geometryDataObjects:
            for geometryData in geometryDataObjects:
                dataTime = geometryData.getDataTime()
                level = geometryData.getLevel()
                for parameter in geometryData.getParameters():
                    self.__addItem(parameter, dataTime, level, geometryData.getNumber(parameter))
    
    def __addItem(self, parameter, dataTime, level, value):
        timeLayer = self._dataDict.get(dataTime, _SoundingTimeLayer(dataTime))
        self._dataDict[dataTime] = timeLayer
        timeLayer._addItem(parameter, level, value)
        if dataTime not in self._sortedTimes:
            self._sortedTimes.append(dataTime)
            self._sortedTimes.sort()
    
    def __getitem__(self, key):
        return self._dataDict[key]
    
    def __len__(self):
        return len(self._dataDict)
        
    def times(self):
        """ 
        Returns the valid times for this sounding.
        
        Returns:
            A list containing the valid DataTimes for this sounding in order.
        """
        return self._sortedTimes

 
class _SoundingTimeLayer(object):
    """
    The second-level sounding object returned when calling SoundingsSupport.getSounding.
    
    This object acts as a 2-tiered dict which is keyed by level then parameter
    name. Calling levels() will return all valid keys into this 
    object. Calling time() will return the DataTime for this particular layer.
    """

    def __init__(self, dataTime):
        self._dataTime = dataTime
        self._dataDict = {}
        
    def _addItem(self, parameter, level, value):
        asString = str(level)
        levelLayer = self._dataDict.get(asString, _SoundingTimeAndLevelLayer(self._dataTime, asString))
        levelLayer._addItem(parameter, value)
        self._dataDict[asString] = levelLayer
    
    def __getitem__(self, key):
        asString = str(key)
        if asString in self._dataDict:
            return self._dataDict[asString]
        else:
            raise KeyError("Level " + str(key) + " is not a valid level for this sounding.")
    
    def __len__(self):
        return len(self._dataDict)
        
    def time(self):
        """ 
        Returns the DataTime for this sounding cube layer.
        
        Returns:
            The DataTime for this sounding layer.
        """
        return self._dataTime
    
    def levels(self):
        """ 
        Returns the valid levels for this sounding.
        
        Returns:
            A list containing the valid levels for this sounding in order of
            closest to surface to highest from surface.
        """
        sortedLevels = [Level(level) for level in self._dataDict.keys()]
        sortedLevels.sort()
        return [str(level) for level in sortedLevels]


class _SoundingTimeAndLevelLayer(object):
    """
    The bottom-level sounding object returned when calling SoundingsSupport.getSounding.
    
    This object acts as a dict which is keyed by parameter name. Calling 
    parameters() will return all valid keys into this object. Calling time()
    will return the DataTime for this particular layer. Calling level() will
    return the level for this layer.
    """

    def __init__(self, time, level):
        self._time = time
        self._level = level
        self._parameters = {}
        
    def _addItem(self, parameter, value):
        self._parameters[parameter] = value
    
    def __getitem__(self, key):
        return self._parameters[key]
    
    def __len__(self):
        return len(self._parameters)
        
    def level(self):
        """ 
        Returns the level for this sounding cube layer.
        
        Returns:
            The level for this sounding layer.
        """
        return self._level

    def parameters(self):
        """ 
        Returns the valid parameters for this sounding.
        
        Returns:
            A list containing the valid parameter names.
        """
        return list(self._parameters.keys())

    def time(self):
        """ 
        Returns the DataTime for this sounding cube layer.
        
        Returns:
            The DataTime for this sounding layer.
        """
        return self._time
