##
##

# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.slice import AbstractGridSlice


class WeatherGridSlice(AbstractGridSlice):

    def __init__(self):
        super(WeatherGridSlice, self).__init__()
        self.weatherGrid = None
        self.keys = []
        
    def getNumPyGrid(self):
        pass

    def getWeatherGrid(self):
        return self.weatherGrid

    def setWeatherGrid(self, weatherGrid):
        self.weatherGrid = weatherGrid

    def getKeys(self):
        return self.keys

    def setKeys(self, keys):
        self.keys = keys
