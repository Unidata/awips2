
# File auto-generated against equivalent DynamicSerialize Java class
# 
#      SOFTWARE HISTORY
# 
#     Date            Ticket#       Engineer       Description
#     ------------    ----------    -----------    --------------------------
#     Sep 16, 2016                  pmoyer         Generated

import numpy

class Station(object):

    def __init__(self):
        self.elevation = None
        self.state = None
        self.stationId = None
        self.longitude = None
        self.latitude = None
        self.wmoIndex = None
        self.country = None

    def getElevation(self):
        return self.elevation

    def setElevation(self, elevation):
        self.elevation = elevation

    def getState(self):
        return self.state

    def setState(self, state):
        self.state = state

    def getStationId(self):
        return self.stationId

    def setStationId(self, stationId):
        self.stationId = stationId

    def getLongitude(self):
        return self.longitude

    def setLongitude(self, longitude):
        self.longitude = numpy.float64(longitude)

    def getLatitude(self):
        return self.latitude

    def setLatitude(self, latitude):
        self.latitude = numpy.float64(latitude)

    def getWmoIndex(self):
        return self.wmoIndex

    def setWmoIndex(self, wmoIndex):
        self.wmoIndex = wmoIndex

    def getCountry(self):
        return self.country

    def setCountry(self, country):
        self.country = country

