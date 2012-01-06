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

# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.slice import AbstractGridSlice
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.weather import WeatherKey


class PythonWeatherGridSlice(AbstractGridSlice):

    def __init__(self):
        super(PythonWeatherGridSlice, self).__init__()
        self.weatherGrid = None
        self.keys = []
        
    def getNumPyGrid(self):
        return (self.weatherGrid.getNumPyGrid(), self.keys)

    def getWeatherGrid(self):
        return self.weatherGrid

    def setWeatherGrid(self, weatherGrid):
        self.weatherGrid = weatherGrid

    def getKeys(self):
        return self.keys

    def setKeys(self, keys):
        del self.keys[:]
        for key in keys:
            self.keys.append(WeatherKey(subKeys=key))
