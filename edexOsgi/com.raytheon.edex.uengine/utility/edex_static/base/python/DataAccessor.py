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

import math
from com.raytheon.edex.plugin.grib.dao import GribDao
from com.raytheon.edex.plugin.satellite.dao import SatelliteDao
from com.raytheon.edex.plugin.radar.dao import RadarDao
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DFloat
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DByte
from com.raytheon.uf.common.dataplugin.radar.util import RadarMapper
from javax.measure.unit import Unit

###
# Radar, Grib, and Satellite data accessor for use with the command line interface
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/08/10        3537          bphillip      Initial Creation.
#    
###
class DataAccessor():
    
    ##
    #  Initializes the DataAccessor.  Creates the data access objects
    ##
    def __init__(self):
        self.__gribDao = GribDao()
        self.__satDao = SatelliteDao("satellite")
        self.__radarDao = RadarDao("radar")
        
    ##
    #  Gets the HDF5 grib data associated with the provided GribRecord
    #  @param record:  The GribRecord for which to retrieve the HDF5 data for
    #  @return: The numpy float array of the the raw grib data
    ##
    def getGribData(self, record):
        data = self.__gribDao.getHDF5Data(record, - 1)
        return Grid2DFloat(int(data[0].getSizes()[0]), int(data[0].getSizes()[1]), data[0].getFloatData()).__numpy__[0]
    
    ##
    #  Gets the HDF5 satellite data associated with the provided SatelliteRecord
    #  @param record: The SatelliteRecord for which to retrieved the HDF5 data for
    #  @return: The numpy byte array of the raw satellite data
    ##
    def getSatelliteData(self, record):
        data = self.__satDao.getHDF5Data(record, - 1)
        return Grid2DByte(int(data[0].getSizes()[0]), int(data[0].getSizes()[1]), data[0].getByteData()).__numpy__[0]
    
    ##
    #  Gets the HDF5 radar data associated with the provided RadarRecord
    #  @param record: The RadarRecord for which to retrieve the HDF5 data for 
    #  @return: The numpy byte array of the raw radar data
    ##
    def getRadarData(self, record):
        from com.raytheon.edex.uengine.tasks.radar import DecodeRadarImage
        from com.raytheon.edex.uengine.tasks.decode import FileIn
        self.__radarDao.populateData(record)
        radarImage = DecodeRadarImage(record, FileIn("radar", record).retrieveGroup())
        theData = Grid2DByte(920, 920,radarImage.execute())
        return theData.__numpy__[0]
