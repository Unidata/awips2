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

import grib2
import numpy
from math import pow
import time, os, sys, math
import LogStream
import tempfile
from matplotlib.mlab import griddata

from java.lang import Float
from java.lang import Double
from java.lang import Integer

from java.util import Calendar
from java.util import Date
from java.util import GregorianCalendar

from javax.measure.unit import SI
from javax.measure.unit import NonSI
from javax.measure.unit import Unit

from com.raytheon.uf.common.time import DataTime
from com.raytheon.uf.common.time import TimeRange
from com.raytheon.uf.common.geospatial import MapUtil
from com.raytheon.uf.common.serialization import SerializationUtil

from com.raytheon.uf.common.dataplugin.grid import GridRecord

from com.raytheon.uf.common.gridcoverage import LambertConformalGridCoverage
from com.raytheon.uf.common.gridcoverage import LatLonGridCoverage
from com.raytheon.uf.common.gridcoverage import MercatorGridCoverage
from com.raytheon.uf.common.gridcoverage import PolarStereoGridCoverage 
from com.raytheon.uf.common.gridcoverage.lookup import GridCoverageLookup

from com.raytheon.uf.common.gridcoverage import Corner 
from com.raytheon.edex.plugin.grib.util import GribModelLookup

from com.raytheon.uf.common.dataplugin.level.mapping import LevelMapper
from com.raytheon.uf.common.dataplugin.level import Level
from com.raytheon.uf.common.dataplugin.level import LevelFactory

from com.raytheon.edex.plugin.grib.spatial import GribSpatialCache 
from com.raytheon.edex.util.grib import GribTableLookup
from com.raytheon.edex.util import Util

from com.raytheon.edex.plugin.grib import Grib1Decoder

from com.raytheon.edex.util.grib import GribParamTranslator

from com.raytheon.uf.common.parameter import Parameter;
from com.raytheon.uf.common.parameter.mapping import ParameterMapper;


PLUGIN_NAME = "grid"

# Static values for accessing parameter lookup tables
PARAMETER_TABLE = "4.2"
GENPROCESS_TABLE = "A"
LEVELS_TABLE = "4.5"
DOT = "."
DASH = "-"
SPACE = " "
MISSING = "Missing"

# Static values for converting forecast times to seconds
SECONDS_PER_MINUTE = 60
SECONDS_PER_HOUR = 3600
SECONDS_PER_DAY = 86400
# Assumes 31 days in 1 month
SECONDS_PER_MONTH = 2678400
#Assumes 365 days in 1 year
SECONDS_PER_YEAR = 977616000

# Default values for earth shape
MAJOR_AXIS_DEFAULT = 6378160
MINOR_AXIS_DEFAULT = 6356775

# Default values for dx/dy spacing of grids
DEFAULT_SPACING_UNIT = "km"
DEFAULT_SPACING_UNIT2 = "degree"

# Quasi-regular values (grids 37-44)
THINNED_GRID_PTS = 73
THINNED_GRID_MIDPOINT = 37
THINNED_GRID_SPACING = 1.25
THINNED_GRID_SIZE = 3447
THINNED_GRID_REMAPPED_SIZE = 5329
THINNED_XI_LINSPACE = numpy.linspace(0, THINNED_GRID_PTS - 1, THINNED_GRID_PTS)
THINNED_YI_LINSPACE = numpy.linspace(0, THINNED_GRID_PTS - 1, THINNED_GRID_PTS)

# Map of latitudes (north and south) to number of points on a quasi-regular (thinned) grid
THINNED_GRID_PT_MAP = {0:73, 1.25:73, 2.50:73, 3.75:73, 5.0:73, 6.25:73, 7.50:73, 8.75:73, 10.0:72, 11.25:72, 12.50:72,
                    13.75:71, 15.0:71, 16.25:71, 17.50:70, 18.75:70, 20.0:69, 21.25:69, 22.50: 68, 23.75:67, 25.00:67,
                    26.25:66, 27.50:65, 28.75:65, 30.0:64, 31.25:63, 32.50:62, 33.75:61, 35.00:60, 36.25:60, 37.50:59,
                    38.75:58, 40.00:57, 41.25:56, 42.5:55, 43.75:54, 45.00:52, 46.25:51, 47.50:50, 48.75:49, 50.00:48,
                    51.25:47, 52.50:45, 53.75:44, 55.00:43, 56.25:42, 57.50:40, 58.75:39, 60.00:38, 61.25:36, 62.50:35,
                    63.75:33, 65.00:32, 66.25:30, 67.50:29, 68.75:28, 70.00:26, 71.25:25, 72.50:23, 73.75:22, 75.00:20,
                    76.25:19, 77.50:17, 78.75:16, 80.00:14, 81.25:12, 82.50:11, 83.75:9, 85.00:8, 86.25:6, 87.50:5,
                    88.75:3, 90.00:2}

THINNED_GRID_VALUES = THINNED_GRID_PT_MAP.values()

#
#  Python implementation of the grib decoder.  This decoder uses the python ctypes
#  library to access the NCEP grib decoder for extracting data
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/7/09         #1994         bphillip       Initial Creation.
#    
class GribDecoder():

    ##
    # Initializes the grib decoder
    #
    # @param text: Unused
    # @param filePath: The file to decode
    ##
    def __init__(self, text=None, filePath=None):  
        # Assign public file name
        self.fileName = filePath
        

    ##
    # Decodes the grib file
    #
    # @return: List of decoded GribRecords
    # @rtype: List
    ##
    def decode(self):
        # The GribRecords to be returned back to Java
        records = []
        
        #tokens = self.fileName.rsplit("/")
        #if tokens[len(tokens) - 1].startswith("H"):
        #    return records
        
        filePointer = 0;
        version = -1;
        
        if os.path.exists(self.fileName):
            try:
                version = grib2.checkVersion(self.fileName)
            except:
                LogStream.logProblem("Error opening file [", self.fileName, "]: ", sys.exc_info()[1])
                return records
        else:
            LogStream.logProblem("The file does not exist: [", self.fileName, "]")
            return records
        
        decodeFile = None        
        if version == 1:
              grib1Decoder = Grib1Decoder()
              return grib1Decoder.decode(self.fileName)
        else:
              decodeFile = self.fileName
        if decodeFile == None:
            LogStream.logProblem("Could not get final filename to decode: [", self.fileName, "]")
            return records
        gribFile = open(decodeFile, "rb")

        # Define some basic navigation variable for extracting grib records
        recordIndex = 0
        fieldIndex = 0
        numFields = 1
        
        try:
                # Iterate over and decode each record in the file
            while numFields != -1:
    
                while fieldIndex < numFields:
                    # Extract the metadata to the metadata array
                    metadataResults = grib2.getMetadata(gribFile, recordIndex, fieldIndex + 1, 0)
                    numFields = metadataResults['numFields']
                    fieldIndex = fieldIndex + 1
                    if numFields != -1:
                        metadata = metadataResults['metadata']
                        record = self._getData(gribFile, metadata, recordIndex, fieldIndex)
                        if record != None:
                            records.append(record)
    
                recordIndex = recordIndex + 1
                fieldIndex = 0
        except:
            LogStream.logProblem("Error processing file [", self.fileName, "]: ", LogStream.exc())
        finally:
            gribFile.close()       
                
        return records

    ##
    # Decodes a single record contained in the grib file
    #
    # @param fptr: The C file pointer to the file
    # @param metadata: The extracted metadata
    # @param recordIndex: The index of the record being decoded in the file
    # @param fieldIndex: The index of the field of the record in the file
    # @return: Decoded GribRecord object
    # @rtype: GribRecord
    ##
    def _getData(self, fptr, metadata, recordIndex, fieldIndex):

        # Extracts data from grib record via C call to getData
        dataResults = grib2.getData(fptr, recordIndex, fieldIndex)
        
        data = dataResults['data']
        localSectionValues = None
        bitMap = None
        
        # Extracts data from the ID section
        idSectionValues = self._decodeIdSection(dataResults['idSection'])
        self.id = dataResults['idSection']
        
        # Extracts data from the Local section
        if 'localSection' in dataResults:
            localSectionValues = self._decodeLocalSection(dataResults['localSection'])
        
        # Extracts data from the gds template
        gdsSectionValues = self._decodeGdsSection(metadata, dataResults['gdsTemplate'])
        
        self.gds = dataResults['gdsTemplate']
        
        # Extracts data from the pds template
        pdsSectionValues = self._decodePdsSection(metadata, dataResults['idSection'], dataResults['pdsTemplate'])
        self.pds = dataResults['pdsTemplate']
        
        if 'bitmap' in dataResults:
            bitMap = dataResults['bitmap']
        
        # Construct the DataTime object
        if pdsSectionValues['endTime'] is None:
            dataTime = DataTime(idSectionValues['refTime'], pdsSectionValues['forecastTime'])
        else:
            # endTime defines forecast time based on the difference to refTime since forecastTime is the start of the valid period
            timeRange = TimeRange(idSectionValues['refTime'].getTimeInMillis() + (pdsSectionValues['forecastTime'] * 1000), pdsSectionValues['endTime'].getTimeInMillis())
            forecastTime = int(float(pdsSectionValues['endTime'].getTimeInMillis() - idSectionValues['refTime'].getTimeInMillis()) / 1000)
            dataTime = DataTime(idSectionValues['refTime'], forecastTime, timeRange)
                                     
        hybridCoordList = None
        if 'coordList' in dataResults:
            hybridCoordList = numpy.resize(coordList, (1, coordList.size))

        
        numpyDataArray = None
        thinnedPts = None
        thinnedGrid = gdsSectionValues['thinned']
        
        # Special case for thinned grids.
        # Map the thinned grid on to a square lat/lon grid
        if thinnedGrid:
            optValues = dataResults['listOps']
            optList = numpy.zeros(len(optValues), numpy.int32)
            for i in range(0, len(optValues)):
                optList[i] = optValues[i]
                
            dataArray = numpy.zeros(len(data), numpy.float32)
            for i in range(0, len(data)):
                dataArray[i] = data[i]
                

            # Temporary place holder pending Numpy update
            numpyDataArray = numpy.zeros((THINNED_GRID_PTS, THINNED_GRID_PTS), numpy.float32)
            
            # The list of points per parallel for thinned grids
            thinnedPts = numpy.resize(numpy.frombuffer(optList, numpy.int32)[::-1], (1, len(optList)))
            
            # Temporary arrays to hold the (grid, not lat/lon) coordinates of the data
            x = numpy.zeros(THINNED_GRID_SIZE)
            y = numpy.zeros(THINNED_GRID_SIZE)
            
            # The index in the original data
            dataIndex = 0
            for row in range(THINNED_GRID_PTS):
                pts = optList[row]
                rowSpace = numpy.linspace(0, THINNED_GRID_PTS, pts)
                for curridx in range(pts):
                    x[dataIndex] = rowSpace[curridx]
                    y[dataIndex] = row
                    dataIndex = dataIndex + 1
            
            # grid the data.
            numpyDataArray = griddata(x, y, dataArray, THINNED_XI_LINSPACE, THINNED_YI_LINSPACE).astype(numpy.float32)
            
            
        # Apply the bitmap if one is provided and set masked values to missing value
        if metadata[18] == 0:
            data = numpy.where(bitMap == 0, -999999, data)
            
        # Check for fill value provided if complex packing is used
        drs = dataResults['drsTemplate']
        if metadata[14] == 2 or metadata[14] == 3:
            primaryFill = Float.intBitsToFloat(drs[7])
            secondaryFill = Float.intBitsToFloat(drs[8])
            if drs[6] == 1:
                data = numpy.where(data == primaryFill, -999999, data)
            elif drs[6] == 2:
                data = numpy.where(data == primaryFill, -999999, data)
                data = numpy.where(data == secondaryFill, -999999, data)
             
        nx = gdsSectionValues['coverage'].getNx().intValue()
        ny = gdsSectionValues['coverage'].getNy().intValue()

        # Correct the data according to the scan mode found in the gds section.
        scanMode = gdsSectionValues['scanMode']
        if scanMode is not None:
            
            if not thinnedGrid:
                numpyDataArray = numpy.resize(data, (ny, nx))
                    
            # Check if rows are scanned in opposite direction.  If so, we need to flip them around 
            if scanMode & 16 == 16:
                # Check if x or y points are scanned consecutively
                if scanMode & 32 == 32:
                    # y points are scanned consecutively
                    i = 0
                    while i < numpyDataArray.shape[1]:
                        theColumn = numpy.zeros(numpyDataArray.shape[0])
                        for j in range(0, numpyDataArray.shape[0]):
                            theColumn[j] = numpyDataArray[j][i]
                        for j in range(0, numpyDataArray.shape[0]):
                            numpyDataArray[j][i] = theColumn[numpyDataArray.shape[0] - j - 1]
                        i = i + 2
                else:
                    # x points are scanned consecutively
                    i = 1
                    while i < numpyDataArray.shape[0]:
                        theRow = numpy.array(numpyDataArray[i], copy=True)
                        numpyDataArray[i] = theRow[::-1]
                        i = i + 2
                        
            # Check y direction scan mode
            if scanMode & 64 == 64:
                numpyDataArray = numpy.flipud(numpyDataArray)
                
            # Check x direction scan mode
            if scanMode & 128 == 128:
                numpyDataArray = numpy.fliplr(numpyDataArray)
                
        else:
            if not thinnedGrid:
                numpyDataArray = data
               
        origCoverage = gdsSectionValues['coverage']
                        
        # check sub gridding
        modelName = self._createModelName(pdsSectionValues, origCoverage)
        spatialCache = GribSpatialCache.getInstance()
        gridCoverage = gdsSectionValues['coverage']
        subCoverage = spatialCache.getSubGridCoverage(modelName, gridCoverage)

        if subCoverage is not None:
            subGrid = spatialCache.getSubGrid(modelName, gridCoverage)
            # resize the data array
            numpyDataArray = numpy.resize(numpyDataArray, (ny, nx))
            startx = subGrid.getUpperLeftX()
            starty = subGrid.getUpperLeftY()
            subnx = subGrid.getNX()
            subny = subGrid.getNY()
            endY = starty + subny
            endX = startx + subnx

            # handle world wide grid wrap
            if (endX > nx):
                subGridDataArray = numpy.zeros((subny, subnx), numpy.float32)
                midx = nx - startx
                subGridDataArray[0:subny, 0:midx] = numpyDataArray[starty:endY, startx:nx]
                subGridDataArray[0:subny, midx:subnx] = Util.GRID_FILL_VALUE
                numpyDataArray = subGridDataArray
            else:
                numpyDataArray = numpyDataArray[starty:endY, startx:endX]

            # update the number of points
            nx = subnx
            ny = subny
            metadata[4] = nx * ny

            # set the new coverage
            gdsSectionValues['coverage'] = subCoverage

        numpyDataArray = numpy.resize(numpyDataArray, (1, metadata[4]))        
        
        newAbbr = GribParamTranslator.getInstance().translateParameter(2, pdsSectionValues['parameterAbbreviation'], pdsSectionValues['centerid'], pdsSectionValues['subcenterid'], pdsSectionValues['genprocess'], dataTime, gridCoverage)
        
        if newAbbr is None:
            if pdsSectionValues['parameterName'] != MISSING and dataTime.getValidPeriod().getDuration() > 0:
                pdsSectionValues['parameterAbbreviation'] = pdsSectionValues['parameterAbbreviation'] + str(dataTime.getValidPeriod().getDuration() / 3600000) + "hr"
        else:
            pdsSectionValues['parameterAbbreviation'] = newAbbr
        pdsSectionValues['parameterAbbreviation'] = pdsSectionValues['parameterAbbreviation'].replace('_', '-')  
        
        # Construct the GribRecord
        record = GridRecord()
        record.setPluginName(PLUGIN_NAME)
        record.setDataTime(dataTime)
        record.setMessageData(numpyDataArray)
        record.setLocation(gdsSectionValues['coverage'])
        record.setLevel(pdsSectionValues['level'])
        record.setDatasetId(modelName)
        record.addExtraAttribute("centerid", Integer(pdsSectionValues['centerid']))
        record.addExtraAttribute("subcenterid", Integer(pdsSectionValues['subcenterid']))
        record.addExtraAttribute("genprocess", Integer(pdsSectionValues['genprocess']))
        record.addExtraAttribute("backGenprocess", Integer(pdsSectionValues['backGenprocess']))
        record.addExtraAttribute("pdsTemplate", Integer(pdsSectionValues['pdsTemplateNumber']))
        record.addExtraAttribute("gridid", origCoverage.getName())
        if "numForecasts" in pdsSectionValues:
            record.addExtraAttribute("numForecasts", pdsSectionValues['numForecasts'])
        record.setEnsembleId(pdsSectionValues['ensembleId'])
        param = Parameter(pdsSectionValues['parameterAbbreviation'], pdsSectionValues['parameterName'], pdsSectionValues['parameterUnit'])
        GribParamTranslator.getInstance().getParameterNameAlias(modelName, param)
        record.setParameter(param)        # record.setResCompFlags(Integer(gdsSectionValues['resCompFlags']))
        
        #check if forecast used flag needs to be removed
        self._checkForecastFlag(pdsSectionValues, origCoverage, record.getDataTime()) 
              
        return record
    
    ##
    # Decodes the values from the id section into a dictionary
    # @param idSectionData: The values of the ID section of the grib file
    # @return: A dictionary containing the values of the ID section
    # @rtype: dictionary
    ##
    def _decodeIdSection(self, idSectionData):
        
        # Map to hold the values
        idSection = {}
        
        # GRIB master tables version number (currently 2) (see table 1.0)
        idSection['masterTableVersion'] = idSectionData[2]
        
        # Version number of GRIB local tables used to augment Master Table (see Table 1.1)
        idSection['localTableVersion'] = idSectionData[3]
        
        # Significance of reference time (See table 1.2)
        idSection['sigRefTime'] = idSectionData[4]
        
        # The reference time as a java.util.GregorianCalendar object
        idSection['refTime'] = GregorianCalendar(idSectionData[5], idSectionData[6] - 1, idSectionData[7], idSectionData[8], idSectionData[9], idSectionData[10])
        
        # Production Status of Processed Data in the GRIB message (see table 1.3)
        idSection['productionStatus'] = idSectionData[11]
        
        # Type of processed data in this GRIB message (See table 1.4)
        idSection['typeProcessedData'] = idSectionData[12]
        
        return idSection
    
    ##
    # Extracts the local section into a numpy array
    # @param localSectionData: the values of the local section of the grib file
    # @return: The local section as a numpy array if present, else None is returned
    # @rtype: numpy array else None if local section not present
    ##
    def _decodeLocalSection(self, localSectionData):
      
        # Extract the local section and resize into a numpy array
        if len(localSectionData) > 0:
            localData = numpy.zeros(len(localSectionData),numpy.int32)
            for i in range(0,len(localSectionData)):
                localData[i] = localSectionData[i]
            return localData
        # Return None if local section is not present
        return None
    
    ##
    # Decodes the values in the PDS template
    #
    # @param metadata: The metadata information
    # @param idSection: The ID section values
    # @param pdsTemplate: The PDS template values 
    # @return: Dictionary of PDS information
    # @rtype: Dictionary
    ##  
    def _decodePdsSection(self, metadata, idSection, pdsTemplate):    
        
        # Dictionary to hold information extracted from PDS template
        pdsFields = {}
        endTime = None
        forecastTime = 0
        duration = 0
        
        centerID = idSection[0]
        subcenterID = idSection[1]

        pdsTemplateNumber = metadata[10]
        
        # Default to null
        pdsFields['ensembleId'] = None
        pdsFields['pdsTemplateNumber'] = pdsTemplateNumber

        # default to UNKNOWN
        pdsFields['level'] = LevelFactory.getInstance().getLevel(LevelFactory.UNKNOWN_LEVEL, float(0));

        # Templates 0-11 are ordered the same for the most part and can therefore be processed the same
        # Exception cases are handled accordingly

        if pdsTemplateNumber <= 12:
            
            # Get the basic level and parameter information
            if (pdsTemplate[0] == 255):
                parameterName = MISSING
                parameterAbbreviation = MISSING
                parameterUnit = MISSING
            else:
                metadata19 = metadata[19]
                pds0 = pdsTemplate[0]
                tableName = PARAMETER_TABLE + DOT + str(metadata19) + DOT + str(pds0)
                parameter = GribTableLookup.getInstance().getTableValue(centerID, subcenterID, tableName, pdsTemplate[1])

                if parameter is not None:
                    parameterName = parameter.getName()

                    if parameter.getD2dAbbrev() is not None:
                        parameterAbbreviation = parameter.getD2dAbbrev()
                    else:
                        parameterAbbreviation = parameter.getAbbreviation()
                    parameterUnit = parameter.getUnit()
                else:
                    LogStream.logEvent("No parameter information for center[" + str(centerID) + "], subcenter[" +
                                          str(subcenterID) + "], tableName[" + tableName +
                                          "], parameter value[" + str(pdsTemplate[1]) + "]");
                    parameterName = MISSING
                    parameterAbbreviation = MISSING
                    parameterUnit = MISSING
                
            genprocess = GribTableLookup.getInstance().getTableValue(centerID, subcenterID, GENPROCESS_TABLE+"center"+str(centerID), pdsTemplate[4])

            levelName = None;
            levelUnit = None;
            gribLevel = GribTableLookup.getInstance().getTableValue(centerID, subcenterID, LEVELS_TABLE, pdsTemplate[9])

            if gribLevel is not None:
               levelName = gribLevel.getAbbreviation();
               levelUnit = gribLevel.getUnit()
            else:
               LogStream.logEvent("No level information for center[" + str(centerID) + "], subcenter[" +
                                     str(subcenterID) + "], tableName[" + LEVELS_TABLE + "], level value[" +
                                     str(pdsTemplate[9]) + "]");

            if levelName is None or len(levelName) == 0:
                levelName = LevelFactory.UNKNOWN_LEVEL

            # Convert the forecast time to seconds
            forecastTime = self._convertToSeconds(pdsTemplate[8], pdsTemplate[7])
            
            # Scale the level one value if necessary
            if pdsTemplate[10] == 0 or pdsTemplate[11] == 0:
                levelOneValue = float(pdsTemplate[11])
            else:
                levelOneValue = float(pdsTemplate[11] * pow(10, pdsTemplate[10] * - 1))
            
            levelTwoValue = levelOneValue

            # If second level is present, scale if necessary
            if pdsTemplate[12] == 255:
                levelTwoValue = Level.getInvalidLevelValue()
            elif pdsTemplate[12] == 1:
                levelTwoValue = Level.getInvalidLevelValue()
            else:
                if pdsTemplate[13] == 0 or pdsTemplate[14] == 0:
                    levelTwoValue = float(pdsTemplate[14])
                else:
                    levelTwoValue = float(pdsTemplate[14] * pow(10, pdsTemplate[13] * - 1))
            
            if levelName=='SFC' and levelOneValue != float(0):
                levelOneValue=float(0)
                
            if levelName=='EATM':
                levelOneValue=float(0)
                levelTwoValue=float(Level.getInvalidLevelValue())
                
            # Special case handling for specific PDS Templates
            if pdsTemplateNumber == 1 or pdsTemplateNumber == 11:
                typeEnsemble = Integer(pdsTemplate[15]).intValue()
                perturbationNumber = Integer(pdsTemplate[16]).intValue()
                pdsFields['numForecasts'] = Integer(pdsTemplate[17])
                if(typeEnsemble == 0):
                     pdsFields['ensembleId'] = "ctlh" + str(perturbationNumber);
                elif(typeEnsemble == 1):
                     pdsFields['ensembleId'] = "ctll" + str(perturbationNumber);
                elif(typeEnsemble == 2):
                     pdsFields['ensembleId'] = "n" + str(perturbationNumber);
                elif(typeEnsemble == 3):
                     pdsFields['ensembleId'] = "p" + str(perturbationNumber);
                else:
                    pdsFields['ensembleId'] = str(typeEnsemble) + "." + str(perturbationNumber);
                
                if pdsTemplateNumber == 11:
                    endTime = GregorianCalendar(pdsTemplate[18], pdsTemplate[19] - 1, pdsTemplate[20], pdsTemplate[21], pdsTemplate[22], pdsTemplate[23])

                    numTimeRanges = pdsTemplate[24]
                    numMissingValues = pdsTemplate[25]
                    statisticalProcess = pdsTemplate[26]

            elif pdsTemplateNumber == 2 or pdsTemplateNumber == 12:
                derivedForecast = pdsTemplate[15]
                
                if (derivedForecast == 1):
                    parameterAbbreviation= parameterAbbreviation+"mean"
                elif (derivedForecast == 2):
                    parameterAbbreviation= parameterAbbreviation+"sprd"
                
                pdsFields['typeEnsemble'] = Integer(pdsTemplate[15])
                pdsFields['numForecasts'] = Integer(pdsTemplate[16])
                
                if(pdsTemplateNumber == 12):
                    endTime = GregorianCalendar(pdsTemplate[17], pdsTemplate[18] - 1, pdsTemplate[19], pdsTemplate[20], pdsTemplate[21], pdsTemplate[22])
                    numTimeRanges = pdsTemplate[23]
                    numMissingValues = pdsTemplate[24]
                    statisticalProcess = pdsTemplate[25]
                
                
                
            elif pdsTemplateNumber == 5 or pdsTemplateNumber == 9:
                parameterUnit = "%"
                probabilityNumber = pdsTemplate[15]
                forecastProbabilities = pdsTemplate[16]
                probabilityType = pdsTemplate[17]
                scaleFactorLL = pdsTemplate[18]
                scaledValueLL = pdsTemplate[19]
                scaleFactorUL = pdsTemplate[20]
                scaledValueUL = pdsTemplate[21]
                
                if(pdsTemplateNumber == 9):
                    endTime = GregorianCalendar(pdsTemplate[22], pdsTemplate[23] - 1, pdsTemplate[24], pdsTemplate[25], pdsTemplate[26], pdsTemplate[27])
                    numTimeRanges = pdsTemplate[28]
                    numMissingValues = pdsTemplate[29]
                    statisticalProcess = pdsTemplate[30]
                
                if(probabilityType == 1 or probabilityType ==2):
                    if(scaleFactorUL == 0):
                        parameterAbbreviation = parameterAbbreviation+"_"+str(scaledValueUL)
                    else:
                        parameterAbbreviation = parameterAbbreviation+"_"+str(scaledValueUL)+"E"+str(scaleFactorUL)
                elif(probabilityType == 0):
                    if(scaleFactorLL == 0):
                        parameterAbbreviation = parameterAbbreviation+"_"+str(scaledValueLL)
                    else:
                        parameterAbbreviation = parameterAbbreviation+"_"+str(scaledValueLL)+"E"+str(scaleFactorLL)
                
            elif pdsTemplateNumber == 8:
                endTime = GregorianCalendar(pdsTemplate[15], pdsTemplate[16] - 1, pdsTemplate[17], pdsTemplate[18], pdsTemplate[19], pdsTemplate[20])
                
                numTimeRanges = pdsTemplate[21]
                numMissingValues = pdsTemplate[22]
                statisticalProcess = pdsTemplate[23]

            elif pdsTemplateNumber == 10:
                endTime = GregorianCalendar(pdsTemplate[16], pdsTemplate[17] - 1, pdsTemplate[18], pdsTemplate[19], pdsTemplate[20], pdsTemplate[21])

                numTimeRanges = pdsTemplate[22]
                numMissingValues = pdsTemplate[23]
                statisticalProcess = pdsTemplate[24]

            if(pdsTemplate[2] == 6 or pdsTemplate[2] == 7):
                parameterAbbreviation = parameterAbbreviation+"erranl"
                
            parameterAbbreviation = ParameterMapper.getInstance().lookupBaseName(parameterAbbreviation, "grib");
            # Constructing the GribModel object
            pdsFields['centerid'] = centerID
            pdsFields['subcenterid'] = subcenterID
            pdsFields['backGenprocess'] = pdsTemplate[3]
            pdsFields['genprocess'] = pdsTemplate[4]
            pdsFields['parameterName'] = parameterName
            pdsFields['parameterAbbreviation'] = parameterAbbreviation
            pdsFields['parameterUnit'] = parameterUnit

            # Constructing the Level object
            level = LevelMapper.getInstance().lookupLevel(levelName, 'grib', levelOneValue, levelTwoValue, levelUnit)
            pdsFields['level'] = level

            
        
        # Derived forecasts based on all ensemble members at a horizontal
        # level or in a horizontal layer, in a continuous or non-continuous
        # time interval.
        #elif pdsTemplateNumber == 12:
        #    pass

        # Derived forecasts based on a cluster of ensemble members over a 
        # rectangular area at a horizontal level or in a horizontal layer, 
        # in a continuous or non-continuous time interval.
        elif pdsTemplateNumber == 13:
            pass
        
        # Derived forecasts based on a cluster of ensemble members over a 
        # circular area at a horizontal level or in a horizontal layer, in 
        # a continuous or non-continuous time interval.
        elif pdsTemplateNumber == 14:
            pass
        
        # Radar Product
        elif pdsTemplateNumber == 20:
            pass
        
        # Satellite Product Template
        # NOTE:This template is deprecated. Template 31 should be used instead.
        elif pdsTemplateNumber == 30:
            pass
        
        # Satellite Product Template
        elif pdsTemplateNumber == 31:
            pass
        
        # CCITT IA5 character string
        elif pdsTemplateNumber == 254:
            pass
        
        # Cross-section of analysis and forecast at a point in time. 
        elif pdsTemplateNumber == 1000:
            pass
        
        # Cross-section of averaged or otherwise statistically processed analysis or forecast over a range of time.
        elif pdsTemplateNumber == 1001:
            pass
        
        # Cross-section of analysis and forecast, averaged or otherwise statistically-processed over latitude or longitude.
        elif pdsTemplateNumber == 1002:
            pass
        
        # Hovmoller-type grid with no averaging or other statistical processing 
        elif pdsTemplateNumber == 1100:
            pass
        
        # Reserved or Missing
        else:
            pass
        
        #Temporary fix to prevent invalid values getting persisted 
        #to the database until the grib decoder is fully implemented
        if pdsTemplateNumber >= 13:
            pdsFields['parameterName'] ="Unknown"
            pdsFields['parameterAbbreviation'] ="Unknown"
            pdsFields['parameterUnit'] ="Unknown"
        
        # endtime needs to be used to calculate forecastTime and forecastTime should be used for startTime of interval
        pdsFields['forecastTime'] = forecastTime
        pdsFields['endTime'] = endTime
        
        return pdsFields

    ##
    # Decodes spatial information from the GDS template
    # @param metadata: The metadata information
    # @param gdsTemplate: The GDS Template values
    # @return: Dictionary of GDS information
    # @rtype: Dictionary
    ##
    def _decodeGdsSection(self, metadata, gdsTemplate):

        # Dictionary to hold information extracted from PDS template
        gdsFields = {}
        coverage = None
        scanMode = None
        resCompFlags = None
        thinned = False
        gdsTemplateNumber = metadata[7]

        # Latitude/Longitude projection
        if gdsTemplateNumber == 0:

            coverage = LatLonGridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            # la1 = self._correctLat(self._divideBy10e6(gdsTemplate[11]))
            # lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[12]))
            # la2 = self._correctLat(self._divideBy10e6(gdsTemplate[14]))
            # lo2 = self._correctLon(self._divideBy10e6(gdsTemplate[15]))
            la1 = self._divideBy10e6(gdsTemplate[11])
            lo1 = self._divideBy10e6(gdsTemplate[12])
            la2 = self._divideBy10e6(gdsTemplate[14])
            lo2 = self._divideBy10e6(gdsTemplate[15])
            scanMode = gdsTemplate[18]
            resCompFlags = gdsTemplate[13]
            

            # Check for quasi-regular grid
            if metadata[5] > 0:
                # Quasi-regular grid detected
                thinned = True
                nx = THINNED_GRID_PTS
                ny = THINNED_GRID_PTS
                dx = THINNED_GRID_SPACING
                dy = THINNED_GRID_SPACING
                metadata[4] = THINNED_GRID_REMAPPED_SIZE
            else:
                # Not a quasi-regular grid
                nx = gdsTemplate[7]
                ny = gdsTemplate[8]
                dx = self._divideBy10e6(gdsTemplate[16])
                dy = self._divideBy10e6(gdsTemplate[17])

            # According to the grib2 spec 65.535 is completely valid, however it
            # is impossible to define anything larger than a 5x2 grid with this
            # spacing so we assume it is invalid and try to calculate a better
            # value. 65.535 was chosen because it is the value encoded in the
            # GFS161 model and it is completely wrong. This value is probably
            # an artifact of converting from grib1 to grib2 since in grib1 this
            # value would be encoded as an unsigned short with all bits as 1
            # which is a special value in grib1, but in grib2 its just wrong
            if dx >= 65.535:
                dx = abs(lo1-lo2)/nx
            if dy >= 65.535:
                dy = abs(la1-la2)/ny
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT2)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setDx(dx)
            coverage.setDy(dy)
            corner = GribSpatialCache.determineFirstGridPointCorner(scanMode)
            coverage.setFirstGridPointCorner(corner)
            
            coverage = self._getGrid(coverage)
            
        # Rotated Latitude/Longitude projection
        elif gdsTemplateNumber == 1:
            pass
        
        # Stretched Latitude/Longitude projection
        elif gdsTemplateNumber == 2:
            pass
        
        # Rotated and Stretched Latitude/Longitude projection
        elif gdsTemplateNumber == 3:
            pass
        
        # Mercator projection
        elif gdsTemplateNumber == 10:

            coverage = MercatorGridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            nx = gdsTemplate[7]
            ny = gdsTemplate[8]
            la1 = self._correctLat(self._divideBy10e6(gdsTemplate[9]))
            lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[10]))
            latin = self._correctLat(self._divideBy10e6(gdsTemplate[12]))
            la2 = self._correctLat(self._divideBy10e6(gdsTemplate[13]))
            lo2 = self._correctLon(self._divideBy10e6(gdsTemplate[14]))
            dx = self._divideBy10e6(gdsTemplate[17])
            dy = self._divideBy10e6(gdsTemplate[18])
            scanMode = gdsTemplate[15]
            resCompFlags = gdsTemplate[11]
            
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT)
            coverage.setMajorAxis(majorAxis)
            coverage.setMinorAxis(minorAxis)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLatin(latin)
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setDx(dx)
            coverage.setDy(dy)
            corner = GribSpatialCache.determineFirstGridPointCorner(scanMode)
            coverage.setFirstGridPointCorner(corner)
            
            coverage = self._getGrid(coverage)
            
        # Polar Stereographic projection
        elif gdsTemplateNumber == 20:

            coverage = PolarStereoGridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            nx = gdsTemplate[7]
            ny = gdsTemplate[8]
            la1 = self._correctLat(self._divideBy10e6(gdsTemplate[9]))
            lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[10]))
            lov = self._correctLon(self._divideBy10e6(gdsTemplate[13]))
            lad = self._correctLat(self._divideBy10e6(gdsTemplate[12]))
            dx = self._divideBy10e6(gdsTemplate[14])
            dy = self._divideBy10e6(gdsTemplate[15])
            scanMode = gdsTemplate[17]
            resCompFlags = gdsTemplate[11]
            
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT)
            coverage.setMajorAxis(majorAxis)
            coverage.setMinorAxis(minorAxis)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLov(lov)
            coverage.setLad(lad)
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setDx(dx)
            coverage.setDy(dy)
            corner = GribSpatialCache.determineFirstGridPointCorner(scanMode)
            coverage.setFirstGridPointCorner(corner)
            
            coverage = self._getGrid(coverage)

        # Lambert Conformal projection
        elif gdsTemplateNumber == 30:

            coverage = LambertConformalGridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            nx = gdsTemplate[7]
            ny = gdsTemplate[8]
            la1 = self._correctLat(self._divideBy10e6(gdsTemplate[9]))
            lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[10]))
            lov = self._correctLon(self._divideBy10e6(gdsTemplate[13]))
            dx = self._divideBy10e6(gdsTemplate[14])
            dy = self._divideBy10e6(gdsTemplate[15])
            latin1 = self._correctLat(self._divideBy10e6(gdsTemplate[18]))
            latin2 = self._correctLat(self._divideBy10e6(gdsTemplate[19]))
            scanMode = gdsTemplate[17]
            resCompFlags = gdsTemplate[11]
            
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT)
            coverage.setMajorAxis(majorAxis)
            coverage.setMinorAxis(minorAxis)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLov(lov)
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setDx(dx)
            coverage.setDy(dy)
            coverage.setLatin1(latin1)
            coverage.setLatin2(latin2)
            corner = GribSpatialCache.determineFirstGridPointCorner(scanMode)
            coverage.setFirstGridPointCorner(corner)
            
            coverage = self._getGrid(coverage)
            
        # Albers Equal Area projection
        elif gdsTemplate == 31:
            pass
        
        # Gaussian Latitude/Longitude projection
        elif gdsTemplate == 40:
            pass
        
        # Rotated Gaussian Latitude/Longitude projection
        elif gdsTemplate == 41:
            pass
        
        # Stretched Gaussian Latitude/Longitude projection
        elif gdsTemplate == 42:
            pass
        
        # Rotated and Stretched Gaussian Latitude/Longitude projection
        elif gdsTemplate == 43:
            pass
        
        # Spherical Harmonic Coefficients
        elif gdsTemplate == 50:
            pass
            
        # Rotated Spherical Harmonic Coefficients
        elif gdsTemplate == 51:
            pass
        
        # Stretched Spherical Harmonic Coefficients
        elif gdsTemplate == 52:
            pass
        
        # Rotated and Stretched Spherical Harmonic Coefficients
        elif gdsTemplate == 53:
            pass
        
        # Space View Perspective or Orthographic
        elif gdsTemplate == 90:
            pass
        
        # Triangular Grid based on Icosahedron
        elif gdsTemplate == 100:
            pass
        
        # Equatorial Azimuthal Equidistance projection
        elif gdsTemplate == 110:
            pass
        
        # Azimuth-Range projection
        elif gdsTemplate == 120:
            pass
        
        # Curvilinear Orthogonal projection
        elif gdsTemplate == 204:
            pass
        
        # Cross Section Grid with Points Equally spaced on the horizontal
        elif gdsTemplate == 1000:
            pass
        
        # Hovmoller Diagram with Points Equally spaced on the horizontal
        elif gdsTemplate == 1100:
            pass
        
        # Time Section grid
        elif gdsTemplate == 1200:
            pass
            
        # Rotated Latitude/Longitude (Arakawa Staggered E-Grid)
        elif gdsTemplate == 32768:
            pass
        
        # Missing
        elif gdsTemplate == 65535:
            pass
            
        gdsFields['scanMode'] = scanMode
        gdsFields['coverage'] = coverage
        gdsFields['thinned'] = thinned
        gdsFields['resCompFlags'] = resCompFlags
        return gdsFields
        
    ##
    # Gets a grid from the cache.  If not found, one is created and stored to the cache
    #
    # @param temp: A GridCoverage object withough geometry or crs information populated
    # @return: A GribCoverage object
    # @rtype: GribCoverage
    ##
    def _getGrid(self, temp):        
        # Check the cache first
        grid = GribSpatialCache.getInstance().getGrid(temp)

        # If not found, create a new GribCoverage and store in the cache
        if grid is None:
            grid = GridCoverageLookup.getInstance().getCoverage(temp, True)


        return grid
        
    ##
    # Divides a number by 1000
    # 
    # @param number: A number to be divided by 1000
    # @return: The provided number divided by 1000
    # @rtype: float
    ##
    def _divideBy10e3(self, number):
        return float(float(number) / 1000)
    
    ##
    # Divides a number by 1000000
    # 
    # @param number: A number to be divided by 1000000
    # @return: The provided number divided by 1000000
    # @rtype: float
    ##
    def _divideBy10e6(self, number):
        return float(float(number) / 1000000)
    ##
    # Convert a scaledValue and scaleFactor to the unscaled value
    #
    # @param scaledValue: The scaled value
    # @param scaleFactor: The scale factor  
    # @return: The unscaled value
    # @rtype: float
    ##
    def _convertScaledValue(self, scaledValue, scaleFactor):
        return float(scaledValue) / 10**scaleFactor
    
    ##
    # Corrects a longitude to fall within the geotools required bounds of -180 and 180
    #
    # @param lon: The longitude to be corrected
    # @return: The corrected longitude
    # @rtype: float
    ##
    def _correctLon(self, lon):
        
        if lon < 0:
            lon = lon % 360
        else:
            lon = lon % 360
        
        if lon > 180:
            lon = (180 - lon % 180) * - 1
        elif lon < - 180:
            lon = (180 - (- lon % 180))
            
        return lon
    
        ##
    # Corrects a latitude to fall within the geotools required bounds of -90 and 90
    #
    # @param lat: The latitude to be corrected
    # @return: The corrected latitude
    # @rtype: float
    ##
    def _correctLat(self, lat):

        if lat < 0:
            lat = lat % -180
        else:
            lat = lat % 180

        if lat > 90:
            lat = 90 - lat % 90
        elif lat < - 90:
            lat = (90 - (- lat % 90)) * - 1
        return lat
            
    ##
    # Gets the shape of the earth based on Table 3.2
    #
    # @param gdsTemplate:The gdsTemplate values
    # @return: The minor and major axis sizes of the earth
    # @rtype: long, long
    ##
    def _getEarthShape(self, gdsTemplate):

        # Shape of the earth which keys into Table 3.2
        number = gdsTemplate[0]

        #
        # Determine the shape of Earth based on Table 3.2
        #

        # Earth assumed spherical with radius = 6,367,470.0 m
        if number == 0:
            minorAxis = 6367470.0
            majorAxis = 6367470.0
            
        # Earth assumed spherical with radius specified (in m) by data producer
        elif number == 1:
            minorAxis = self._convertScaledValue(gdsTemplate[2], gdsTemplate[1])
            majorAxis = minorAxis
            if majorAxis < 6000000.0 or minorAxis < 6000000.0:
                LogStream.logEvent("Invalid earth shape majorAxis,minorAxis = " + str(majorAxis) + "," + str(minorAxis) + " defaulting to 6367470.0,6367470.0")
                minorAxis = majorAxis = 6367470.0
            
        # Earth assumed oblate spheriod with size as determined by IAU in 1965
        # (major axis = 6,378,160.0 m, minor axis = 6,356,775.0 m, f = 1/297.0)   
        elif number == 2:
            minorAxis = 6356775.0
            majorAxis = 6378160.0
            
        # Earth assumed oblate spheriod with major and minor axes specified (in km) by data producer
        elif number == 3:
            minorAxis = self._convertScaledValue(gdsTemplate[4], gdsTemplate[3]) * 1000
            if minorAxis < 6000000.0:
                LogStream.logEvent("Invalid earth shape minorAxis = " + str(minorAxis) + " defaulting to " + MINOR_AXIS_DEFAULT)
                minorAxis = MINOR_AXIS_DEFAULT
                
            majorAxis = self._convertScaledValue(gdsTemplate[6], gdsTemplate[5]) * 1000
            if majorAxis < 6000000.0:
                LogStream.logEvent("Invalid earth shape majorAxis = " + str(majorAxis) + " defaulting to " + MAJOR_AXIS_DEFAULT)
                majorAxis = MAJOR_AXIS_DEFAULT
                
        # Earth assumed oblate spheriod as defined in IAG-GRS80 model
        # (major axis = 6,378,137.0 m, minor axis = 6,356,752.314 m, f = 1/298.257222101)
        elif number == 4:
            minorAxis = 6356752.314
            majorAxis = 6378137.0
            
        # Earth assumed represented by WGS84 (as used by ICAO since 1998)
        elif number == 5:
            minorAxis = 6356752.314245 
            majorAxis = 6378137.0
        
        # Earth assumed spherical with radius = 6,371,229.0 m
        elif number == 6:
            minorAxis = 6371229.0
            majorAxis = 6371229.0
            
        # Earth assumed oblate spheroid with major and minor axes specified (in m) by data producer
        elif number == 7:
            minorAxis = self._convertScaledValue(gdsTemplate[4], gdsTemplate[3])
            if minorAxis < 6000000.0:
                LogStream.logEvent("Invalid earth shape minorAxis = " + str(minorAxis) + " defaulting to " + MINOR_AXIS_DEFAULT)
                minorAxis = MINOR_AXIS_DEFAULT
                
            majorAxis = self._convertScaledValue(gdsTemplate[6], gdsTemplate[5])
            if majorAxis < 6000000.0:
                LogStream.logEvent("Invalid earth shape majorAxis = " + str(majorAxis) + " defaulting to " + MAJOR_AXIS_DEFAULT)
                majorAxis = MAJOR_AXIS_DEFAULT
                
        # Earth model assumed spherical with radius 6,371,200 m,
        # but the horizontal datum of the resulting Latitude/Longitude field is
        # the WGS84 reference frame
        elif number == 8:
            minorAxis = 6371200.0
            majorAxis = 6371200.0
        else:
            minorAxis = MINOR_AXIS_DEFAULT
            majorAxis = MAJOR_AXIS_DEFAULT
        
        return float(majorAxis), float(minorAxis)
    
    ##
    # Converts a value in the specified unit (according to table 4.4) to seconds
    #
    # @param value: The value to convert to seconds
    # @param fromUnit: The value from Table 4.4 to convert from
    # @return: The number of seconds of the provided value 
    # @rtype: long
    ##
    def _convertToSeconds(self, value, fromUnit):
        
        retVal = value
        
        # Convert from minutes
        if fromUnit == 0:
            retVal = value * SECONDS_PER_MINUTE
            
        # Convert from hours
        elif fromUnit == 1:
            retVal = value * SECONDS_PER_HOUR
            
        # Convert from days
        elif fromUnit == 2:
            retVal = value * SECONDS_PER_DAY
            
        # Convert from months
        elif fromUnit == 3:
            retVal = value * SECONDS_PER_MONTH
            
        # Convert from years
        elif fromUnit == 4:
            retVal = value * SECONDS_PER_YEAR
            
        # Convert from decades
        elif fromUnit == 5:
            retVal = value * 10 * SECONDS_PER_YEAR
        
        # Convert from Normal (30 years)
        elif fromUnit == 6:
            retVal = value * 30 * SECONDS_PER_YEAR
            
        # Convert from centuries
        elif fromUnit == 7:
            retVal = value * 100 * SECONDS_PER_YEAR
            
        # Convert from 3 hours
        elif fromUnit == 10:
            retVal = value * 3 * SECONDS_PER_HOUR
            
        # Convert from 6 hours
        elif fromUnit == 11:
            retVal = value * 6 * SECONDS_PER_HOUR
            
        # Convert from 12 horus
        elif fromUnit == 12:
            retVal = value * 12 * SECONDS_PER_HOUR
            
        return retVal

    def _getGridModel(self, pdsSectionValues, grid):
        center = pdsSectionValues['centerid']
        subcenter = pdsSectionValues['subcenterid']

        process = pdsSectionValues['genprocess']
        gridModel = GribModelLookup.getInstance().getModel(center, subcenter, grid, process)
        return gridModel
    
    def _createModelName(self, pdsSectionValues, grid):
        center = pdsSectionValues['centerid']
        subcenter = pdsSectionValues['subcenterid']

        process = pdsSectionValues['genprocess']
        return GribModelLookup.getInstance().getModelName(center, subcenter, grid, process)
        
    def _checkForecastFlag(self, pdsSectionValues, grid, dataTime):
        gridModel = self._getGridModel(pdsSectionValues, grid)
        if gridModel is None:
            return
        else:
            if gridModel.getAnalysisOnly():
                dataTime.getUtilityFlags().remove(FLAG.FCST_USED)

