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

from gov.noaa.nws.ncep.common.dataplugin.ncgrib import NcgribRecord
from gov.noaa.nws.ncep.common.dataplugin.ncgrib import NcgribModel
from gov.noaa.nws.ncep.common.dataplugin.ncgrib import NcgribParameter
from gov.noaa.nws.ncep.common.dataplugin.ncgrib import Ncgrib1Parameter
from gov.noaa.nws.ncep.common.dataplugin.ncgrib.util import Ncgrib1ParameterLookup

from gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections import LambertConformalNcgridCoverage
from gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections import LatLonNcgridCoverage
from gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections import MercatorNcgridCoverage
from gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections import PolarStereoNcgridCoverage 

from com.raytheon.uf.common.dataplugin.level import Level
from com.raytheon.uf.common.dataplugin.level import LevelFactory

from gov.noaa.nws.ncep.edex.plugin.ncgrib.spatial import NcgribSpatialCache
from gov.noaa.nws.ncep.edex.plugin.ncgrib.util import NcgribModelCache
from gov.noaa.nws.ncep.edex.util.ncgrib import NcgribTableLookup
from gov.noaa.nws.ncep.edex.util.ncgrib import NcgribModelLookup
#from gov.noaa.nws.ncep.common.dataplugin.ncgrib.util import NcgribModelLookup
from gov.noaa.nws.ncep.edex.util.grib2vars import Grib2VarsTableLookup
from gov.noaa.nws.ncep.edex.util.grib2vcrd import Grib2VcrdTableLookup

from  gov.noaa.nws.ncep.edex.plugin.ncgrib import Ncgrib1Decoder
from  gov.noaa.nws.ncep.edex.util.ncgrib import NcgribParamTranslator

PLUGIN_NAME = "ncgrib"

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
#  Python implementation of the ncgrib decoder.  This decoder uses the python ctypes
#  library to access the NCEP grib decoder for extracting data
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/7/09         #1994         bphillip       Initial Creation.
#    10/13/10        #276          llin           modified for NC GRIB.
#    06/28/11                      xguo           Added codes to get correct VAR/VCD
#    07/12/11                      xguo           Changed Grib1Decoder() to Ncgrib1Decoder()
#    07/26/11                      xguo           Added codes to handle Derived Ensemble Data 
#    09/08/11                      xguo           Added one column grib data
#    09/21/11                      xguo           Check derived ensemble data
#    10/04/11                      xguo           Remove '_' from model name
#    11/02/11                      xguo           Added codes to decode firewx
#    11/08/11                      xguo           Adjusted glevel1/glevel2 for PRES/PDLY/POTV
#    11/22/11                      xguo           Updated Level infor in model
#    
class NcgribDecoder():

    ##
    # Initializes the ncgrib decoder
    #
    # @param text: Unused
    # @param filePath: The file to decode
    ##
    def __init__(self, text=None, filePath=None):  
        # Assign public file name
        self.fileName = filePath
        self.dicipline = -1
        self.inputFile = None
        self.abbr = None
        self.derived = None
        self.VCD = -1
        self.count = 1
        self.AddColumn = -1
    
    ##
    # Decodes the ncgrib file
    #
    # @return: List of decoded NcgribRecords
    # @rtype: List
    ##
    def decode(self):
        # The NcgribRecords to be returned back to Java
        records = []
        
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
              grib1Decoder = Ncgrib1Decoder()
              return grib1Decoder.decode(self.fileName)
        else:
              decodeFile = self.fileName
              fileList = decodeFile.split("/")
              self.inputFile = fileList[len(fileList) - 1]
 
        if decodeFile == None:
            LogStream.logProblem("Could not get final filename to decode: [", self.fileName, "]")
            return records
        gribFile = open(decodeFile,"rb")

        # Define some basic navigation variable for extracting grib records
        recordIndex = 0
        fieldIndex = 0
        numFields = 1
        
        try:
                # Iterate over and decode each record in the file
            while numFields != - 1 :
    
                while fieldIndex < numFields:
                    # Extract the metadata to the metadata array
                    metadataResults = grib2.getMetadata(gribFile, recordIndex, fieldIndex + 1, 0)
                    numFields = metadataResults['numFields']
                    fieldIndex = fieldIndex + 1
                    if numFields != - 1:
                        numFieldsSave = numFields
                        metadata = metadataResults['metadata']
                        
                        record = self._getData(gribFile, metadata, recordIndex, fieldIndex)
                        if record != None:
                            self._addRecord(records, record)
                            
                            if self.abbr == "uW" or self.abbr == "uWmean" or self.abbr == "uWsprd" or self.abbr == "uWprob":
                                #Extract a second field if it exists
                                metadataResults = grib2.getMetadata(gribFile, recordIndex, fieldIndex + 1, 0)
                                numFields = metadataResults['numFields']
                                fieldIndex = fieldIndex + 1
                                if numFields != - 2:
                                    metadata = metadataResults['metadata']
                                    record = self._getData(gribFile, metadata, recordIndex, fieldIndex)
                                    if record != None:
                                        self._addRecord(records, record)
                                    
                        numFields = numFieldsSave
                                
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
    # @return: Decoded NcgribRecord object
    # @rtype: NcgribRecord
    ##
    def _getData(self, fptr, metadata, recordIndex, fieldIndex):
        
        interval = -9999

        # get discipline
        self.dicipline = metadata[19]
        
        # Extracts data from ncgrib record via C call to getData
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
        self.VCD = self.pds[9]
        lenPds = len(self.pds)
                                 
        if 'bitmap' in dataResults:
            bitMap = dataResults['bitmap']
        
        # Construct the DataTime object
#        if pdsSectionValues['endTime'] is None:
#            dataTime = DataTime(idSectionValues['refTime'], pdsSectionValues['forecastTime'])
#        else:
            # endTime defines forecast time based on the difference to refTime since forecastTime is the start of the valid period
#            timeRange = TimeRange(idSectionValues['refTime'].getTimeInMillis() + (pdsSectionValues['forecastTime'] * 1000), pdsSectionValues['endTime'].getTimeInMillis())
#            forecastTime = int(float(pdsSectionValues['endTime'].getTimeInMillis() - idSectionValues['refTime'].getTimeInMillis()) / 1000) 
#            dataTime = DataTime(idSectionValues['refTime'], forecastTime, timeRange)
                                      
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
            optList = numpy.zeros(len(optValues),numpy.int32)
            for i in range(0,len(optValues)):
                optList[i] = optValues[i]
                
            dataArray = numpy.zeros(len(data),numpy.float32)
            for i in range(0,len(data)):
                dataArray[i] = data[i]
                

            # Temporary place holder pending Numpy update
            numpyDataArray = numpy.zeros((THINNED_GRID_PTS, THINNED_GRID_PTS), numpy.float32)
            
            # The list of points per parallel for thinned grids
            thinnedPts = numpy.resize(numpy.frombuffer(optList, numpy.int32)[:: - 1], (1, len(optList)))
            
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
            data = numpy.where(bitMap == 0, - 999999, data)
             
        nx = gdsSectionValues['coverage'].getNx().intValue()
        ny = gdsSectionValues['coverage'].getNy().intValue()
        
        if self.AddColumn == 1:
            nx = nx - 1

        # Correct the data according to the scan mode found in the gds section.
        scanMode = gdsSectionValues['scanMode']
        if scanMode is not None:
            
            # Flip the grid vertically
            if scanMode == 64:
                if not thinnedGrid:
                    numpyDataArray = numpy.resize(data, (ny, nx)) 
                
                numpyDataArray = numpy.flipud(numpyDataArray)
                
            # Flip the grid horizontally
            elif scanMode == 128:
                
                if not thinnedGrid:
                    numpyDataArray = numpy.resize(data, (ny, nx)) 
                numpyDataArray = numpy.fliplr(numpyDataArray)
                
            # Flip vertically and horizontally (rotate 180 degrees)
            elif scanMode == 192:
                
                if not thinnedGrid:
                    numpyDataArray = numpy.resize(data, (ny, nx)) 
                numpyDataArray = numpy.rot90(numpyDataArray, 2)

            # No modification necessary 
            else:
                if not thinnedGrid:
                    numpyDataArray = data
        else:
            if not thinnedGrid:
                numpyDataArray = data
            
  
        # check sub gridding
        
        #
        # Synchronizes model information with the database.
        #
        pdsSectionValues['model'].setGridid(gdsSectionValues['coverage'].getName())
        
        self._createModelName(pdsSectionValues['model'], self.inputFile)
        modelName = pdsSectionValues['model'].getModelName()
        if modelName == "GHMNEST" or modelName == "GHM6TH" or modelName == "HWRFNEST" or modelName == "GFS" or modelName == "NAMFIREWX":
           pdsSectionValues['model'].generateId(self.inputFile)
        else:
            pdsSectionValues['model'].generateId()
        
        spatialCache = NcgribSpatialCache.getInstance()
        subCoverage = spatialCache.getSubGridCoverage(modelName)

        if subCoverage is not None:
            subGrid = spatialCache.getSubGrid(modelName)
            # resize the data array
            numpyDataArray = numpy.resize(numpyDataArray, (ny, nx))
            startx = subGrid.getStartX()
            starty = subGrid.getStartY()
            nx = subGrid.getNX()
            ny = subGrid.getNY()
            numpyDataArray = numpyDataArray[starty:starty + ny,startx:startx + nx]

            # update the number of points
            metadata[4] = nx * ny

            # set the new coverage
            gdsSectionValues['coverage'] = subCoverage

        numpyDataArray = numpy.resize(numpyDataArray, (1, metadata[4]))
        pdsSectionValues['model'].setLocation(gdsSectionValues['coverage'])

        if pdsSectionValues['model'].getParameterName() == MISSING:
            model = pdsSectionValues['model']
        else:
            model = NcgribModelCache.getInstance().getModel(pdsSectionValues['model'])
        
                # Construct the DataTime object
        if pdsSectionValues['endTime'] is None:
            dataTime = DataTime(idSectionValues['refTime'], pdsSectionValues['forecastTime'])
            duration = dataTime.getValidPeriod().getDuration()
        else:
            # endTime defines forecast time based on the difference to refTime since forecastTime is the start of the valid period
            timeRange = TimeRange(idSectionValues['refTime'].getTimeInMillis() + (pdsSectionValues['forecastTime'] * 1000), pdsSectionValues['endTime'].getTimeInMillis())
            forecastTime = int(float(pdsSectionValues['endTime'].getTimeInMillis() - idSectionValues['refTime'].getTimeInMillis()) / 1000) 
            dataTime = DataTime(idSectionValues['refTime'], forecastTime, timeRange)
            duration = dataTime.getValidPeriod().getDuration()

        newAbbr = NcgribParamTranslator.getInstance().translateParameter(2,pdsSectionValues['model'],dataTime)

        if newAbbr is None:
            if pdsSectionValues['model'].getParameterName() != MISSING and duration > 0:
                abbrStr = pdsSectionValues['model'].getParameterAbbreviation() + str(duration / 3600000) + "hr"
#                pdsSectionValues['model'].setParameterAbbreviation(abbrStr)
                model.setParameterAbbreviation(abbrStr)
        else:
#            pdsSectionValues['model'].setParameterAbbreviation(newAbbr)
            model.setParameterAbbreviation(newAbbr)
        
#        self.abbr = pdsSectionValues['model'].getParameterAbbreviation()
        self.abbr = model.getParameterAbbreviation()
        
#        if pdsSectionValues['model'].getParameterName() == MISSING:
#            model = pdsSectionValues['model']
#        else:
#            model = NcgribModelCache.getInstance().getModel(pdsSectionValues['model'])
                
        # Construct the ncgribRecord
        record = NcgribRecord()
        record.setPluginName(PLUGIN_NAME)
        record.setDataTime(dataTime)
        record.setMasterTableVersion(idSectionValues['masterTableVersion'])
        record.setLocalTableVersion(idSectionValues['localTableVersion'])
        record.setRefTimeSignificance(idSectionValues['sigRefTime'])
        #record.setProductionStatus(idSectionValues['productionStatus'])
        record.setProcessedDataType(idSectionValues['typeProcessedData'])
        record.setLocalSectionUsed(localSectionValues is not None)
        record.setLocalSection(localSectionValues)
        record.setHybridGrid(hybridCoordList is not None)
        record.setHybridCoordList(hybridCoordList)
        record.setThinnedGrid(False)
        record.setThinnedPts(thinnedPts)       
        record.setModelName(model.getModelName())
        record.setFileName(self.inputFile)
        tokens = self.inputFile.split(".")
        if len(tokens) >= 3 and tokens[2] == "firewxnest":
            record.setEventName(tokens[2]);
        else :
            record.setEventName(tokens[0])
        
        record.setModelInfo(model)  
        record.setResCompFlags(Integer(gdsSectionValues['resCompFlags']))
        
        discipline = self.dicipline;
        record.setDiscipline(discipline)
        record.setCategory(int(pdsSectionValues['category']))
        record.setParameterId(int(pdsSectionValues['parameterId']))
        pdsTemplateNumber = metadata[10]
#        pdt = metadata[10]
        record.setPdt(pdsTemplateNumber)
                   
        # In order match press unit (mb) in GEMPACK, do this scale,
        # expect to remove the scale in near future
        category = record.getCategory()
        parameterId = record.getParameterId()
        discipline = record.getDiscipline()
#        pdt = record.getPdt()
        g2scale = Grib2VarsTableLookup.getG2varsScale(discipline, category, parameterId, pdsTemplateNumber)

        if g2scale != 0:
            scale = pow(10,g2scale )
            missscale = -999999 * scale
            for i in range(0,len(numpyDataArray)):
                numpyDataArray[i] = numpyDataArray[i] * scale
            numpyDataArray = numpy.where(numpyDataArray == missscale, -999999, numpyDataArray)

        if self.AddColumn == 1:
            self.AddColumn = -1
            numpyDataArray1 = None
            nx1 = nx + 1
            metadata[4] = nx1 * ny
            numpyDataArray1 = numpy.zeros((ny, nx1), numpy.float32)
            numpyDataArray = numpy.resize(numpyDataArray, (ny, nx))

            for i in range(ny):
                for j in range(nx):
                    numpyDataArray1[i,j] = numpyDataArray[i,j]
                numpyDataArray1[i,nx] = numpyDataArray[i,0]
                
            numpyDataArray1 = numpy.resize(numpyDataArray1, (1, metadata[4]))    
            
            record.setMessageData(numpyDataArray1)
        else:
            record.setMessageData(numpyDataArray)
        
        if pdsTemplateNumber == 8 :
            if lenPds > 27 :
                interval = self.pds[26]
            
        record.setInterval(interval)
        record.setProcessType(int(pdsSectionValues['processedDataType']))
        record.setVcrdId1(int(pdsSectionValues['verticalCoordinate1']))
        record.setVcrdId2(int(pdsSectionValues['verticalCoordinate2']))
        record.setDecodedLevel1( pdsSectionValues['level1'] )
        record.setGridVersion(2)
        if  pdsSectionValues['level2'] is not None:
            record.setDecodedLevel2( pdsSectionValues['level2'] )
        
        # Special case handling for ffg grids
        if(model.getCenterid() == 9):
            from com.raytheon.edex.plugin import PluginFactory
            record.constructDataURI()
            ncgribDao = PluginFactory.getInstance().getPluginDao("ncgrib")
            result = ncgribDao.executeNativeSql("select max(gridVersion) from awips.grib where datauri like '" + 
                                     record.getDataURI() + "%'")
            resultCount = result.getResultCount()
            if(resultCount == 1 and result.getRowColumnValue(0, 0) is not None):
                newVersion = result.getRowColumnValue(0, 0).intValue() + 1
                record.setGridVersion(newVersion)
        
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
            return numpy.resize(numpy.frombuffer(localSectionData, numpy.float32), (1, len(localSectionData)))
        
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
        model = NcgribModel()
        endTime = None
        forecastTime = 0
        duration = 0
        
        centerID = idSection[0]
        subcenterID = idSection[1]

        pdsTemplateNumber = metadata[10]
        model.setPdsTemplate(pdsTemplateNumber)

        # default to UNKNOWN
        model.setLevel(LevelFactory.getInstance().getLevel(LevelFactory.UNKNOWN_LEVEL, float(0)));

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
                parameter = NcgribTableLookup.getInstance().getTableValue(centerID, subcenterID, tableName, pdsTemplate[1])

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
                
            genprocess = NcgribTableLookup.getInstance().getTableValue(centerID, subcenterID, GENPROCESS_TABLE+"center"+str(centerID), pdsTemplate[4])

            levelName = None;
            levelUnit = None;
            ncgribLevel = NcgribTableLookup.getInstance().getTableValue(centerID, subcenterID, LEVELS_TABLE, pdsTemplate[9])

            if ncgribLevel is not None:
               levelName = ncgribLevel.getAbbreviation();
               levelUnit = ncgribLevel.getUnit()
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
            
            if levelName=='SFC' and levelOneValue != float(0):
                levelOneValue=float(0)

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
            
            # Special case handling for specific PDS Templates
            if pdsTemplateNumber == 1 or pdsTemplateNumber == 11:
                pdst15 = pdsTemplate[15]
                model.setTypeEnsemble(Integer(pdst15))
                
                # Use the following codes with correct grib headers 
                # print "Type of Ensemble Forecast: ", pdst15, " perturbation number: ", pdsTemplate[16]
                #if  ( pdst15 == 0 or self.fileName.find ('ctl1') != -1 ):
                #    self.derived = 'ctl1'
                #elif ( pdst15 == 1 or self.fileName.find ('ctl2') != -1 ):
                #    self.derived = 'ctl2'
                #elif pdst15 == 2:
                #    self.derived = 'n'
                # to do
                #elif pdst15 == 3:
                #    self.derived = 'p'
                # To do
                # set perturbation number when code value = 2/3/192 to avoid 0                      

                if pdst15 == 2 or pdst15 == 3 or pdst15 == 192:
                    model.setPerturbationNumber(str(pdsTemplate[16]))
                    
                model.setNumForecasts(Integer(pdsTemplate[17]))
                
                if pdsTemplateNumber == 11:
                    endTime = GregorianCalendar(pdsTemplate[18], pdsTemplate[19] - 1, pdsTemplate[20], pdsTemplate[21], pdsTemplate[22], pdsTemplate[23])

                    numTimeRanges = pdsTemplate[24]
                    numMissingValues = pdsTemplate[25]
                    statisticalProcess = pdsTemplate[26]

            elif pdsTemplateNumber == 2 or pdsTemplateNumber == 12:
                derivedForecast = pdsTemplate[15]
                
                if (derivedForecast == 0 or derivedForecast == 1 or derivedForecast == 6):
                #    parameterAbbreviation= parameterAbbreviation+"mean"
                    self.derived = 'mean'
                elif (derivedForecast == 2  or derivedForecast == 3 ):
                #    parameterAbbreviation= parameterAbbreviation+"sprd"
                    self.derived = 'sprd'
                elif (derivedForecast >= 193  and derivedForecast <= 195 ):
                #    parameterAbbreviation= parameterAbbreviation+"prob"
                    self.derived = 'prob'
                 
                # Use the following codes with correct grib headers 
                # if (derivedForecast >= 192  and derivedForecast <= 195 ):  
                #if ( derivedForecast == 193 or self.fileName.find ('10p') != -1):
                #    self.derived = '10p'
                #elif ( derivedForecast == 194 or self.fileName.find ('50p') != -1):
                #    self.derived = '50p'
                #elif ( derivedForecast == 195 or self.fileName.find ('90p') != -1):
                #    self.derived = '90p'     
                #elif ( derivedForecast == 192 or self.fileName.find ('mode') != -1):
                #   self.derived = 'mode'
                     
                model.setTypeEnsemble(Integer(pdsTemplate[15]))
                model.setNumForecasts(Integer(pdsTemplate[16]))
                
                if(pdsTemplateNumber == 12):
                    endTime = GregorianCalendar(pdsTemplate[17], pdsTemplate[18] - 1, pdsTemplate[19], pdsTemplate[20], pdsTemplate[21], pdsTemplate[22])
                    numTimeRanges = pdsTemplate[23]
                    numMissingValues = pdsTemplate[24]
                    statisticalProcess = pdsTemplate[25]
                
                
                
            elif pdsTemplateNumber == 5 or pdsTemplateNumber == 9:
                
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

            # Constructing the NcgribModel object
            model.setCenterid(centerID)
            model.setSubcenterid(subcenterID)
            model.setBackGenprocess(pdsTemplate[3])
            model.setGenprocess(pdsTemplate[4])
            model.setParameterName(parameterName)
            model.setParameterAbbreviation(parameterAbbreviation)
            model.setParameterUnit(parameterUnit)
            
            tokens = self.inputFile.split(".")
            if len(tokens) >= 3 and tokens[2] == "firewxnest":
                model.setEventName(tokens[2])
            else:
                model.setEventName(tokens[0])

            # Constructing the Level object
            level = LevelFactory.getInstance().getLevel(levelName, levelOneValue, levelTwoValue, levelUnit)
            model.setLevel(level);
            
        
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
            model.setParameterName("Unknown")
            model.setParameterAbbreviation("Unknown")
            model.setParameterUnit("Unknown")
        
        # endtime needs to be used to calculate forecastTime and forecastTime should be used for startTime of interval
        pdsFields['forecastTime'] = forecastTime
        pdsFields['endTime'] = endTime
        pdsFields['model'] = model
        
        pdsFields['level1'] = levelOneValue #pdsTemplate[11]
        pdsFields['level2'] = levelTwoValue #pdsTemplate[14]
        pdsFields['category'] = pdsTemplate[0]
        pdsFields['parameterId'] = pdsTemplate[1]
        pdsFields['processedDataType'] = pdsTemplate[2]
        pdsFields['verticalCoordinate1'] = pdsTemplate[9]
        pdsFields['verticalCoordinate2'] = pdsTemplate[12]
    
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

            coverage = LatLonNcgridCoverage()
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
            Lon1 = lo1 + lo2
            Lon2 = lo2 + dx
            if Lon1 == 360.0 or Lon2 == 360.0:
                nx = nx + 1
                lo2 = lo2 + dx
                self.AddColumn = 1
                
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT2)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setLa2(la2)
            coverage.setLo2(lo2)
            coverage.setDx(dx)
            coverage.setDy(dy)
            coverage.setId(coverage.hashCode())
            
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

            coverage = MercatorNcgridCoverage()
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

            Lon1 = lo1 + lo2
            Lon2 = lo2 + dx
            if Lon1 == 360.0 or Lon2 == 360.0:
                nx = nx + 1
                lo2 = lo2 + dx
                self.AddColumn = 1
                            
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT)
            coverage.setMajorAxis(majorAxis)
            coverage.setMinorAxis(minorAxis)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setLatin(latin)
            coverage.setLa2(la2)
            coverage.setLo2(lo2)
            coverage.setDx(dx)
            coverage.setDy(dy)
            coverage.setId(coverage.hashCode())
            
            coverage = self._getGrid(coverage)
            
        # Polar Stereographic projection
        elif gdsTemplateNumber == 20:

            coverage = PolarStereoNcgridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            nx = gdsTemplate[7]
            ny = gdsTemplate[8]
            la1 = self._correctLat(self._divideBy10e6(gdsTemplate[9]))
            lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[10]))
            lov = self._correctLon(self._divideBy10e6(gdsTemplate[13]))
            dx = self._divideBy10e6(gdsTemplate[14])
            dy = self._divideBy10e6(gdsTemplate[15])
            scanMode = gdsTemplate[17]
            resCompFlags = gdsTemplate[11]
            
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT)
            coverage.setMajorAxis(majorAxis)
            coverage.setMinorAxis(minorAxis)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setLov(lov)
            coverage.setDx(dx)
            coverage.setDy(dy)
            coverage.setId(coverage.hashCode())
            
            coverage = self._getGrid(coverage)

        # Lambert Conformal projection
        elif gdsTemplateNumber == 30:

            coverage = LambertConformalNcgridCoverage()
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
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setLov(lov)
            coverage.setDx(dx)
            coverage.setDy(dy)
            coverage.setLatin1(latin1)
            coverage.setLatin2(latin2)
            coverage.setId(coverage.hashCode())
            
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
        elif str(gdsTemplateNumber) == "32768":
            
            coverage = LatLonNcgridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
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

            Lon1 = lo1 + lo2
            Lon2 = lo2 + dx
            if Lon1 == 360.0 or Lon2 == 360.0:
                nx = nx + 1
                lo2 = lo2 + dx
                self.AddColumn = 1

            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT2)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setLa2(la2)
            coverage.setLo2(lo2)
            coverage.setDx(dx)
            coverage.setDy(dy)
            coverage.setId(coverage.hashCode())
            
            coverage = self._getGrid(coverage)
        
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
        cache = NcgribSpatialCache.getInstance()
        
        # Check the cache first
        grid = cache.getGrid(temp)

        # If not found, create a new GribCoverage and store in the cache
        if grid is None:
            cache.putGrid(temp, True)
            grid = cache.getGrid(temp.getId())

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
            minorAxis = float(gdsTemplate[2])
            majorAxis = float(gdsTemplate[2])
            
        # Earth assumed oblate spheriod with size as determined by IAU in 1965
        # (major axis = 6,378,160.0 m, minor axis = 6,356,775.0 m, f = 1/297.0)   
        elif number == 2:
            minorAxis = 6356775.0
            majorAxis = 6378160.0
            
        # Earth assumed oblate spheriod with major and minor axes specified (in km) by data producer
        elif number == 3:
            if gdsTemplate[3] > 0:
                minorAxis = gdsTemplate[4] * gdsTemplate[3] * 1000
            else:
                minorAxis = gdsTemplate[4] * 1000
                
            if gdsTemplate[5] > 0:
                majorAxis = gdsTemplate[6] * gdsTemplate[5] * 1000
            else:
                majorAxis = gdsTemplate[6] * 1000
                
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
            if gdsTemplate[3] > 0:
                minorAxis = gdsTemplate[4] * gdsTemplate[3]
            else:
                minorAxis = gdsTemplate[4]
                
            if gdsTemplate[5] > 0:
                majorAxis = gdsTemplate[6] * gdsTemplate[5]
            else:
                majorAxis = gdsTemplate[6]
                
        # Earth model assumed spherical with radius 6,371,200 m,
        # but the horizontal datum of the resulting Latitude/Longitude field is
        # the WGS84 reference frame
        elif number == 8:
            minorAxis = 6371200.0
            majorAxis = 6371200.0
        else:
            minorAxis = MINOR_AXIS_DEFAULT
            majorAxis = MAJOR_AXIS_DEFAULT
        
        return majorAxis, minorAxis
    
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

    def _createModelName(self, model, fileName):
        
        center = model.getCenterid()
        subcenter = model.getSubcenterid()

        gridid = model.getGridid()
        process = model.getGenprocess()
        gridModel = NcgribModelLookup.getInstance().getModel(center, subcenter, gridid, process, fileName, model)

        if gridModel is None:
            name = "NewGrid:" + str(center) + ":" + str(subcenter) + ":" + str(process) + ":" + gridid
            tokens = fileName.split(".")
            hurricane = tokens[0]
            basin = hurricane[-1]
            trackno = hurricane[-3:-1]
            if trackno.isdigit() or tokens[2] =="firewxnest" or tokens[2] == "hysplit":
                if trackno.isdigit():
                    basins = "lewcs"
                    if basin in basins:
                    #name = "GHM:" + str(center) + ":" + str(subcenter) + ":" + str(process) + ":" + gridid
                    #hurricaneName = hurricane[:len(hurricane)-3]
                        name = "ghm"
                        if tokens[2] == "gribn3":
                            name = "ghmNest"
                        elif tokens[2] == "grib6th":
                            name = "ghm6th"
                        elif tokens[2] == "hwrfprs_n":
                            name = "hwrfNest"
                        elif tokens[2] == "hwrfprs_p":
                            name = "hwrf"
                elif tokens[2] == "firewxnest":
                    name = "fireWxNest"       
                    
                else: 
                    name = "hysplit"  
                NcgribModelLookup.getInstance().setModel(center, subcenter, gridid, process, name)
                gridModel = NcgribModelLookup.getInstance().getModel(center, subcenter, gridid, process, filename)
                    #name = gridModel.getName()
        else:
            name = gridModel.getName()
            
        model.setModelName(name)
      
    ##
    # Add record to record list if its parameters are in
    # NCEP VARS and VCRD control files.
    #
    # @param records: the record list.
    # @param record: the record is going to be checked.
    ##
    def _addRecord(self, records, record):
        
        category = record.getCategory()
        discipline = record.getDiscipline()
        pdt = record.getPdt()
        parameterId = record.getParameterId()
        vcrd1 = record.getVcrdId1()
        vcrd2 = record.getVcrdId2()
        
        g2varsId = -1
        g2vcrdId = -1
        
        vcord=''
        scale=''
        parm=''
        accumHour=0
        charHour=''
        
        #calculate accumulative hour
        interval = record.getInterval()
        if pdt == 8 :
            accumHour = interval
            if accumHour < 12 :
                charHour = '0' + str(accumHour)
            else :
                charHour = str(accumHour)
        
        g2vcrdId = Grib2VcrdTableLookup.getG2vcrdId(vcrd1, vcrd2)
        
        if str(g2vcrdId).isdigit() :
            # get and set vcord and scale
            vcord = Grib2VcrdTableLookup.getGnamByVcrdId(vcrd1, vcrd2)
            record.setVcord(vcord)
            scale = float( Grib2VcrdTableLookup.getScaleByVcrdId(vcrd1, vcrd2) )
            scale = pow ( 10, scale)
            #record.setScale(scale)
            glevel1 = record.getDecodedLevel1()
            if (glevel1 != 0 and scale != 1 ) :
                record.setGlevel1(int(round(glevel1 * scale)))
            else :
                record.setGlevel1(int(round(glevel1)))
                
            if vcrd2 == 255 :
                record.setGlevel2(-9999) 
            else :
                glevel2 = record.getDecodedLevel2()
                if (glevel2 != 0 and scale != 1 ) :
                    record.setGlevel2(int(round(glevel2 * scale)))
                else :
                    record.setGlevel2( int(round(glevel2) ))
            levelName = record.getModelInfo().getLevelName()
            levelOneValue = 1.0*record.getGlevel1()
            levelTwoValue = 1.0*record.getGlevel2()
            levelUnit = record.getModelInfo().getLevelUnit()
            level = LevelFactory.getInstance().getLevel(levelName, levelOneValue, levelTwoValue, levelUnit)
            record.getModelInfo().setLevel(level)
        else :
            record.setGlevel1( int(record.getDecodedLevel1) )
            record.setGlevel2( int(record.getDecodedLevel2) )
            LogStream.logEvent("No vertical coordinate ID for first fixed surfaced level" + str(vcrd1) + "], and second fixed surfaced [" + 
                                     str(vcrd2) + "]");
            
  
        if str(g2vcrdId).isdigit() :
            g2varsId = Grib2VarsTableLookup.getG2varsId(discipline, category, parameterId, pdt)

    #        print ("5=", discipline, category, parameterId, pdt,g2varsId, parm)
            if g2varsId > 0 :
                parm = Grib2VarsTableLookup.getVarGnam(discipline, category, parameterId, pdt)
                modelName = record.getModelName()
                #if self.derived == 'mean':
                #    parm = parm + "ENMW"
                #    if modelName.find('Mean') == -1:   
                #        modelName = modelName +"Mean"
                #if self.derived == 'sprd':
                #    parm = parm + "ENSA"
                #    if modelName.find('Spread') == -1 : 
                #        modelName = modelName +"Spread"
                #elif self.derived == 'prob':  
                #    if modelName.find('PROB') == -1 :                   
                #        modelName = modelName +"PROB"
                #elif self.derived == 'mode':
                #parm = parm + "ENMO"
                #    if modelName.find('MODE') == -1 : 
                #        modelName = modelName +"MODE"   
                #elif self.derived == '10p':
                # parm = parm + "PROB"
                #    if modelName.find ('10P') == -1: 
                #        modelName = modelName +"10P"
                #elif self.derived == '50p':
                #    if modelName.find ('50P') == -1: 
                #        modelName = modelName + "50P"
                #elif self.derived == '90p':
                #    if modelName.find ('90P') == -1: 
                #        modelName = modelName + "90P"
                #elif self.derived == 'ctl1' or self.derived == 'ctl2':                    
                #    if modelName.find ('CTL') == -1:                     
                #        modelName = modelName + self.derived
                
                record.setModelName(modelName)
                record.getModelInfo().setModelName(modelName)
                
                if pdt == 8 :
                    record.setParm(parm.replace("--", charHour))
                else :
                    record.setParm(parm)
            record.setProcessType(self.count)        
            if g2varsId > 0 :
                # the parameter ID is in the table so append the record
                records.append(record)
            else :
                if discipline != 255:
                    LogStream.logEvent("No variable ID for discipline:", discipline, "category =",category, "parameterId =", parameterId, "pdt=", pdt);
        self.derived = None
        self.count = self.count + 1
        