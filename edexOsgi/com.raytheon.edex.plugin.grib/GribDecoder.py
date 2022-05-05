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
#
#  Python implementation of the grib decoder.  This decoder uses the grib2 module
# to access the NCEP grib decoder for extracting data
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer    Description
# ------------- -------- ----------- --------------------------
# Apr 07, 2009  1994     bphillip    Initial Creation.
# Mar 25, 2013  1821     bsteffen    Reshape grib data arrays in place to
#                                    improve performance.
# Sep 04, 2013  2298     rjpeter     Removed setPluginName call
# Sep 06, 2013  2336     bsteffen    Switch from logstream to logging with
#                                    UFStatusHandler.
# Sep 06, 2013  2402     bsteffen    Switch to use file extents for multipart
#                                    grib files.
# Feb 11, 2014  2765     bsteffen    Better handling of probability parameters.
# Apr 28, 2014  3084     bsteffen    Use full grid for looking up parameter aliases.
# Jul 07, 2014  3344     rferrel     Change GRID_FILL_VALUE to new plugin location.
# Aug 15, 2014  15699    MPorricelli Import GridUtil and update reference
#                                    to GRID_FILL_VALUE
# Dec 15, 2014  DR16509  Matt Foster Changes in _decodePdsSection to accommodate
#                                    EKDMOS
# Mar 05, 2015  3959     rjpeter     Update sub gridding to handle world wrap.
# Mar 05, 2015  3959     rjpeter     Fix subgrid across seam of world wide grid.
# Jul 28, 2015  4264     njensen     Use constant float32 fill value
# Oct 01, 2015  4868     rjpeter     Discard invalid subgrids.
# Dec 16, 2015  5182     tjensen     Updated GribModelLookup calls to pass in filepath.
# Oct 31, 2016  5979     njensen     Cast to primitives for compatibility
# Mar 10, 2017  6171     bsteffen    Improve handling of probability grids for NBM
# Jun 29, 2017  6323     randerso    Changes for P-ETSS model
# Jul 17, 2017  DR19976  MPorricelli Add labeling of Probability Matched Mean fields#
# Nov 29, 2017  6536     bsteffen    Handle redundant statistical specifiers better.
# Jan 30, 2018  7172     bsteffen    Recalculate more precise dx for some worldwide grids.
# Feb 07, 2018  7213     nabowle     Only allow P-ETSS to create Statistical parameters.
# Mar 19, 2018  20395    wkwock      Added PDS template 15
# Jul  3, 2019  7879     tgurney     Python 3 fixes
#

import grib2
import numpy
from math import pow
import logging
import UFStatusHandler
import re
from matplotlib.mlab import griddata

from java.lang import Float
from java.lang import Integer

from java.util import GregorianCalendar

from com.raytheon.uf.common.time import DataTime
from com.raytheon.uf.common.time import TimeRange

from com.raytheon.uf.common.dataplugin.grid import GridRecord

from com.raytheon.uf.common.gridcoverage import LambertConformalGridCoverage
from com.raytheon.uf.common.gridcoverage import LatLonGridCoverage
from com.raytheon.uf.common.gridcoverage import MercatorGridCoverage
from com.raytheon.uf.common.gridcoverage import PolarStereoGridCoverage
from com.raytheon.uf.common.gridcoverage.lookup import GridCoverageLookup

from com.raytheon.uf.common.grib import GribModelLookup
from com.raytheon.uf.common.grib.tables import GribTableLookup

from com.raytheon.uf.common.dataplugin.level.mapping import LevelMapper
from com.raytheon.uf.common.dataplugin.level import Level
from com.raytheon.uf.common.dataplugin.level import LevelFactory

from com.raytheon.edex.plugin.grib.spatial import GribSpatialCache
from com.raytheon.uf.common.util import GridUtil

from com.raytheon.edex.util.grib import GribParamTranslator

from com.raytheon.uf.common.parameter import Parameter
from com.raytheon.uf.common.parameter.mapping import ParameterMapper

# default fill value for now...someday NaN would be better
F32_GRID_FILL_VALUE = numpy.float32(GridUtil.GRID_FILL_VALUE)

# Static values for accessing parameter lookup tables
PARAMETER_TABLE = "4.2"
PROCESS_TYPE_TABLE = "4.3"
LEVELS_TABLE = "4.5"
DOT = "."
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

THINNED_GRID_VALUES = list(THINNED_GRID_PT_MAP.values())

logHandler = UFStatusHandler.UFStatusHandler("com.raytheon.edex.plugin.grib", "EDEX")

class GribDecoder():

    ##
    # Initializes the grib decoder
    #
    # @param filePath: The file to decode
    # @param startPosition: The start position of the grib message
    # @param messageLength: The length of the grib message
    ##
    def __init__(self, filePath, startPosition, messageLength):
        # Assign public file name
        self.fileName = filePath
        self.startPosition = startPosition
        self.messageLength = messageLength


        self.log = logging.getLogger("GribDecoder")
        self.log.addHandler(logHandler)


    ##
    # Decodes the grib file
    #
    # @return: List of decoded GribRecords
    # @rtype: List
    ##
    def decode(self):
        # The GribRecords to be returned back to Java
        records = []

        gribDictList = []
        gribFile = open(self.fileName, "rb")
        try:
            # This structure is a list of dicts for each field. For more
            # information on what keys are available see the documentation on
            # the gribfield struct in g2clib-1.1.8/grib2c.doc
            gribDictList = grib2.decode(gribFile, self.startPosition, self.messageLength)
        except:
            self.log.exception("Error processing file [" + self.fileName + "]: ")
        finally:
            gribFile.close()
        for gribDict in gribDictList:
            record = self._getData(gribDict)
            if record is not None:
                records.append(record)

        return records

    ##
    # Decodes a single record contained in the grib file
    #
    # @param gribDict: a single gribDict from the grib2 module decoder.
    # @return: Decoded GridRecord object
    # @rtype: GridRecord
    ##
    def _getData(self, gribDict):
        self._decodeIdSection(gribDict)
        self._decodeGdsSection(gribDict)
        self._decodePdsSection(gribDict)

        # Construct the DataTime object
        refTime = gribDict['refTime']
        if 'endTime' in gribDict:
            endTime = gribDict['endTime']
            # endTime defines forecast time based on the difference to refTime since forecastTime is the start of the valid period
            timeRange = TimeRange(refTime.getTimeInMillis() + (gribDict['forecastTime'] * 1000), endTime.getTimeInMillis())
            forecastTime = int(float(endTime.getTimeInMillis() - refTime.getTimeInMillis()) // 1000)
            dataTime = DataTime(refTime, forecastTime, timeRange)
        elif 'forecastTime' in gribDict:
            dataTime = DataTime(refTime, gribDict['forecastTime'])
        else:
            dataTime = DataTime(refTime, 0)

        data = gribDict['fld']

        numpyDataArray = None

        # Special case for thinned grids.
        # Map the thinned grid on to a square lat/lon grid
        if 'thinned' in gribDict:
            optValues = gribDict['list_opt']
            optList = numpy.zeros(len(optValues), numpy.int32)
            for i in range(0, len(optValues)):
                optList[i] = optValues[i]

            dataArray = numpy.zeros(len(data), numpy.float32)
            for i in range(0, len(data)):
                dataArray[i] = data[i]


            # Temporary place holder pending Numpy update
            numpyDataArray = numpy.zeros((THINNED_GRID_PTS, THINNED_GRID_PTS), numpy.float32)

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
        if gribDict['ibmap'] == 0:
            bitMap = gribDict['bmap']
            data = numpy.where(bitMap == 0, F32_GRID_FILL_VALUE, data)

        # Check for fill value provided if complex packing is used
        drsTemplateNumber = gribDict['idrtnum']
        if drsTemplateNumber in [2, 3]:
            drs = gribDict['idrtmpl']
            primaryFill = Float.intBitsToFloat(int(drs[7]))
            secondaryFill = Float.intBitsToFloat(int(drs[8]))
            if drs[6] == 1:
                data = numpy.where(data == primaryFill, F32_GRID_FILL_VALUE, data)
            elif drs[6] == 2:
                data = numpy.where(data == primaryFill, F32_GRID_FILL_VALUE, data)
                data = numpy.where(data == secondaryFill, F32_GRID_FILL_VALUE, data)

        gridCoverage = gribDict['coverage']
        nx = int(gridCoverage.getNx())
        ny = int(gridCoverage.getNy())

        # Correct the data according to the scan mode found in the gds section.
        scanMode = gribDict['scanMode']
        if scanMode is not None:

            if 'thinned' not in gribDict:
                numpyDataArray = numpy.reshape(data, (ny, nx))

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

        elif 'thinned' not in gribDict:
                numpyDataArray = data

        modelName = self._createModelName(gribDict, gridCoverage)
        #check if forecast used flag needs to be removed
        self._checkForecastFlag(gribDict, gridCoverage, dataTime)
        # check parameter abbreivation mapping
        parameterAbbreviation = gribDict['parameterAbbreviation']
        newAbbr = GribParamTranslator.getInstance().translateParameter(2, parameterAbbreviation, gribDict['center'], gribDict['subcenter'], gribDict['genprocess'], dataTime, gridCoverage)

        if newAbbr is None:
            if gribDict['parameterName'] != MISSING and dataTime.getValidPeriod().getDuration() > 0:
               parameterAbbreviation = parameterAbbreviation + str(dataTime.getValidPeriod().getDuration() // 3600000) + "hr"
        else:
            parameterAbbreviation = newAbbr
        parameterAbbreviation = parameterAbbreviation.replace('_', '-')

        # check sub gridding
        spatialCache = GribSpatialCache.getInstance()
        subCoverage = spatialCache.getSubGridCoverage(modelName, gridCoverage)

        if subCoverage is not None:
            subGrid = spatialCache.getSubGrid(modelName, gridCoverage)

            startx = subGrid.getUpperLeftX()
            starty = subGrid.getUpperLeftY()
            subnx = subGrid.getNX()
            subny = subGrid.getNY()
            endY = starty + subny
            endX = startx + subnx

            if subnx <= 0 or subny <= 0:
                # sub grid did not intersect main grid
                self.log.info("Discarding model [" + modelName + "], sub grid does not meet minimum coverage area")
                return None

            # resize the data array
            numpyDataArray = numpy.reshape(numpyDataArray, (ny, nx))

            # handle world wide grid wrap
            if (endX > nx):
                subGridDataArray = numpy.zeros((subny, subnx), numpy.float32)
                endX = nx
                wrapCount = gridCoverage.getWorldWrapCount()

                if wrapCount > 0:
                    # handle grid that comes in with data already wrapped
                    endX = wrapCount

                midx = endX - startx
                subGridDataArray[0:subny, 0:midx] = numpyDataArray[starty:endY, startx:endX]
                if (wrapCount > 0):
                    subGridDataArray[0:subny, midx:subnx] = numpyDataArray[starty:endY, 0:subnx - midx]
                else:
                    subGridDataArray[0:subny, midx:subnx] = GridUtil.GRID_FILL_VALUE
                numpyDataArray = subGridDataArray
            else:
                numpyDataArray = numpyDataArray[starty:endY, startx:endX]

            # update the number of points
            nx = subnx
            ny = subny
            gribDict['ngrdpts'] = nx * ny

            # set the new coverage
            gridCoverage = subCoverage

        numpyDataArray = numpy.reshape(numpyDataArray, (1, gribDict['ngrdpts']))

        # Construct the GribRecord
        record = GridRecord()
        record.setDataTime(dataTime)
        record.setMessageData(numpyDataArray)
        record.setLocation(gridCoverage)
        record.setLevel(gribDict['level'])
        record.setDatasetId(modelName)

        if "ensembleId" in gribDict:
            record.setEnsembleId(gribDict['ensembleId'])
        param = Parameter(parameterAbbreviation, gribDict['parameterName'], gribDict['parameterUnit'])
        GribParamTranslator.getInstance().getParameterNameAlias(modelName, param)
        record.setParameter(param)

        # TODO this can be removed when grib table is removed.
        record.addExtraAttribute("centerid", Integer(gribDict['center']))
        record.addExtraAttribute("subcenterid", Integer(gribDict['subcenter']))
        record.addExtraAttribute("genprocess", Integer(gribDict['genprocess']))
        record.addExtraAttribute("backGenprocess", Integer(gribDict['backGenprocess']))
        record.addExtraAttribute("pdsTemplate", Integer(gribDict['ipdtnum']))
        record.addExtraAttribute("gridid", gridCoverage.getName())
        if "forecastInterval" in gribDict:
            record.addExtraAttribute("forecastInterval", gribDict['forecastInterval'])
        if "forecastIntervalUnit" in gribDict:
            record.addExtraAttribute("forecastIntervalUnit", gribDict['forecastIntervalUnit'])
        if "numForecasts" in gribDict:
            record.addExtraAttribute("numForecasts", gribDict['numForecasts'])

        return record

    ##
    # Decodes the values from the id section. Decoded values are added to the gribDict
    # @param gribDict: a single gribDict from the grib2 module decoder.
    ##
    def _decodeIdSection(self, gribDict):
        idSection = gribDict['idsect']

        gribDict['center'] = int(idSection[0])
        gribDict['subcenter'] = int(idSection[1])

        #gribDict['masterTableVersion'] = int(idSection[2])
        #gribDict['localTableVersion'] = int(idSection[3])
        #gribDict['sigRefTime'] = int(idSection[4])
        gribDict['refTime'] = self._convertToCalendar(idSection, 5)
        #gribDict['productionStatus'] = int(idSection[11])
        #gribDict['typeProcessedData'] = int(idSection[12])

    ##
    # Decodes the values from the pds section. Decoded values are added to the gribDict
    # @param gribDict: a single gribDict from the grib2 module decoder.
    ##
    def _decodePdsSection(self, gribDict):
        pdsTemplate = gribDict['ipdtmpl']
        pdsTemplateNumber = gribDict['ipdtnum']
        centerID = gribDict['center']
        subcenterID = gribDict['subcenter']

        # Templates 0-11 are ordered the same for the most part and can therefore be processed the same
        # Exception cases are handled accordingly

        if pdsTemplateNumber <= 12 or pdsTemplateNumber == 15:

            # Get the basic level and parameter information
            if (pdsTemplate[0] == 255):
                gribDict['parameterName'] = MISSING
                parameterAbbreviation = MISSING
                gribDict['parameterUnit'] = MISSING
            else:
                discipline = gribDict['discipline']
                tableName = PARAMETER_TABLE + DOT + str(discipline) + DOT + str(pdsTemplate[0])
                parameter = GribTableLookup.getInstance().getTableValue(centerID, subcenterID, tableName, int(pdsTemplate[1]))

                if parameter is not None:
                    gribDict['parameterName'] = parameter.getName()

                    if parameter.getD2dAbbrev() is not None:
                        parameterAbbreviation = parameter.getD2dAbbrev()
                    else:
                        parameterAbbreviation = parameter.getAbbreviation()
                    gribDict['parameterUnit'] = parameter.getUnit()
                else:
                    self.log.info("No parameter information for center[" + str(centerID) + "], subcenter[" +
                                          str(subcenterID) + "], tableName[" + tableName +
                                          "], parameter value[" + str(pdsTemplate[1]) + "]")
                    gribDict['parameterName'] = MISSING
                    parameterAbbreviation = MISSING
                    gribDict['parameterUnit'] = MISSING

            processType = int(pdsTemplate[2])
            gribDict['processType'] = str(GribTableLookup.getInstance().getTableValue(centerID, subcenterID, PROCESS_TYPE_TABLE, processType))

            levelName = None
            levelUnit = None
            
            #In case the 1st level is 'SFC' and the 2nd level is 'FHAG', use FHAG as the level number
            levelNumber = int(pdsTemplate[9])
            levelNumber2 = int(pdsTemplate[12])
            if levelNumber == 1 and levelNumber2 == 103:
                levelNumber = levelNumber2
            gribLevel = GribTableLookup.getInstance().getTableValue(centerID, subcenterID, LEVELS_TABLE, levelNumber)

            if gribLevel is not None:
               levelName = gribLevel.getAbbreviation()
               levelUnit = gribLevel.getUnit()
            else:
               self.log.info("No level information for center[" + str(centerID) + "], subcenter[" +
                                     str(subcenterID) + "], tableName[" + LEVELS_TABLE + "], level value[" +
                                     str(pdsTemplate[9]) + "]")

            if levelName is None or len(levelName) == 0:
                levelName = LevelFactory.UNKNOWN_LEVEL

            # Convert the forecast time to seconds
            gribDict['forecastTime'] = self._convertToSeconds(pdsTemplate[8], pdsTemplate[7])
            # harvest forecast interval for longer term models to post process
            gribDict['forecastInterval'] = Integer(int(pdsTemplate[8]))
            gribDict['forecastIntervalUnit'] = Integer(int(pdsTemplate[7]))

            # Store genprocess info
            gribDict['backGenprocess'] = int(pdsTemplate[3])
            gribDict['genprocess'] = int(pdsTemplate[4])

            # Scale the level one value if necessary
            if pdsTemplate[10] == 0 or pdsTemplate[11] == 0:
                levelOneValue = float(pdsTemplate[11])
            else:
                levelOneValue = float(pdsTemplate[11] * pow(10, pdsTemplate[10] * -1))

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
                    levelTwoValue = float(pdsTemplate[14] * pow(10, pdsTemplate[13] * -1))

            if levelName == 'EATM' or levelName == 'SFC':
                levelOneValue = float(0)
                levelTwoValue = float(Level.getInvalidLevelValue())

            durationSecs = None
            typeOfTimeInterval = None

            # Special case handling for specific PDS Templates
            if pdsTemplateNumber == 1 or pdsTemplateNumber == 11:
                typeEnsemble = int(pdsTemplate[15])
                perturbationNumber = int(pdsTemplate[16])
                gribDict['numForecasts'] = Integer(int(pdsTemplate[17]))
                if(typeEnsemble == 0):
                     gribDict['ensembleId'] = "ctlh" + str(perturbationNumber)
                elif(typeEnsemble == 1):
                     gribDict['ensembleId'] = "ctll" + str(perturbationNumber)
                elif(typeEnsemble == 2):
                     gribDict['ensembleId'] = "n" + str(perturbationNumber)
                elif(typeEnsemble == 3):
                     gribDict['ensembleId'] = "p" + str(perturbationNumber)
                else:
                    gribDict['ensembleId'] = str(typeEnsemble) + "." + str(perturbationNumber)

                if pdsTemplateNumber == 11:
                    gribDict['endTime'] = self._convertToCalendar(pdsTemplate, 18)
                    #numTimeRanges = pdsTemplate[24]
                    #numMissingValues = pdsTemplate[25]
                    #statisticalProcess = pdsTemplate[26]

            elif pdsTemplateNumber == 2 or pdsTemplateNumber == 12:
                derivedForecast = pdsTemplate[15]

                if ((derivedForecast == 1 or derivedForecast == 0) and processType == 193):
                    parameterAbbreviation= parameterAbbreviation+"pmmn"
                elif (derivedForecast == 1 or derivedForecast == 0):
                    parameterAbbreviation = parameterAbbreviation + "mean"
                elif (derivedForecast == 2 or derivedForecast == 3 or derivedForecast == 4):
                    parameterAbbreviation = parameterAbbreviation + "sprd"

                gribDict['numForecasts'] = Integer(int(pdsTemplate[16]))

                if(pdsTemplateNumber == 12):
                    gribDict['endTime'] = self._convertToCalendar(pdsTemplate, 17)
                    #numTimeRanges = pdsTemplate[23]
                    #numMissingValues = pdsTemplate[24]
                    #statisticalProcess = pdsTemplate[25]

            elif pdsTemplateNumber == 5 or pdsTemplateNumber == 9:
                probabilityType = pdsTemplate[17]
                scaleFactorLL = pdsTemplate[18]
                scaledValueLL = pdsTemplate[19]
                scaleFactorUL = pdsTemplate[20]
                scaledValueUL = pdsTemplate[21]

                upperLimit = self._convertScaledValue(scaledValueUL, scaleFactorUL)
                lowerLimit = self._convertScaledValue(scaledValueLL, scaleFactorLL)

                if(pdsTemplateNumber == 9):
                    gribDict['endTime'] = self._convertToCalendar(pdsTemplate, 22)
                    #numTimeRanges = pdsTemplate[28]
                    #numMissingValues = pdsTemplate[29]
                    #statisticalProcess = pdsTemplate[30]
                    typeOfTimeInterval = pdsTemplate[31]
                    durationSecs = self._convertToSeconds(pdsTemplate[33], pdsTemplate[32])

                parameterSuffix = None
                unit = gribDict['parameterUnit']
                if probabilityType == 0:
                    parameterSuffix = str(lowerLimit)
                    gribDict['parameterName'] = "Prob of " + gribDict['parameterName'] + " < " + parameterSuffix + unit
                elif probabilityType == 1:
                    parameterSuffix = str(upperLimit)
                    gribDict['parameterName'] = "Prob of " + gribDict['parameterName'] + " > " + parameterSuffix + unit
                elif probabilityType == 2:
                    originalName = gribDict['parameterName']
                    gribDict['parameterName'] = "Prob of " + originalName + " between " + str(lowerLimit) + " and " + str(upperLimit) + " " + unit
                    parameterSuffix = str(lowerLimit) + '-' + str(upperLimit)
                    if "Code table 4." in unit and lowerLimit + 1 == upperLimit:
                        i = unit.index("4.2")
                        table = unit[i:i + 5]
                        codeValue = GribTableLookup.getInstance().getTableValue(centerID, subcenterID, table, int(lowerLimit))
                        if codeValue is not None:
                            gribDict['parameterName'] = "Prob " + originalName + " is " + codeValue
                            parameterSuffix = codeValue.title().replace(" ", "")
                elif probabilityType == 3:
                    parameterSuffix = str(lowerLimit)
                    gribDict['parameterName'] = "Prob of " + gribDict['parameterName'] + " > " + parameterSuffix + unit
                elif probabilityType == 4:
                    parameterSuffix = str(upperLimit)
                    gribDict['parameterName'] = "Prob of " + gribDict['parameterName'] + " < " + parameterSuffix + unit

                if "Code table" in unit:
                    parameterAbbreviation = parameterAbbreviation + parameterSuffix
                elif parameterSuffix is not None:
                    parameterAbbreviation = parameterAbbreviation + parameterSuffix + unit

                gribDict['parameterUnit'] = "%"

            elif pdsTemplateNumber == 8:
                gribDict['endTime'] = self._convertToCalendar(pdsTemplate, 15)

                if self._isStatisticalModel(gribDict):
                    #numTimeRanges = pdsTemplate[21]
                    #numMissingValues = pdsTemplate[22]
                    statisticalProcess = pdsTemplate[23]
                    if parameterAbbreviation == MISSING:
                        # Do not alter the abbreviation of missing param, it is still missing.
                        pass
                    elif statisticalProcess == 0:
                        parameterAbbreviation = parameterAbbreviation + "mean"
                        gribDict['parameterName'] = "Mean " + gribDict['parameterName']
                    elif statisticalProcess == 2:
                        # Parameters that already indicate MAX(or MX) do not need to be modified
                        if not re.match('MA?X', parameterAbbreviation, re.I):
                            parameterAbbreviation = parameterAbbreviation + "max"
                            gribDict['parameterName'] = "Max " + gribDict['parameterName']
                    elif statisticalProcess == 3:
                        # Parameters that already indicate MIN(or MN) do not need to be modified
                        if not re.match('MI?N', parameterAbbreviation, re.I):
                            parameterAbbreviation = parameterAbbreviation + "min"
                            gribDict['parameterName'] = "Min " + gribDict['parameterName']

                    typeOfTimeInterval = pdsTemplate[24]


            elif pdsTemplateNumber == 6 or pdsTemplateNumber == 10:
                # pdsTemplate 6 and 10 are used for percentile-based variables
                # 6 is for instantaneous variables, 10 is for those that span
                # a time range
                parameterAbbreviation = parameterAbbreviation + str(pdsTemplate[15]) + "pct"
                gribDict['parameterName'] = str(pdsTemplate[15]) + "th percentile " + gribDict['parameterName']

                if pdsTemplateNumber == 10:
                    # Add time range information for pdsTemplate 10
                    gribDict['endTime'] = self._convertToCalendar(pdsTemplate, 16)

                    #numTimeRanges = pdsTemplate[22]
                    #numMissingValues = pdsTemplate[23]
                    #statisticalProcess = pdsTemplate[24]

                    typeOfTimeInterval = pdsTemplate[25]
                    durationSecs = self._convertToSeconds(pdsTemplate[27], pdsTemplate[26])


            if durationSecs is not None:
                # This only applies for templates 9 and 10 which are not
                # commonly used templates. For all other data the duration is
                # ignored and it is assumed that reftime, forecast time, and
                # endtime will define the duration. For Template 9 and 10 this
                # will cause forecast time to be ignored so duration is correct.

                # The decoder assumes reftime + forecastTime equals
                # endTime - duration, however for some models
                # reftime + forecasttime instead equals endTime. This reassigns
                # forecastTime as endTime - refTime - duration so that
                # duration is correctly calculated.
                refToEndSecs = (gribDict['endTime'].getTimeInMillis() - gribDict['refTime'].getTimeInMillis()) // 1000
                gribDict['forecastTime'] = refToEndSecs - durationSecs


            if typeOfTimeInterval == 192 and centerID == 7 and subcenterID == 14:
                # For TPC Surge data the type of time interval is significant and they have indicated that
                # 192 means the data is cumulative. Since we don't ordinarily do table lookups on the
                # type of time interval we must encode this information in the parameter abbreviation here.
                parameterAbbreviation = parameterAbbreviation + "Cumul"
                gribDict['parameterName'] = gribDict['parameterName'] + " - cumulative"

            if(pdsTemplate[2] == 6 or pdsTemplate[2] == 7):
                parameterAbbreviation = parameterAbbreviation + "erranl"

            parameterAbbreviation = ParameterMapper.getInstance().lookupBaseName(parameterAbbreviation, "grib")
            # Constructing the GribModel object
            gribDict['parameterAbbreviation'] = parameterAbbreviation

            # Constructing the Level object
            gribDict['level'] = LevelMapper.getInstance().lookupLevel(levelName, 'grib', levelOneValue, levelTwoValue, levelUnit)



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
        if 'parameterAbbreviation' not in gribDict:
            gribDict['parameterAbbreviation'] = "Unknown"
        if 'parameterName' not in gribDict:
            gribDict['parameterName'] = "Unknown"
        if 'parameterUnit' not in gribDict:
            gribDict['parameterUnit'] = "Unknown"

        if 'level' not in gribDict:
            gribDict['level'] = LevelFactory.getInstance().getLevel(LevelFactory.UNKNOWN_LEVEL, float(0))

    ##
    # Decodes the values from the gds section. Decoded values are added to the gribDict
    # @param gribDict: a single gribDict from the grib2 module decoder.
    ##
    def _decodeGdsSection(self, gribDict):
        gdsTemplate = gribDict['igdtmpl']
        gdsTemplateNumber = gribDict['igdtnum']

        # Latitude/Longitude projection
        if gdsTemplateNumber == 0:
            coverage = LatLonGridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            la1 = self._divideBy10e6(gdsTemplate[11])
            lo1 = self._divideBy10e6(gdsTemplate[12])
            la2 = self._divideBy10e6(gdsTemplate[14])
            lo2 = self._divideBy10e6(gdsTemplate[15])
            gribDict['scanMode'] = int(gdsTemplate[18])
            # gribDict['resCompFlags'] = gdsTemplate[13]


            # Check for quasi-regular grid
            if gribDict['numoct_opt'] > 0:
                # Quasi-regular grid detected
                gribDict['thinned'] = True
                nx = THINNED_GRID_PTS
                ny = THINNED_GRID_PTS
                dx = THINNED_GRID_SPACING
                dy = THINNED_GRID_SPACING
                gribDict['ngrdpts'] = THINNED_GRID_REMAPPED_SIZE
            else:
                # Not a quasi-regular grid
                nx = int(gdsTemplate[7])
                ny = int(gdsTemplate[8])
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
                dx = abs(lo1 - lo2) / nx
            if dy >= 65.535:
                dy = abs(la1 - la2) / ny
                
            # Fix rounding errors that can create grids wider than 360° because
            # grids larger than the world are problematic for a variety of
            # algorithms. This occurs because dx is only encoded to 6 decimal
            # places and so it may be rounded from what it should be. For
            # example a worldwide grid with ⅔° degree spacing would be encoded
            # with nx=540 and dx=0.666667°, resulting in a total width of
            # 360.00018°. This code will change dx to 0.666666666648° which is
            # closer to the real spacing of ⅔° and also makes the total width
            # just under 360°.
            #
            # This test finds the next smallest dx that could have been encoded
            # by subtracting 0.000001 from dx. If the current value of dx
            # causes the grid to be wider than 360° but the next smallest dx
            # would have caused the grid to be narrower than 360° then assume
            # that the grid is supposed to be exactly 360° and dx was rounded.
            # In this case calculate a more accurate dx by dividing
            # 359.99999999° by nx. A value just under 360° is used so that any
            # floating point inaccuracies will error on the side of a smaller
            # total width. At 8 decimal places the real world difference of this
            # reduction is about 1mm. 6 decimal places is about 10cm, so this
            # correction should change dx by less than 10cm which should not be
            # noticeable on any real display.
            if nx*dx > 360 and nx*(dx-0.000001) < 360:
                dx = 359.99999999/nx
                
            coverage.setSpacingUnit(DEFAULT_SPACING_UNIT2)
            coverage.setNx(Integer(nx))
            coverage.setNy(Integer(ny))
            coverage.setLa1(la1)
            coverage.setLo1(lo1)
            coverage.setDx(dx)
            coverage.setDy(dy)
            corner = GribSpatialCache.determineFirstGridPointCorner(gribDict['scanMode'])
            coverage.setFirstGridPointCorner(corner)
            gribDict['coverage'] = self._getGrid(coverage)

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
            nx = int(gdsTemplate[7])
            ny = int(gdsTemplate[8])
            la1 = self._correctLat(self._divideBy10e6(gdsTemplate[9]))
            lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[10]))
            latin = self._correctLat(self._divideBy10e6(gdsTemplate[12]))
            la2 = self._correctLat(self._divideBy10e6(gdsTemplate[13]))
            lo2 = self._correctLon(self._divideBy10e6(gdsTemplate[14]))
            dx = self._divideBy10e6(gdsTemplate[17])
            dy = self._divideBy10e6(gdsTemplate[18])
            gribDict['scanMode'] = int(gdsTemplate[15])
            # gribDict['resCompFlags'] = gdsTemplate[11]

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
            corner = GribSpatialCache.determineFirstGridPointCorner(gribDict['scanMode'])
            coverage.setFirstGridPointCorner(corner)

            gribDict['coverage'] = self._getGrid(coverage)

        # Polar Stereographic projection
        elif gdsTemplateNumber == 20:
            coverage = PolarStereoGridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            nx = int(gdsTemplate[7])
            ny = int(gdsTemplate[8])
            la1 = self._correctLat(self._divideBy10e6(gdsTemplate[9]))
            lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[10]))
            lov = self._correctLon(self._divideBy10e6(gdsTemplate[13]))
            lad = self._correctLat(self._divideBy10e6(gdsTemplate[12]))
            dx = self._divideBy10e6(gdsTemplate[14])
            dy = self._divideBy10e6(gdsTemplate[15])
            gribDict['scanMode'] = int(gdsTemplate[17])
            # gribDict['resCompFlags'] = gdsTemplate[11]

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
            corner = GribSpatialCache.determineFirstGridPointCorner(gribDict['scanMode'])
            coverage.setFirstGridPointCorner(corner)

            gribDict['coverage'] = self._getGrid(coverage)

        # Lambert Conformal projection
        elif gdsTemplateNumber == 30:

            coverage = LambertConformalGridCoverage()
            majorAxis, minorAxis = self._getEarthShape(gdsTemplate)
            nx = int(gdsTemplate[7])
            ny = int(gdsTemplate[8])
            la1 = self._correctLat(self._divideBy10e6(gdsTemplate[9]))
            lo1 = self._correctLon(self._divideBy10e6(gdsTemplate[10]))
            lov = self._correctLon(self._divideBy10e6(gdsTemplate[13]))
            dx = self._divideBy10e6(gdsTemplate[14])
            dy = self._divideBy10e6(gdsTemplate[15])
            latin1 = self._correctLat(self._divideBy10e6(gdsTemplate[18]))
            latin2 = self._correctLat(self._divideBy10e6(gdsTemplate[19]))
            gribDict['scanMode'] = int(gdsTemplate[17])
            # gribDict['resCompFlags'] = gdsTemplate[11]

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
            corner = GribSpatialCache.determineFirstGridPointCorner(gribDict['scanMode'])
            coverage.setFirstGridPointCorner(corner)

            gribDict['coverage'] = self._getGrid(coverage)

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

        # If not found, create a new GridCoverage and store in the cache
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
        return float(number / 1000)

    ##
    # Divides a number by 1000000
    #
    # @param number: A number to be divided by 1000000
    # @return: The provided number divided by 1000000
    # @rtype: float
    ##
    def _divideBy10e6(self, number):
        return float(number / 1000000)
    ##
    # Convert a scaledValue and scaleFactor to the unscaled value
    #
    # @param scaledValue: The scaled value
    # @param scaleFactor: The scale factor
    # @return: The unscaled value
    # @rtype: float
    ##
    def _convertScaledValue(self, scaledValue, scaleFactor):
        return float(scaledValue) / 10 ** float(scaleFactor)

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
            lon = (180 - lon % 180) * -1
        elif lon < -180:
            lon = (180 - (-lon % 180))

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
        elif lat < -90:
            lat = (90 - (-lat % 90)) * -1
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
                self.log.info("Invalid earth shape majorAxis,minorAxis = " + str(majorAxis) + "," + str(minorAxis) + " defaulting to 6367470.0,6367470.0")
                minorAxis = majorAxis = 6367470.0

        # Earth assumed oblate spheroid with size as determined by IAU in 1965
        # (major axis = 6,378,160.0 m, minor axis = 6,356,775.0 m, f = 1/297.0)
        elif number == 2:
            minorAxis = 6356775.0
            majorAxis = 6378160.0

        # Earth assumed oblate spheroid with major and minor axes specified (in km) by data producer
        elif number == 3:
            minorAxis = self._convertScaledValue(gdsTemplate[4], gdsTemplate[3]) * 1000
            if minorAxis < 6000000.0:
                self.log.info("Invalid earth shape minorAxis = " + str(minorAxis) + " defaulting to " + MINOR_AXIS_DEFAULT)
                minorAxis = MINOR_AXIS_DEFAULT

            majorAxis = self._convertScaledValue(gdsTemplate[6], gdsTemplate[5]) * 1000
            if majorAxis < 6000000.0:
                self.log.info("Invalid earth shape majorAxis = " + str(majorAxis) + " defaulting to " + MAJOR_AXIS_DEFAULT)
                majorAxis = MAJOR_AXIS_DEFAULT

        # Earth assumed oblate spheroid as defined in IAG-GRS80 model
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
                self.log.info("Invalid earth shape minorAxis = " + str(minorAxis) + " defaulting to " + MINOR_AXIS_DEFAULT)
                minorAxis = MINOR_AXIS_DEFAULT

            majorAxis = self._convertScaledValue(gdsTemplate[6], gdsTemplate[5])
            if majorAxis < 6000000.0:
                self.log.info("Invalid earth shape majorAxis = " + str(majorAxis) + " defaulting to " + MAJOR_AXIS_DEFAULT)
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
    # Converts some numeric values from a grib section to a java Calendar.
    # The date should consist of 6 int values ordered as follows:
    # year, month, day, hour, minute, second.
    #
    # @param section: numpy int array containing date
    # @param start: the start index in section to read the date.
    # @return: java Calendar object
    # @rtype: Calendar
    ##
    def _convertToCalendar(self, section, start):
        year = int(section[start])
        month = int(section[start + 1] - 1)
        day = int(section[start + 2])
        hour = int(section[start + 3])
        minute = int(section[start + 4])
        second = int(section[start + 5])
        return GregorianCalendar(year, month, day, hour, minute, second)

    ##
    # Converts a value in the specified unit (according to table 4.4) to seconds
    #
    # @param value: The value to convert to seconds
    # @param fromUnit: The value from Table 4.4 to convert from
    # @return: The number of seconds of the provided value
    # @rtype: int
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

        return int(retVal)

    def _getGridModel(self, gribDict, grid):
        center = gribDict['center']
        subcenter = gribDict['subcenter']

        process = gribDict['genprocess']
        processType = gribDict['processType']
        gridModel = GribModelLookup.getInstance().getModel(center, subcenter, grid, process, processType, self.fileName)
        return gridModel

    def _createModelName(self, gribDict, grid):
        center = gribDict['center']
        subcenter = gribDict['subcenter']

        process = gribDict['genprocess']
        processType = gribDict['processType']
        return GribModelLookup.getInstance().getModelName(center, subcenter, grid, process, processType, self.fileName)

    def _checkForecastFlag(self, gribDict, grid, dataTime):
        gridModel = self._getGridModel(gribDict, grid)
        if gridModel is None:
            return
        else:
            if gridModel.getAnalysisOnly():
                dataTime.getUtilityFlags().remove(FLAG.FCST_USED)

    def _isStatisticalModel(self, gribDict):
        # For now, only allow P-ETSS to generate mean/min/max parameters
        grid = gribDict['coverage']
        modelName = self._createModelName(gribDict, grid)

        if modelName is None:
            return False

        return modelName.find("P-ETSS") == 0
