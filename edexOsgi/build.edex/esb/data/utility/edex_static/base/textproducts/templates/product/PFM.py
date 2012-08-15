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
#-------------------------------------------------------------------------
# File Name: PFM.py
# Description: This product creates an AFM or PFM table containing 
# times across the top and weather element as rows.  Depending on 
# the Local file, the user can generate an AFM or PFM. User can control 
# the point at which entries are displayed as ranges vs. single values,
# and can optionally include Heat Index and Wind Chill.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# PFM, AFM_<site>_<MultiPil>_Definition, AFM_<site>_Overrides
#      PFM_<site>_<MultiPil>_Definition, PFM_<site>_Overrides
#-------------------------------------------------------------------------
# User Configurable Variables:
# Definition Section:
# displayName         If not None, defines how product appears in GFE GUI
#
# fullStationID        Full station identifier, 4 letter, such as KSLC
#
# wfoCityState         Identifier for the location of your WFO, such as
#                      "Cleveland, Ohio"
#
# wmoID                WMO ID for product header, such as "FOUS51"
#
# pil                  Product pil, such as "AFMBOS"
#
# productType          Product Type, must be "AFM" or "PFM"
#
#  editAreaSuffix      default None. For AFM only. Allows for generating the body of the product for
#                      an edit area that is a subset (e.g. population areas) of the
#                      edit areas specified in the defaultEditAreas.  So given the edit area,
#                      "COZ035" and the editAreaSuffix is "_pt", then the edit area that
#                      will be sampled and reported for the body of the product will be
#                      "COZ035_pt".  If no such edit area exists, the system will simply
#                      use the original edit area.
#                      Note that Hazards will always be generated for the entire edit area.
#  mapNameForCombinations Name of the map background that is used for 
#                         creating/editing the combinations file.  This must 
#                         be defined or the GFE zone combiner
#  database               Source database for product. Can be "Official", 
#                         "Fcst" or "ISC"
#  outputFile             Defines the output location of the finished product
#                         when saved from the Formatter Launcher.
#  debug                  If on, debug_print statements will appear.
#  textdbPil              Defines the awips product identifier 
#                         (e.g., DENCCFDEN) that is used to store the product 
#                         in the AWIPS text database. 
#                         This value is also used for the default GUI entry for 
#                         storage.
#  awipsWANPil            Defines the awips product identifier 
#                         (e.g., KBOUCCFDEN) that is used to transmit the 
#                         product to the AWIPS WAN.
#                         This value is also used for the default GUI 
#                         entry for storage.
# defaultEditAreas    defines edit area names, ugc code and area descriptor
#                         (editAreaName, "ugc1\nareaDescription1")
# areaDictionary      area dictionary format defining geographical codes
#                     (used if combinations file is specified in
#                     default edit areas)
#
# tempRangeThreshold   If the data range of temperature exceeds this threshold,
#                      then a range of temperatures will be output rather
#                      than a single value.
#
# qpfRangeThreshold    If the data range of QPF exceeds this threshold,
#                      then a range of qpf values will be output rather
#                      than a single value.
#
# snowRangeThreshold   If the data range of snow exceeds this threshold,
#                      then a range of snow values will be output rather
#                      than a single value.
#
# includeSnowAmt       Set to 1 to include SnowAmt, 0 to exclude.
#
# includeHeatIndex     Set to 1 to include HeatIndex, 0 to exclude. Heat
#                      Index is only included when Heat Index grids are
#                      present, this option is 1, and the Heat Index
#                      thresholds are exceeded.
#
# heatIndexDifference  Defines the threshold HeatIndex-T before reporting
#                      heat index. Heat Index  must be this many degrees
#                      above the temperature before a heat index is reported.
#
# heatIndexLimit       Defines the absolute threshold below which no
#                      heat index temperatures will be reported.
#
# includeWindChill     Set to 1 to include WindChill, 0 to exclude. Wind
#                      Chill is only included when the Wind Chill grids
#                      are present, this option is 1, and the Wind Chill
#                      thresholds are exceeded.
#
# windChillDifference  Defines the threshold T-WindChill before reporting
#                      wind chill. WindChill must be this many degrees
#                      below the temperature before a wind chill is reported.
#
# windChillLimit       Defines the absolute threshold above which no
#                      wind chill temperatures will be reported.
#
# separateByTimeZone   Normally set to "effectiveTZ" to ensure that zones
#                      combined in the product all have the same effective
#                      timezone, e.g., EST5, EST5EDT are considered the same
#                      in the winter since the effective tz is EST5.  
#                      Can also be None to not separate out
#                      zones, or set to "actualTZ" to separate by actual
#                      TZ environment variable, e.g., EST5, EST5EDT are
#                      considered different in the winter.
#
#-------------------------------------------------------------------------
# Weather Elements Needed: 
# To 66 hours: WindGust, QPF,  SnowAmt, HeatIndex (opt.), WindChill (opt.).  
# To 7 days: MaxT, MinT, T, Td, Wind, Sky, PoP, and Wx.
#-------------------------------------------------------------------------
# Edit Areas Needed: Typically area or point edit areas are required,
# depending upon whether an AFM or PFM is being created.  The
# required format of the edit area naming string for the AFM is:
#       (editAreaName, "areaName\nareaDescriptor")
# The required format for the PFM is:
#       (editAreaName, "ugc code\npoint description\nyy.yyN xxx.xxW")
# where the editAreaName is the name of the sampling edit area,
# areaName is the UGC codes for the area, such as "COZ043"
# areaDescriptor is the descriptive name of that area.
#-------------------------------------------------------------------------
# Associated Utilities Files e.g. Combinations file: None
#-------------------------------------------------------------------------
# Component Products: None
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Development tasks that are identified and in progress:
#-------------------------------------------------------------------------
# Additional Information:
#  Sampling is complicated in this product since the column headers mean
#  different things to different weather elements.  In some cases, the
#  column header denotes the ending time, in other cases, the starting
#  time.  There are 3hr, 6hr, and 12hr elements in the top portion of
#  the product, and 6hr and 12hr elements in the extended (bottom) portion
#  of the product.
#
#  If HeatIndex is enabled, the HeatIndex row (and values) will only appear
#  if the threshold values are exceeded from the HeatIndex grid.  If
#  HeatIndex appears, then Max Heat will appear in the output.  
#
#  If WindChill is enabled, the WindChill row (and values) will only appear
#  if the threshold values are exceeded from the WindChill grid. If
#  Wind Chill appears, then MIN CHILL will appear in the output.
#
#  WindGust will only appear if WindGust grids are present and the wind
#  gust values exceed the wind values by at least 10mph.
#
#  Fields are blanked in the output if those times are before the
#  product issuance time.
#
#  Note that no headers will appear for Day 3 on the Afternoon issuance due to
#  inadequate space.
#
#-------------------------------------------------------------------------
# Example Output:
#
##    FOUS51 KRLX 311833
##    AFMRLX

##    AREA FORECAST MATRICES
##    NATIONAL WEATHER SERVICE CHARLESTON WV
##    1233 PM MDT WED JUL 31 2002

##    Area 1-010333
##    1233 PM MDT WED JUL 31 2002

##    DATE          WED 07/31/02            THU 08/01/02            FRI 08/02/02
##    UTC 3HRLY  09 12 15 18 21 00 03 06 09 12 15 18 21 00 03 06 09 12 15 18 21 00
##    MDT 3HRLY  03 06 09 12 15 18 21 00 03 06 09 12 15 18 21 00 03 06 09 12 15 18

##    MAX/MIN             92 94 96    62 64 66    77 79 81    61 63 65    81 83 85
##    TEMP       81 75 86 94 74 76 73 69 66 64 67 72 76 79 76 70 66 63 67 73 79 83
##    DEWPT      34 32 29 26 52 52 52 51 51 50 49 49 49 49 50 51 52 53 52 50 49 48
##    RH         18 20 12  8 46 43 47 52 58 60 52 44 38 34 40 50 60 69 58 44 34 29
##    WIND DIR   NW NW NW  N  S SE SE  S  S  S  S  S  S SE  S  S  S  S  S  S  S  S
##    WIND SPD    3  6  5  5  8 10 11 10 11 11 11 10 12 12 13 14 14 13 14 14 13 13
##    WIND GUST
##    CLOUDS     CL CL CL CL MM OV OV OV SC SC SC FW FW FW SC SC BK OV BK SC FW FW
##    POP 12HR                   0           0          28           0          16
##    QPF 12HR                0.04        0.00        0.03        0.00        0.00
##    SNOW 12HR              00-00       00-00       00-00       00-00       00-00
##    RAIN SHWRS             SC SC
##    RAIN                                     WP WP WP WP

##    DATE            SAT 08/03/02  SUN 08/04/02  MON 08/05/02  TUE 08/06/02
##    UTC 6HRLY  06   12 18 00 06   12 18 00 06   12 18 00 06   12 18 00 06
##    MDT 6HRLY  00   06 12 18 00   06 12 18 00   06 12 18 00   06 12 18 00

##    MAX/MIN         65    94      64    79      63    83      65    87
##    TEMP       74   65 72 76 69   64 72 79 70   63 73 83 74   65 72 87
##    DEWPT      49   50 48 52 51   50 49 49 51   53 50 48 49   50 48 45
##    PWIND DIR        S     S       S     S       S     S       S     S
##    WIND CHAR       BZ    BZ      GN    GN      BZ    BZ      BZ    BZ
##    AVG CLOUDS      FW FW SC SC   BK BK FW FW   SC SC SC SC   FW SC SC
##    POP 12HR         0     1       0    28       0    16       0     1
##    RAIN SHWRS            SC
##    RAIN                                WP

##    $$

###########################################################################

import TextRules
import SampleAnalysis
import LogStream
import string, time, types, os
import TimeRange, AbsTime

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    VariableList = [
                    (("Product Issuance", "productIssuance"), "Morning", "radio",
                     ["Morning","Afternoon"]),
    #                (("Tropical Storm Winds", "tropicalStorm"), "NO", "radio", ["NO", "YES"]),
                   ]
    Definition =  {
        "type": "smart",
        "displayName": "None",
        # Source database for product. Can be "Official", "Fcst" or "ISC"
        "database": "Official",
        # Defines output location of finished product.
        "outputFile": "{prddir}/TEXT/PFM_<MultiPil>.txt",
        "debug": 0,
        
        "defaultEditAreas" : "Combinations_PFM_<site>_<MultiPil>",
        "editAreaSuffix": None,
       # Name of map background for creating Combinations
        "mapNameForCombinations": "Zones_<site>",
        
        "runTimeEditAreas" : "no", #  if yes, ask user at run time

        "textdbPil": "<textdbPil>",       # Product ID for storing to AWIPS text database.
        "awipsWANPil": "<awipsWANPil>",   # Product ID for transmitting to AWIPS WAN.

        # Product-specific variables:
        "tempRangeThreshold": 5,     # determines range vs. single value output
        "qpfRangeThreshold": 0.05,  # determines range vs. single value output
        "snowRangeThreshold": 3,  # determines range vs. single value output

        # Options for product
        "includeSnowAmt": 1,     # set to 1 to include snow amount in output
        "includeHeatIndex": 1,   # set to 1 to include heat index in output
        "includeWindChill": 1,   # set to 1 to include wind chill in output
        "windChillDifference": 5, # T-WindChill difference before reporting
        "windChillLimit": 40,    # don't report wind chills above this value
        "heatIndexDifference": 0, # indicates HI-T difference before reporting
        "heatIndexLimit": 80,     # don't report heat index below this value

        # Area Dictionary - Descriptive info about zones (AFM only)
        "areaDictionary": "AreaDictionary",
 
        # Identifiers for product
        "fullStationID": "<fullStationID>",  # full station identifier (4letter)
        "wfoCityState": "<wfoCityState>",  # city,state of wfo for header
        "wmoID": "<wmoID>",       # WMO ID  code
        "pil": "<pil>",         # product pil
        "productType": "PFM",    # Denotes product type, "AFM", or "PFM"

        # purge time
        "fixedExpire": 1, #ensure VTEC actions don't affect segment time

        "separateByTimeZone": "effectiveTZ", #separate segments based on tz

        }

    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def generateForecast(self, argDict):
        # Generate formatted product for a list of edit areas

        # get current time
        self._currentTime = argDict['creationTime']

        # Get variables from varDict and Definition
        self._getVariables(argDict)

        # Get the areaList -- derived fromEditAreas and
        # may be solicited at run-time from user if desired
        self._areaList = self.getAreaList(argDict)
        if len(self._areaList) == 0:
            return "WARNING -- No Edit Areas Specified to Generate Product."

        # Determine time ranges, for each possible time zone
        self._areaTZ = self.getAreaTZ(argDict, self._areaList)  #all TimeZones
        tzDictTR = {}
        for key in self._areaTZ.keys():
            tz = self._areaTZ[key]
            if not tzDictTR.has_key(tz):
                 tzDictTR[tz] =  self._determineTimeRanges(argDict, tz)
        self._determineZuluTimeRanges(argDict, tzDictTR)


        # Sample the data. 
        sampleInfo = []
        # determine all of the areas using the same time zone
        for timeZone in tzDictTR.keys():
            areasInTimeZone = []
            for area in self._areaList:
                areaLabel = area[1]   #(editArea, areaLabel)
                if self._areaTZ[areaLabel] == timeZone:
                    areasInTimeZone.append(area)
            sampleInfo += self._sampleData(argDict, tzDictTR[timeZone], 
              areasInTimeZone)
        self._sampler = self.getSampler(argDict, sampleInfo)

        # Initialize the output string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        # Generate the product for each edit area in the list
        fraction = 0
        fractionOne = 1.0/float(len(self._areaList))
        percent = 50.0
        for editArea, areaLabel in self._areaList:
            self.progressMessage(fraction, percent, "Making Product for " + areaLabel)
            tz = self._areaTZ[areaLabel]
            fcst = self._preProcessArea(fcst, editArea, areaLabel, argDict,
              tzDictTR[tz]['timeLabel'])
            fcst  = self._makeProduct(fcst, editArea, areaLabel, argDict,
              tzDictTR[tz], tz)
            fcst = self._postProcessArea(fcst, editArea, areaLabel, argDict)
            fraction = fractionOne

        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    def _getVariables(self, argDict):
        # Make argDict accessible
        self.__argDict = argDict

        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        # Set up product-specific variables
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"
        try:
            if self._tropicalStorm == "YES":
                self._tropicalStorm = 1
            else:
                self._tropicalStorm = 0
        except:
            self._tropicalStorm = 0
            

        self._lineLength = 66  #only will apply to UGC header

        # Basic widths for product
        self._rowLabelWidth = 13    #width of row label
        self._top3hrWidth = 3       #top part of product, 3hrly width
        self._top6hrWidth = 6       #top part of product, 6hrly width
        self._top12hrWidth = 12     # top part of product, 12hrly width
        self._bottom6hrWidth = 3    # bottom part, 6hr width
        self._bottom12hrWidth = 6   # bottom part, 12hr width
        self._extraDayWidth = 2     # extra spaces between days, bottom part


    def getAreaTZ(self, argDict, areaList):
        #gets the list of areas and their time zones for the product
        #the areas are defined by the "areaList" and aren't the actual
        #zones UGCs, e.g., "Combo0" and not "FLZ050". Only 1 time zone
        #is permitted in the area.  Returns the time zone environmental 
        #variable, e.g., EST5EDT, with the effective TZ, e.g., EST,  in a 
        #dictionary keyed by the areaLabel.
        import ModuleAccessor
        accessor = ModuleAccessor.ModuleAccessor()
        areaDict = accessor.variable(self._areaDictionary, "AreaDictionary")
        tzDir = {}
        localTZ = os.environ['TZ']   #current WFO time zone
        localTZid = time.strftime("%Z", time.localtime(argDict['creationTime']))
        for editArea, areaLabel in areaList:
            areas = self.getCurrentAreaNames(argDict, areaLabel) #get areas
            tzs = []
            for area in areas:
                #extract 1st string out for PFM.  The AFM is in correct format.
                if self._productType == "PFM":
                    areaStrings = string.split(area, '\n')
                    area = areaStrings[0]  #1st line is the UGC code
                try:
                    zoneTZ = areaDict[area]['ugcTimeZone']
                    prevTZ = os.environ['TZ']
                    os.environ['TZ'] = zoneTZ
                    time.tzset()
                    tzid = time.strftime("%Z", 
                      time.localtime(argDict['creationTime']))
                    os.environ['TZ'] = prevTZ
                    time.tzset()
                except:
                    zoneTZ = localTZ
                    tzid = localTZid
                    LogStream.logProblem("WARNING: Entry " + area +  
                      " missing from AreaDictionary. Using default time zone.")

                if (zoneTZ, tzid) not in tzs:
                    tzs.append((zoneTZ, tzid))
            #should only be 1 effective time zone at this point. 
            if len(tzs) == 0:
                tzDir[areaLabel] = localTZ  #force localTZ
            elif len(tzs) == 1:
                tzDir[areaLabel] = tzs[0][0]   #use the 1 time zone
            else:
                tzid = tzs[0][1]  #1st one, get the effective id
                for x in xrange(1, len(tzs)):
                    if tzs[x][1] != tzid:
                        LogStream.logProblem(\
                          "WARNING: Multiple Effective Time Zones in segment." + 
                          str(tzs) + " for " + str(areas) + " -- using first one.")
                tzDir[areaLabel] = tzs[0][0]   #use the 1 time zone

        return tzDir

    def _determineZuluTimeRanges(self, argDict, tzDictTR):
        # Determine time ranges that deal with Zulu-time, i.e., not
        # specific times.  The tzDictTR is the group of dictionaries
        # returned from determineTimeRanges().

        ##################################
        # Setup Time Labels
        ##################################
        # Sets up the product's time labels

        self._ddhhmmTime = time.strftime("%d%H%M",
                                         time.gmtime(self._currentTime))

        # Sets up the expiration time
        self._expireTime, self._ddhhmmTimeExpire = \
          self.getExpireTimeFromLToffset(self._currentTime,
          self.expireOffset(), "")

        # time label for the MND header
        self._mndTimeLabel = self.getCurrentTime(
           argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)

        # the overall time range
        earliest = None
        latest = None
        for key in tzDictTR.keys():
            d = tzDictTR[key]
            if earliest is None:
                earliest = d['top_3hr'][0][0].startTime()
                latest = d['bottom_12hr'][-1][0].endTime()
            else:
                earliest =  min(d['top_3hr'][0][0].startTime(),earliest)
                latest = max(d['bottom_12hr'][-1][0].endTime(),latest)
            
        self._timeRange = TimeRange.TimeRange(earliest, latest)
        argDict["productTimeRange"] = self._timeRange

    def _shortenPeriods(self, periods):
        #Shorten the period containing the current time so that we don't
        #sample the entire period.  Periods is from getPeriods(), returns
        #modified version.
        thisHour = int(self._currentTime / 3600) * 3600  #truncated to hh:00
        for x in xrange(len(periods)):
            tr, label = periods[x]
            startT = tr.startTime().unixTime()
            endT = tr.endTime().unixTime()
            if startT < thisHour and thisHour < endT:
                tr = TimeRange.TimeRange(AbsTime.AbsTime(thisHour), AbsTime.AbsTime(endT))
                periods[x] = (tr, label)
            elif thisHour < startT:
                break
        return periods
     
    def _creTR(self, baseTime, offset):
        # creates and returns a python TimeRange, based on the AbsTime baseTime,
        # and the offset (in hours).  The length of the TimeRange is one hour.
        # This is a substitute for the createTimeRange() in determineTimeRanges
        startTime = baseTime + 3600*offset
        return TimeRange.TimeRange(startTime, startTime + 3600)

    def _determineTimeRanges(self, argDict, timeZone):
        # Determine time ranges for product - fairly complicated since
        # multiple samplings and two sets of tables (short-term, long-term)
        # Sets up:
        # top_3hr - top row of AFM, 3 hourly LT periods
        # top_3hr_snap - top row of AFM, 3 hourly LT periods/labels
        # top_6hr - top row of AFM, 6 hourly LT periods
        # top_6hrShort - top row of AFM, 6 hourly LT periods, start now
        # top_12hr - top row of AFM, 12 hourly sample periods
        # top_12hrShort - top row of AFM, 12 hourly sample periods, start now
        # bottom_6hr - bottom row of AFM, 6 hourly LT periods
        # bottom_6hr_snap - bottom row of AFM, 6 hourly, snapshots, labels
        # bottom_12hr - bottom row of AFM, 12 hourly sample periods
        #
        # Returns dictionary for the particular timezone containing the
        # above keys and values.
        tzDict = {}

        # change the time zone
        prevTZ = os.environ['TZ']
        os.environ['TZ'] = timeZone
        time.tzset()        

        # determine the optimal time for the zulu-based product columns,
        # that most closely mirror 3AM LT and 3PM LT.  The final baseTime is
        # the column label for the first column.
        if self._productIssuance == "Afternoon":
            #check for update for Afternoon issuance after midnight
            if time.localtime(self._currentTime)[3] >= 4:  
                tr = self.createTimeRange(15, 15+1)  #3PM target (not update)
            else:
                tr = self.createTimeRange(15-24, 15-24+1) #3PM target yesterday
        else:
            tr = self.createTimeRange(3, 3+1)    #3AM target

        # determine offset between potential times, want 3z,9z,15,21z
        baseTime = (((tr.startTime().unixTime() - 3600*3) 
          / 3600 / 6 ) * 3600*6) + 3600*3
        offsetFromBaseTime = ((((tr.startTime().unixTime() - 3600*3) 
          / 3600.0 / 6.0 ) * 3600*6) + 3600*3 - baseTime) / 3600
        offsetFromBaseTime = int(offsetFromBaseTime)
        if offsetFromBaseTime > 3:    #over halfway to next basetime
            baseTime = baseTime + 3600*6    #move to next 6 hour increment
        baseTime = AbsTime.AbsTime(baseTime)  #convert to AbsTime
        
        # Set up the beginning Time Range - note they are different for the
        # 3/6hrly, and the 12hrly sampling. Note that the 6hr is offset by
        # 1 hour to allow the minChill/maxHeat to include the hour indicated
        # by the column. Comments are for offsets for sampling periods from
        # first column's labeled time.  Length of created time range is 1 hour
        topTimeRange3hr = self._creTR(baseTime, -3)  #-3h to 0h
        topTimeRange6hr = self._creTR(baseTime, -2)  #-2h to 4h
        topTimeRange3hrSnap = self._creTR(baseTime, 0)  #0h to 1h
        topTimeRange12hr = self._creTR(baseTime, 3)  #3h to 15h
        bottomTimeRange6hr = self._creTR(baseTime, -3+66)  #63h to 66h
        bottomTimeRange6hrSnap = self._creTR(baseTime, -3+72) #69h to 70h
        bottomTimeRange12hr = self._creTR(baseTime, 3+60)  #63h to 75h

        ##################################
        # Set up 3hr, 6hr, and 12hr elements in top portion
        ##################################
        timePeriod = 3
        timeSpan = 3
        numPeriods = 22
        tzDict['top_3hr'] = self.getPeriods(topTimeRange3hr, timePeriod,
                                       timeSpan, numPeriods)
        timeSpan = 1
        tzDict['top_3hr_snap'] = self.getPeriods(topTimeRange3hrSnap,
                                        timePeriod, timeSpan, numPeriods,
                                        self._hour24localLabel)
        timePeriod = 6
        timeSpan = 6
        numPeriods = 11
        tzDict['top_6hr'] = self.getPeriods(topTimeRange6hr, timePeriod, 
          timeSpan, numPeriods)
        periods = self.getPeriods(topTimeRange6hr, timePeriod, 
          timeSpan, numPeriods) 
        tzDict['top_6hrShort'] = self._shortenPeriods(periods)


        timePeriod = 12
        timeSpan = 12
        numPeriods = 5
        tzDict['top_12hr'] = self.getPeriods(topTimeRange12hr,
                                        timePeriod, timeSpan, numPeriods)
        periods = self.getPeriods(topTimeRange12hr,
                                        timePeriod, timeSpan, numPeriods)
        tzDict['top_12hrShort'] = self._shortenPeriods(periods)

        ##################################
        # Set up 6hr and 12hr elements in bottom portion
        ##################################
        timePeriod = 6
        timeSpan = 6
        if self._productIssuance == "Morning":
            numPeriods = 16
        else:
            numPeriods = 18
        tzDict['bottom_6hr'] = self.getPeriods(bottomTimeRange6hr,
                                       timePeriod, timeSpan, numPeriods)
        timeSpan = 1
        tzDict['bottom_6hr_snap'] = self.getPeriods(
                                       bottomTimeRange6hrSnap,
                                        timePeriod, timeSpan, numPeriods,
                                        self._hour24localLabel)

        timePeriod = 12
        timeSpan = 12
        if self._productIssuance == "Morning":
            numPeriods = 8
        else:
            numPeriods = 9
        tzDict['bottom_12hr'] = self.getPeriods(bottomTimeRange12hr,
                                        timePeriod, timeSpan, numPeriods,
                                        self._hour24localLabel)


        # timeLabel is the spelled-out version of the current time
        # for each UGC header
        tzDict['timeLabel'] = self.getCurrentTime(
           argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)

        # restore the time zone
        os.environ['TZ'] = prevTZ
        time.tzset()

        return tzDict

    def _sampleData(self, argDict, tp, areas):
        # Sample the data.  (tp) Time Periods is a dictionary containing 
        # all of the time periods and categories to sample for a single time
        # zone.  areas are the areas that have this time zone.
        # Different sampler lists are required due to different time periods 
        # and different elements.  Sets up the following to sample:
        # 3hr_top = 3hrly, top of product, 3 hr durations
        # 3hr_snap_top = 3hrly, top of product, 1 hr duration
        # 6hr_top = 6hrly, top of product, 6 hr durations
        # 12hr_top = 12hrly, top of product
        # 6hr_bottom = 6 hrly, bottom of product, 6 hr durations
        # 6hr_snap_bottom = 6 hrly, bottom of product, 1 hr durations
        # 12hr_bottom = 12hrly, bottom of product

        # the analysis method is called "self._analysisList_" plus the 
        # name of the sample period, which is the key.
        # Each entry: (analysisList, periods, areaList)


        sampleInfo = [
          (self._analysisList_top_3hr(),         tp['top_3hr'], areas),
          (self._analysisList_top_3hr_snap(),    tp['top_3hr_snap'], areas),
          (self._analysisList_top_6hr(),         tp['top_6hr'], areas),
          (self._analysisList_top_6hrShort(),    tp['top_6hrShort'], areas),
          (self._analysisList_top_12hr(),        tp['top_12hr'], areas),
          (self._analysisList_top_12hrShort(),   tp['top_12hrShort'], areas),
          (self._analysisList_bottom_6hr(),      tp['bottom_6hr'], areas),
          (self._analysisList_bottom_6hr_snap(), tp['bottom_6hr_snap'], areas), 
          (self._analysisList_bottom_12hr(),     tp['bottom_12hr'], areas), 
           ]
          
        return sampleInfo

    def _preProcessProduct(self, fcst, argDict):
        # Add product heading to fcst string
        fcst = fcst + self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n"
        if self._productType == "AFM":
            productDescription = "AREA FORECAST MATRICES"
        else:
            productDescription = "POINT FORECAST MATRICES"

        issuedByString = self.getIssuedByString()
        
        productName = self.checkTestMode(argDict, 
          productDescription) 
        
        fcst = fcst + self._pil + "\n\n"
        fcst = fcst + productName + "\n"
        fcst = fcst + "NATIONAL WEATHER SERVICE "
        fcst = fcst + self._wfoCityState +"\n"
        fcst = fcst + issuedByString 
        fcst = fcst + self._mndTimeLabel + "\n\n"

        return fcst

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict, timeLabel):
        # extract out the ugc codes and the area descriptors 
        # AFM: combinations filename, or (ugc, descriptionlines) 
        # PFM: areaLabel length 4 (ugc, description, latlon, elevation)
        areaStrings = string.split(areaLabel, '\n')

        #AFM setup with combinations file
        if self._productType == "AFM" and len(areaStrings) == 1:
            areaHeader = self.makeAreaHeader(
              argDict, areaLabel, AbsTime.AbsTime(self._currentTime), 
              AbsTime.AbsTime(self._expireTime), self._areaDictionary, 
              self._defaultEditAreas)
         
            fcst = fcst + areaHeader

 
        #No Combinations File or PFM product
        else:
            if self._productType == "PFM":
                if len(areaStrings) != 4 and len(areaStrings) != 3:
                    raise SyntaxError, """
PFM requires defaultEditArea format of (editAreaName, 
ugcLine\\narea description line\\nll.llN lll.llW\\nElev
editAreaName is the name of the edit area for sampling
ugcLine is the ugc code representing the area, and is used for timezone info
area description line describes the area
ll.llN lll.llW is the latitude and longitude for the area
Elev is a string representing the station elevation, e.g., 423.
Found description: """ + areaLabel

                latlon = areaStrings[2]
                if not self._latlonCheck(latlon):
                    raise SyntaxError, "PFM lat/lon format must be " +\
                      "exactly ll.llN lll.llW  found:" + latlon

            ugcCode = areaStrings[0]
            s = ugcCode + "-" + self._ddhhmmTimeExpire + "-\n"

            # descriptor lines, lat/lon lines, elevation
            if len(areaStrings) == 4:
                s += areaStrings[1] + "\n" + areaStrings[2] + " ELEV. " + \
                  areaStrings[3] + " FT" + "\n"
            #old OB8.2 format
            else:
                s += areaStrings[1] + "\n" + areaStrings[2] + " ELEV. " + \
                  "???? FT" + "\n"
                #log "old" format
                LogStream.logProblem("WARNING: Old defaultEditArea format " + """
PFM requires defaultEditArea format of (editAreaName, 
ugcLine\\narea description line\\nll.llN lll.llW\\nElev
editAreaName is the name of the edit area for sampling
ugcLine is the ugc code representing the area, and is used for timezone info
area description line describes the area
ll.llN lll.llW is the latitude and longitude for the area
Elev is a string representing the station elevation, e.g., 423.
Found description: """ + areaLabel)

            fcst = fcst + s + timeLabel + "\n\n"

        # setup to handle the hazards
        self._hazards = argDict['hazards']
        self._combinations = argDict['combinations']

        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict, timePeriods,
      timeZone):

        ###############################################################
        # TOP PART OF PRODUCT - valid current time out to around 60hr)
        ###############################################################

        # Day, Period Label (UTC), Period Label (LT)
        dateLabel, utcLabel, ltLabel = self._calcPeriodLabels(\
            timePeriods['top_3hr_snap'], self._top3hrWidth,
            self._rowLabelWidth, 3, timeZone)
        fcst = fcst + dateLabel + "\n" + ltLabel + "\n" + utcLabel + "\n\n"

        # Create statLists
        statList_3hr = self.getStatList(
            self._sampler, self._analysisList_top_3hr(),
            timePeriods['top_3hr'], editArea)
        statList_3hr_snap = self.getStatList(
            self._sampler, self._analysisList_top_3hr_snap(),
            timePeriods['top_3hr_snap'], editArea)
        statList_6hr = self.getStatList(
            self._sampler, self._analysisList_top_6hr(),
            timePeriods['top_6hr'], editArea)
        statList_6hrShort = self.getStatList(
            self._sampler, self._analysisList_top_6hrShort(),
            timePeriods['top_6hrShort'], editArea)
        statList_12hr = self.getStatList(self._sampler,
            self._analysisList_top_12hr(), timePeriods['top_12hr'], editArea)
        statList_12hrShort = self.getStatList(self._sampler,
            self._analysisList_top_12hrShort(), timePeriods['top_12hrShort'], 
            editArea)

        # Max/Min
        tpmaxmin = timePeriods['top_12hr'][0][0]
        if self.getPeriod(tpmaxmin, shiftToLocal=1) == self.DAYTIME():
            label = "MAX/MIN"
        else:
            label = "MIN/MAX"
        fcst=fcst+ self.makeRow(
            label, self._top12hrWidth, timePeriods['top_12hr'],
            statList_12hr, self._mxmnValue, [1],
            self._rowLabelWidth, 18)

        # Temp
        fcst=fcst+ self.makeRow(
            "TEMP", self._top3hrWidth, timePeriods['top_3hr_snap'],
            statList_3hr_snap, self._tempValue, ['T'], self._rowLabelWidth)

        # DewPt
        fcst=fcst+ self.makeRow(
            "DEWPT", self._top3hrWidth, timePeriods['top_3hr_snap'],
            statList_3hr_snap, self._tempValue, ['Td'], self._rowLabelWidth)

        #RH
        fcst=fcst+ self.makeRow(
            "RH", self._top3hrWidth, timePeriods['top_3hr_snap'],
            statList_3hr_snap, self._rhValue, [], self._rowLabelWidth)

        # Wind direction
        fcst=fcst+ self.makeRow(
            "WIND DIR", self._top3hrWidth, timePeriods['top_3hr_snap'],
            statList_3hr_snap, self._windValue, ["dir"],
            self._rowLabelWidth)

        # Windspd
        fcst=fcst+ self.makeRow(
            "WIND SPD", self._top3hrWidth, timePeriods['top_3hr_snap'],
            statList_3hr_snap, self._windValue, ["speed"],
            self._rowLabelWidth)

        # Wind Gust
        gustRow = self.makeRow(
            "WIND GUST", self._top3hrWidth, timePeriods['top_3hr_snap'],
            statList_3hr_snap, self._windGustValue, [],
            self._rowLabelWidth)
        if string.strip(gustRow) != "WIND GUST":
            fcst=fcst+gustRow

        # Sky
        fcst=fcst+ self.makeRow(
            "CLOUDS", self._top3hrWidth, timePeriods['top_3hr_snap'],
            statList_3hr_snap, self._skyValue, [], self._rowLabelWidth)

        # Pop
        fcst=fcst+ self.makeRow(
            "POP 12HR", self._top12hrWidth, timePeriods['top_12hr'],
            statList_12hrShort, self._popValue, [], self._rowLabelWidth,
            18)

        # QPF
        fcst=fcst+ self.makeRow(
            "QPF 12HR", self._top12hrWidth, timePeriods['top_12hr'],
            statList_12hrShort, self._qpfValue, [],
            self._rowLabelWidth, 18)

        #Max qpf
        #fcst=fcst+ self.makeRow(
            #"MAX QPF", self._top12hrWidth, timePeriods['top_12hr'],
            #statList_12hrShort, None, [], self._rowLabelWidth, 18)

        # SnowAmt
        if self._includeSnowAmt:
            snowBasetime = \
             ((timePeriods['top_3hr_snap'][0])[0]).startTime().unixTime()
            fcst=fcst+ self.makeRow(
                "SNOW 12HR", self._top12hrWidth, timePeriods['top_12hr'],
                statList_12hrShort, self._snowValue, [snowBasetime],
                self._rowLabelWidth, 18)

        # Weather
        fcst = self._createWxRows(fcst, timePeriods['top_3hr_snap'],
                                 self._sampler,
                                 self._analysisList_top_3hr_snap(), editArea,
                                 self._top3hrWidth)

        # OBVIS
        fcst = self._createObVisRows(fcst, timePeriods['top_3hr_snap'],
                                 self._sampler,
                                 self._analysisList_top_3hr_snap(), editArea,
                                 self._top3hrWidth)

        # Wind Chill
        if self._includeWindChill:
            row = self.makeRow(
               "WIND CHILL", self._top3hrWidth, timePeriods['top_3hr_snap'],
               statList_3hr_snap, self._windChillValue, [],
               self._rowLabelWidth)
            if string.strip(row) != "WIND CHILL":
                fcst=fcst+row
                fcst = fcst + self.makeRow(
                    "MIN CHILL", self._top6hrWidth, timePeriods['top_6hrShort'],
                    statList_6hrShort, self._minWindChillValue, [],
                    self._rowLabelWidth)


        # Heat Index
        if self._includeHeatIndex:
            row = self.makeRow(
               "HEAT INDEX", self._top3hrWidth, timePeriods['top_3hr_snap'],
               statList_3hr_snap, self._heatIndexValue, [],
               self._rowLabelWidth)
            if string.strip(row) != "HEAT INDEX":
                fcst=fcst+row
                fcst = fcst + self.makeRow(
                    "MAX HEAT", self._top6hrWidth, timePeriods['top_6hrShort'],
                    statList_6hrShort, self._maxHeatIndexValue, [],
                    self._rowLabelWidth)

        # Hazards (WWA)
        #need to make list of actual edit areas first
        combinations = argDict["combinations"]
        if combinations is not None:
            areaList = self.getCurrentAreaNames(argDict, areaLabel)
        else:
            areaList = [editArea.getId().getName()]
        fcst = self._createWWARows(fcst, timePeriods['top_3hr_snap'],
                                 areaList, self._top3hrWidth)

        fcst = fcst + "\n\n"

        ######################################
        # Extended Forecast
        ######################################
        # Determine the column spacing - 3 normally, 5 between days LT
        # Determine change in day by labels.
        colSpacing6hr = []
        colSpacing12hr = []

        # set the time zone
        prevTZ = os.environ['TZ']
        os.environ['TZ'] = timeZone
        time.tzset()

        # determine zulu alignment for bottom section, gather all of the
        # possible zulu hours (can't be more than 2 due to 12h intervals)
        zuluHours = []
        for x in xrange(0, 2):
            period, label = timePeriods['bottom_12hr'][x]
            zuluHours.append(period.endTime().hour)
        
        # add extra space for the first column in each day, except 1st one
        # We treat midnight at the end of the day so we subtract 1 to get
        # the right day for extra space insertion.  We use startTime, since
        # the values are valid at the label time.
        dayOfMonthProcessed6 = None
        firstTime = True
        for period, label in timePeriods['bottom_6hr_snap']:
            dayOfMonth = (period.startTime() -1 + self.determineShift()).day
            if dayOfMonth != dayOfMonthProcessed6 and not firstTime:
                colSpacing6hr.append(self._bottom6hrWidth +
                                     self._extraDayWidth)
            else:
                colSpacing6hr.append(self._bottom6hrWidth)
            dayOfMonthProcessed6 = dayOfMonth
            firstTime = False

        # now determine the bottom 12hr periods and their spacing
        runningTotal = 0
        for x in xrange(len(timePeriods['bottom_6hr_snap'])):
            period, label = timePeriods['bottom_6hr_snap'][x]
            space6 = colSpacing6hr[x]
            runningTotal += space6
            if period.startTime().hour in zuluHours:
                colSpacing12hr.append(runningTotal)
                runningTotal = 0

        # reset the time zone
        os.environ['TZ'] = prevTZ
        time.tzset()

        # Create statLists for bottom portion
        statList_6hr_snap = self.getStatList(
            self._sampler,
            self._analysisList_bottom_6hr_snap(),
            timePeriods['bottom_6hr_snap'], editArea)
        statList_6hr = self.getStatList(
            self._sampler,
            self._analysisList_bottom_6hr(),
            timePeriods['bottom_6hr'], editArea)
        statList_12hr = self.getStatList(self._sampler,
            self._analysisList_bottom_12hr(),
            timePeriods['bottom_12hr'], editArea)

        # Day, Period Label (UTC), Period Label (LT)
        dateLabel, utcLabel, ltLabel = self._calcPeriodLabels(\
            timePeriods['bottom_6hr_snap'], colSpacing6hr, 
            self._rowLabelWidth, 6, timeZone)
        fcst = fcst + dateLabel + "\n" + ltLabel + "\n" + utcLabel + "\n\n"

        # Max/MinT
        tpmaxmin = timePeriods['bottom_12hr'][0][0]
        if self.getPeriod(tpmaxmin, shiftToLocal=1) == self.DAYTIME():
            label = "MAX/MIN"
        else:
            label = "MIN/MAX"
        fcst=fcst+ self.makeRow(
            label, colSpacing12hr, timePeriods['bottom_12hr'],
            statList_12hr, self._mxmnValue, [0],
            self._rowLabelWidth)

        # Temp
        fcst=fcst+ self.makeRow(
            "TEMP", colSpacing6hr, timePeriods['bottom_6hr_snap'],
            statList_6hr_snap, self._tempValue, ["T"], self._rowLabelWidth)


        # DewPt
        fcst=fcst+ self.makeRow(
            "DEWPT", colSpacing6hr, timePeriods['bottom_6hr_snap'],
            statList_6hr_snap, self._tempValue, ["Td"], self._rowLabelWidth)

        # Predominant Wind direction
        fcst=fcst+ self.makeRow(
            "PWIND DIR", colSpacing12hr, timePeriods['bottom_12hr'],
            statList_12hr, self._windValue, ["dir"],
            self._rowLabelWidth)

        # Wind character
        fcst=fcst+ self.makeRow(
            "WIND CHAR", colSpacing12hr, timePeriods['bottom_12hr'],
            statList_12hr, self._windCharValue, ["Wind", "speed", 0],
            self._rowLabelWidth)

        # Average Clouds - 6hrly
        fcst=fcst+ self.makeRow(
            "AVG CLOUDS", colSpacing6hr, timePeriods['bottom_6hr'],
            statList_6hr, self._skyValue, [], self._rowLabelWidth)


        # Pop
        fcst=fcst+ self.makeRow(
            "POP 12HR", colSpacing12hr, timePeriods['bottom_12hr'],
            statList_12hr, self._popValue, [], self._rowLabelWidth)

        # Weather
        fcst = self._createWxRows(fcst, timePeriods['bottom_6hr'],
                                 self._sampler,
                                 self._analysisList_bottom_6hr(), editArea,
                                 colSpacing6hr)
        return fcst

    def _postProcessArea(self, fcst, editArea, areaLabel, argDict):
        return fcst + "\n$$\n"

    def _postProcessProduct(self, fcst, argDict):
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    # provides expiration time offset from today's midnight based on issuance.
    def expireOffset(self):
        if self._productIssuance == "Morning":
            #4pm today
            return 16
        else:
            #4am tomorrow
            return 24+4
    

    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _analysisList_top_3hr(self):          #None needed
      return [
          
          ]

    def _analysisList_top_3hr_snap(self):
      return [
          ("T", self.avg),
          ("Wind", self.vectorAvg),
          ("WindGust", self.maximum),
          ("WindChill", self.avg),
          ("HeatIndex", self.avg),
          ("Sky", self.avg),
          ("Td", self.avg),
          ("RH", self.avg),
          ("Wx", self.dominantWx),
          ]

    def _analysisList_top_6hr(self):
      return [
          ]

    def _analysisList_top_6hrShort(self):
      return [
          ("HeatIndex", self.minMax),
          ("WindChill", self.minMax),
          ]


    def _analysisList_top_12hr(self):
      return [
          ("MaxT", self.minMaxAvg),
          ("MinT", self.minMaxAvg),
          ]

    def _analysisList_top_12hrShort(self):
      return [
          ("PoP",self.stdDevMaxAvg),
          ("QPF",self.minMaxSum),
          ("SnowAmt", self.minMaxSum),
          ]

    def _analysisList_bottom_6hr(self):   
      return [
          ("Sky", self.avg),
          ("Wx", self.dominantWx),
          ]

    def _analysisList_bottom_6hr_snap(self):
      return [
          ("T", self.avg),
          ("Td", self.avg),
          ("RH", self.avg),
          ]

    def _analysisList_bottom_12hr(self):
      return [
          ("Wind", self.vectorAvg),
          ("PoP",self.stdDevMaxAvg),
          ("MaxT", self.avg),
          ("MinT", self.avg),
          ("Sky", self.avg)
          ]

    def _hour24zuluLabel(self, timeRange):
        # returns the starting time of the timeRange in zulu, such as "03"
        label = timeRange.startTime().stringFmt("%H")
        return string.rjust(label, self._top3hrWidth)

    def _hour24localLabel(self, timeRange):
        # returns the starting time of the timeRange in localtime, such as "06"
        start = timeRange.startTime() + self.determineShift()
        label = start.stringFmt("%H")
        return string.rjust(label, self._top3hrWidth)

    def _tempValue(self, statDict, timeRange, argList):
        # return a string for the temperatures, such as "85"
        # can return MM for no data, blanks if timeRange is earlier than now
        if timeRange.startTime().unixTime() < self._currentTime:
            return ""
        val = self.getStats(statDict, argList[0])
        if val is None:
            return "MM"
        return `int(round(val))`

    def _rhValue(self, statDict, timeRange, argList):
        # return a string for the rh, such as "85"
        # can return MM for no data, blanks if timeRange is earlier than now
        if timeRange.startTime().unixTime() < self._currentTime:
            return ""
        # get the T and Td
        tval = self.getStats(statDict, "T")
        if tval is None:
            return "MM"
        tdval = self.getStats(statDict, "Td")
        if tdval is None:
            return "MM"
        t = round(tval)
        td = round(tdval)

        #calc RH from T and Td
        Tc = .556 * (t - 32.0)
        Tdc = .556 * (td - 32.0)
        Vt = 6.11 * pow(10,(Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10,(Tdc * 7.5 / (Tdc + 237.3)))
        RH = (Vd / Vt) * 100.0

        return `int(round(RH))`

    def _popValue(self, statDict, timeRange, argList):
        # return a string for the pop, such as "80"
        # PoP is rounded to nearest 10%, plus the 5% single value is allowed
        # can return MM for no data, blanks if timeRange is earlier than now
        if timeRange.endTime().unixTime() < self._currentTime:
            return ""
        val = self.getStats(statDict, "PoP__stdDevMaxAvg")
        if val is None:
            return "MM"
        popMax5=int(self.round(val,"Nearest",5))
        if popMax5 == 5:
            return "5"
        popMax10=int(self.round(val,"Nearest",10))
        return `int(popMax10)`

    def _qpfValue(self, statDict, timeRange, argList):
        # Return a string for the QPF, such as 0, 0.05, or 0.25-0.49
        # can return "MM" for missing data,
        # blanks if timeRange earlier than now
        # PFMs don't have ranges ever.
        if timeRange.endTime().unixTime() < self._currentTime:
            return ""
        val = self.getStats(statDict, "QPF__minMaxSum")
        if val is None:
            return "MM"
        minV, maxV, sumV = val
        if maxV - minV > self._qpfRangeThreshold and self._productType == "AFM":
            minString = string.strip("%5.2f" %minV)
            maxString = string.strip("%5.2f" %maxV)
            return minString+"-"+maxString
        elif sumV < 0.005:
            return "0"
        else:
            return string.strip("%5.2f" %sumV)


    def _snowValue(self, statDict, timeRange, argList):
        # Return a string for the Snow, such as 00-00, 5, or 5-9
        # Can return "MM" for missing data, blanks if timeRange
        # earlier than now, or if greater than 36 hrs from the base time.
        # PFMs don't have ranges ever.
        #  -- basetime is the argList[0]
        if timeRange.endTime().unixTime() < self._currentTime:
            return ""
        basetime = argList[0]
        if timeRange.startTime().unixTime() >= basetime + 36 * 3600:
            return ""
        val = self.getStats(statDict, "SnowAmt__minMaxSum")
        if val is None:
            return "MM"
        minV, maxV, sumV = val
        if maxV - minV > self._snowRangeThreshold and \
          self._productType == "AFM":
            minString = `int(round(minV))`
            maxString = `int(round(maxV))`
            return minString+"-"+maxString
        elif sumV < 0.1:
            return "00-00"
        elif sumV < 0.5:
            return "T"
        else:
            return `int(round(sumV))`

    def _mxmnValue(self, statDict, timeRange, argList):
        # Return a string for the max or min temperatures.
        # String may be a single value, such as 75, or a range 60 65 70
        # where the values are min, ave, max.  Can return MM for missing data
        # Blanks if timeRange earlier than now.
        # PFMs don't ever have ranges
        if timeRange.endTime().unixTime() < self._currentTime:
            return ""
        dayNight = self.getPeriod(timeRange, shiftToLocal=1)
        if dayNight == self.DAYTIME():
            val = self.getStats(statDict, "MaxT")
        else:
            val = self.getStats(statDict, "MinT")
        if val == None:
            return "MM"

        # did we get a tuple, or just a single value?
        if type(val) is types.TupleType:
            minV, maxV, aveV = val
            if maxV - minV > self._tempRangeThreshold and \
              self._productType == "AFM":
                minString = string.rjust(`int(round(minV))`, self._top3hrWidth)
                aveString = string.rjust(`int(round(aveV))`, self._top3hrWidth)
                maxString = string.rjust(`int(round(maxV))`, self._top3hrWidth)
                return minString+aveString+maxString
            else:
                return `int(round(aveV))`
        else:
            return `int(round(val))`


    def _windChillValue(self, statDict, timeRange, argList):
        # Returns string for WindChill, such as "25"
        # Returns "" for missing data, blanks if data earlier than now.
        # Returns "" if wind chill not below a certain value.
        if timeRange.startTime().unixTime() < self._currentTime:
            return ""

        chill = self.getStats(statDict,"WindChill")
        temp = self.getStats(statDict,"T")
        if chill is None or temp is None:
            return ""

        if chill < temp and chill < self._windChillLimit and \
           (temp - chill) > self._windChillDifference:
            return `int(round(chill))`
        else:
            return ""

    def _heatIndexValue(self, statDict, timeRange, argList):
        # Returns string for HeatIndex, such as "85"
        # Returns "" for missing data, blanks if data earlier than now.
        # Returns "" if heat index not above thresholds
        if timeRange.startTime().unixTime() < self._currentTime:
            return ""

        heat = self.getStats(statDict,"HeatIndex")
        temp = self.getStats(statDict,"T")
        if heat is None or temp is None:
            return ""

        if heat >= self._heatIndexLimit and \
           (heat - temp) >= self._heatIndexDifference:
            return `int(round(heat))`
        else:
            return ""

    def _minWindChillValue(self, statDict, timeRange, argList):
        # Returns string for WindChill, such as "25"
        # Returns "" for missing data, blanks if data earlier than now.
        # Returns wind chill only if below user-set wind chill limit.
        if timeRange.endTime().unixTime() < self._currentTime:
            return ""

        chill = self.getStats(statDict,"WindChill")
        if chill is None:
            return ""
        minV, maxV = chill
        if minV < self._windChillLimit:
            return `int(round(minV))`
        else:
            return ""

    def _maxHeatIndexValue(self, statDict, timeRange, argList):
        # Returns string for HeatIndex, such as "85"
        # Returns "" for missing data, blanks if data earlier than now.
        if timeRange.endTime().unixTime() < self._currentTime:
            return ""

        heat = self.getStats(statDict,"HeatIndex")
        if heat is None:
            return ""

        minV, maxV = heat
        if maxV >= self._heatIndexLimit:
            return `int(round(maxV))`
        return ""


    def _windGustValue(self, statDict, timeRange, argList):
        # Returns string for WindGust, such as "25"
        # Returns "" for missing data, blanks if data earlier than now.
        # Returns "" if gust doesn't exceed normal wind by 10
        # If tropical winds, symbols are used for wind gust.
        if timeRange.startTime().unixTime() < self._currentTime:
            return ""

        windGustVal = self.getStats(statDict,"WindGust")
        windVal = self.getStats(statDict,"Wind")
        if windGustVal is None or windVal is None:
            return ""

        maxGust = windGustVal
        mag, dir = windVal
        if maxGust - mag < (10/1.15):
            return ""   # gust is not significant
        speed = round(maxGust) * 1.15   # convert to MPH

        # hurricane force
        fcstTime = timeRange.endTime().unixTime() - self._currentTime
        if self._tropicalStorm and fcstTime > 24*3600 and speed >= 74:
            return 'HG'

        return `int(speed)`


    def _windValue(self, statDict, timeRange, argList):
        # Returns string for Wind, either direction or speed
        # Format is "25" for speed, "NW" for direction
        # Can return "MM" for missing data, blanks if data earlier than now.
        # If tropical storm, and high winds, different symbols are used.
        # argList[0] for type(dir,speed)
        if timeRange.startTime().unixTime() < self._currentTime:
            return ""
        windType = argList[0]
        windVal = self.getStats(statDict,"Wind")
        if windVal is None:
            return "MM"
        mag, dir = windVal

        # hurricane force - 64kt limits
        fcstTime = timeRange.endTime().unixTime() - self._currentTime
        if self._tropicalStorm and fcstTime > 24*3600 and mag >= 64:
            return 'HU'

        if windType == "dir":
            return self.convertDirection(dir)
        else:
            speed = round(mag) * 1.15   # convert to MPH
            return `int(speed)`

    def _windCharValue(self, statDict, timeRange, argList):
        # Returns wind character (speed characteristic), such as "WY"
        # Can return "MM" for missing data, blanks if timeRange
        # earlier than now.
        if timeRange.endTime().unixTime() < self._currentTime:
            return ""
        value = self.getStats(statDict, "Wind")
        if value is None:
            return "MM"
        maxV, dir  = value
        # convert to MPH
        speed = maxV * 1.15

        # hurricane force
        fcstTime = timeRange.endTime().unixTime() - self._currentTime
        if self._tropicalStorm and fcstTime > 24*3600 and speed >= 74:
            return 'HU'

        # Convert the speed into a text value category. Table shows the
        # max speed allowed for the symbol.
        codes = [("LT", 8), ("GN", 15), ("BZ", 23), ("WY", 31), ("VW", 40),
                 ("SD", 74), ("HF", 300)
                 ]
        for symbol, threshold in codes:
            if speed < threshold:
                return symbol
        return "HF"

    def _skyValue(self, statDict, timeRange, argList):
        # Returns string for sky condition, such as "SC"
        # Can return "MM" for missing data, blanks if timeRange
        # earlier than now
        if timeRange.startTime().unixTime() < self._currentTime:
            return ""
        value =  self.getStats(statDict, "Sky")
        if value is None:
            return "MM"
        # Convert the sky cover percentage into a text value.
        # Table shows the max cloud cover % for that symbol
        codes = [("CL", 5), ("FW", 25), ("SC", 50), ("B1", 69), 
          ("B2", 87), ("OV", 200)]
        for symbol, threshold in codes:
            if value <= threshold:
                return symbol

    def _createWxRows(self, fcst, periods, sampler, analysisList,
                      area, colWidth):
        # Create the row(s) for weather types with codes as column values
        # Create a dictionary of values (SNOW, RAIN, etc.) with
        #  a list of the periods in which they occurred
        # Can return "MM" for missing data, blanks for period earlier than now
        wxDict = {}
        for period, label in periods:

            # Get the Statistics
            statDict = self.getStatDict(sampler, analysisList, period, area)


            # Wx
            wxValues =  self._getWxValues(statDict["Wx"])
            for wxVal, covCode in wxValues:
                if wxDict.has_key(wxVal):
                    wxDict[wxVal].append((period, covCode))
                else:
                    wxDict[wxVal] = [(period, covCode)]

        # Create a row for each weather value in the dictionary
        sortedKeys = wxDict.keys()
        sortedKeys.sort()
        for wxVal in sortedKeys:
            if wxVal == "":
                continue
            fcst = self.addRowLabel(fcst, wxVal, self._rowLabelWidth)
            values = wxDict[wxVal]

            # Add a column for each period
            for x in xrange(len(periods)):
                period, label = periods[x]
                if type(colWidth) is types.ListType:
                    width = colWidth[x]
                else:
                    width = colWidth
                value = ""
                if period.startTime().unixTime() >= self._currentTime:
                    for dictPeriod, covCode in values:
                        if period == dictPeriod:
                            value = covCode
                fcst = self.addColValue(fcst, value, width)
            fcst = fcst + "\n"
        return fcst

    def _getWxValues(self, subkeyList):
        if subkeyList is None:
            return ""
        wxValues = []
        for x in xrange(len(subkeyList)):
            wxKey = subkeyList[x]
            wxValue = ""
            wxCov = ""
            wxType = wxKey.wxType()
            # The following table relates GFE weather types to words used
            # in the AFM/PFM.
            codes = [("R","RAIN"), ("RW", "RAIN SHWRS"), ("T","TSTMS"),
                     ("L","DRIZZLE"), ("S", "SNOW"), ("SW", "SNOWSHWRS"),
                     ("IP", "SLEET"), ("ZR", "FRZG RAIN"),
                     ("ZL", "FRZG DRZL")
                     ]

            # These special codes are used for very light intensities.
            veryLightCodes = [("RW","SPRINKLES"), ("SW", "FLURRIES")]
            for gfecode, symbol in veryLightCodes:
                if wxType == gfecode and wxKey.intensity() == "--":
                    wxValue = symbol
                    break
            if wxValue == '':
                for gfecode, symbol in codes:
                    if wxType == gfecode:
                        wxValue = symbol
                        break

            # determine the coverage codes - note that these are
            # different for the AFM and the PFM (since PFM is a point)
            coverage = wxKey.coverage()


            # The following table relates the gfe coverage/probability code
            # with the AFM coverage/probability code.
            afmCodes = {
                'T':  {"Iso": "IS", "Sct": "SC", "Num": "NM", "Wide": "EC", 
                       "Ocnl": "O", "SChc": "S", "Chc": "C", "Lkly": "L",
                       "Def": "D", 'Frq': "O", 'Brf': "O", 'Pds': "O",
                       'Inter': "O"},
                'R':  {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O", 
                       'Pds': "O", 'Inter': "O"},
                'RW': {"Iso": "IS", "Sct": "SC", "Num": "NM", "Wide": "EC", 
                       "Ocnl": "O", "SChc": "S", "Chc": "C", "Lkly": "L",
                       "Def": "D", 'Frq': "O", 'Brf': "O", 'Pds': "O", 
                       'Inter': "O"},
                'L':  {"Wide": "WD", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", "Areas": "AR",
                       "Patchy": "PA", 'Frq': "O", 'Brf': "O", 'Pds': "O", 
                       'Inter': "O"},
                'ZL':  {"Wide": "WD", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", "Areas": "AR",
                       "Patchy": "PA", 'Frq': "O", 'Brf': "O", 'Pds': "O", 
                       'Inter': "O"},
                'ZR': {"Wide": "WD", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O", 
                       'Pds': "O", 'Inter': "O"},
                'S':  {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O", 
                       'Pds': "O", 'Inter': "O"},
                'SW': {"Iso": "IS", "Sct": "SC", "Num": "NM", "Wide": "EC", 
                       "Ocnl": "O", "SChc": "S", "Chc": "C", "Lkly": "L",
                       "Def": "D", 'Frq': "O", 'Brf': "O", 'Pds': "O", 
                       'Inter': "O"},
                'IP': {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O", 
                       'Pds': "O", 'Inter': "O"}
                }

            # The following table relates the gfe coverage/probability code
            # with the PFM coverage/probability code. Note that some codes
            # for the pfm have been remapped to probability terms due to
            # the point nature of the pfm.
            pfmCodes = {
                'T':  {"Iso": "S", "Sct": "C", "Num": "L", "Wide": "O", 
                       "Ocnl": "O", "SChc": "S", "Chc": "C", "Lkly": "L",
                       "Def": "D", 'Frq': "O", 'Brf': "O",
                       "Pds": "O", 'Inter': "O"},
                'R':  {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O",
                       "Pds": "O", 'Inter': "O"},
                'RW': {"Iso": "S", "Sct": "C", "Num": "L", "Wide": "O", 
                       "Ocnl": "O", "SChc": "S", "Chc": "C", "Lkly": "L",
                       "Def": "D", 'Frq': "O", 'Brf': "O",
                       "Pds": "O", 'Inter': "O"},
                'L':  {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", "Areas": "AR", "Patchy": "O",
                       'Frq': "O", 'Brf': "O", "Pds": "O", 'Inter': "O"},
                'ZL': {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", "Areas": "AR", "Patchy": "O",
                       'Frq': "O", 'Brf': "O", "Pds": "O", 'Inter': "O"},
                'ZR': {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O",
                       "Pds": "O", 'Inter': "O"},
                'S':  {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O",
                       "Pds": "O", 'Inter': "O"},
                'SW': {"Iso": "S", "Sct": "C", "Num": "L", "Wide": "O", 
                       "Ocnl": "O", "SChc": "S", "Chc": "C", "Lkly": "L",
                       "Def": "D", 'Frq': "O", 'Brf': "O",
                       "Pds": "O", 'Inter': "O"},
                'IP': {"Wide": "O", "Ocnl": "O", "SChc": "S", "Chc": "C", 
                       "Lkly": "L", "Def": "D", 'Frq': "O", 'Brf': "O",
                       "Pds": "O", 'Inter': "O"}
                }


            # now map the codes
            if self._productType == "AFM":
                if afmCodes.has_key(wxType) and \
                  afmCodes[wxType].has_key(coverage):
                    wxCov = afmCodes[wxType][coverage]
                else:
                    wxCov = "?"
            elif self._productType == "PFM":
                if pfmCodes.has_key(wxType) and \
                  pfmCodes[wxType].has_key(coverage):
                    wxCov = pfmCodes[wxType][coverage]
                else:
                    wxCov = "?"

            if wxValue != "":
                wxValues.append((wxValue, wxCov))

        return wxValues


    def _createWWARows(self, fcst, periods, editAreaList, colWidth):
        # Create the row(s) for WWAs with codes as column values
        # Create a dictionary of values (TO.A, SV.A, etc.) with
        #  a list of the periods in which they occurred
        #  Blanks for period earlier than now
        hazards = self._hazards.getHazardList(editAreaList)
        allowedActions = ['NEW','CON','EXT','EXA','EXB']

        wwaDict = {}
        for period, label in periods:

            # filter out records to this time period
            hazRecords = []
            for h in hazards:
                if h['act'] in allowedActions and \
                  self.__overlaps((h['startTime'],h['endTime']), 
                  (period.startTime().unixTime(),period.endTime().unixTime())):
                    hazRecords.append(h)

            # Hazards - create the row data
            wwaValues =  self._getWWAValues(hazRecords)
            for wwaVal, sigfCode in wwaValues:
                if wwaDict.has_key(wwaVal):
                    wwaDict[wwaVal].append((period, sigfCode))
                else:
                    wwaDict[wwaVal] = [(period, sigfCode)]

        # Create a row for each hazard value in the dictionary
        sortedKeys = wwaDict.keys()
        sortedKeys.sort()
        for wwaVal in sortedKeys:
            fcst = self.addRowLabel(fcst, wwaVal, self._rowLabelWidth)
            values = wwaDict[wwaVal]

            # Add a column for each period
            for x in xrange(len(periods)):
                period, label = periods[x]
                if type(colWidth) is types.ListType:
                    width = colWidth[x]
                else:
                    width = colWidth
                value = ""
                if period.startTime().unixTime() >= self._currentTime:
                    for dictPeriod, sigfCode in values:
                        if period == dictPeriod:
                            value = sigfCode
                fcst = self.addColValue(fcst, value, width)
            fcst = fcst + "\n"
        return fcst

    def _getWWAValues(self, hazRecs):
        wwaValues = []

        # The following table relates VTEC phens with row labels
        # in the AFM/PFM.
        codes = {
         "AF":"ASHFALL", "AS":"AIR STAG", "BS":"BLOWING SNOW", 
         "BW":"BRISK WIND", "BZ":"BLIZZARD", "CF":"COAST FLOOD", 
         "DS":"DUST STORM", "DU":"BLOWING DUST", "EC":"EXTREME COLD", 
         "EH":"EXCESS HEAT", "FA":"FLOOD", "FF":"FLASH FLOOD", 
         "FG":"DENSE FOG", "FL": "FLOOD", "FR":"FROST", "FZ":"FREEZE", 
         "HF":"HURR FRC WND", "HI":"INLAND HURR", "HS":"HEAVY SNOW", 
         "HT":"HEAT", "HU":"HURRICANE", "HW":"HIGH WIND", "HZ": "HARD FREEZE",
         "IP":"SLEET", "IS":"ICE STORM", "LB":"LKEFF SNBLSN", 
         "LE":"LK EFF SNOW", "LS":"LKSHORE FLD", "UP":"ICE ACCRE",
         "LW":"LAKE WIND", "SB":"SNOW BLOSNOW", "SM":"DENSE SMOKE", 
         "SN":"SNOW", "SU":"HIGH SURF", "SV":"SVR TSTORM", 
         "TI":"INL TRP STRM", "TO":"TORNADO", "TR":"TROP STORM", 
         "TS":"TSUNAMI", "TY":"TYPHOON", "WC":"WIND CHILL", "WI":"WIND", 
         "WS":"WINTER STORM", "WW":"WINTER WEATH", "ZF":"FREEZING FOG", 
         "ZR":"FRZNG RAIN", "FW.W": "RED FLAG", "FW.A": "FIRE WEATHER",
         "ZL":"FRZG DRZL"}

        for rec in hazRecs:
            phen = rec['phen']
            sig = rec['sig']
            phensig = rec['phen'] + '.' + rec['sig']
            if codes.has_key(phen):
                wwaValues.append((codes[phen], sig))
            elif codes.has_key(phensig):
                wwaValues.append((codes[phensig], sig))

        return wwaValues

    def _createObVisRows(self, fcst, periods, sampler, analysisList,
                      area, colWidth):
        # creates and adds to "fcst" the OBVIS row.  OBVIS row included only
        # if there are OBVIS present.
        # determine the obvis values for each period
        obvisValues = []
        for period, label in periods:

            # Get the Statistics
            statDict = self.getStatDict(sampler, analysisList, period, area)

            # Wx
            obvisVal =  self._getObVisValues(statDict["Wx"])
            if period.startTime().unixTime() >= self._currentTime:
                obvisValues.append(obvisVal)
            else:
                obvisValues.append("")   # period earlier than now so blank it

        # Any OBVIS values at all?
        any = 0
        for o in obvisValues:
            if o != "":
                any = 1
                break;
        if any == 0:    # no OBVIS, so skip the row
            return fcst

        fcst = self.addRowLabel(fcst, "OBVIS", self._rowLabelWidth)
        for x in xrange(len(obvisValues)):
            if type(colWidth) is types.ListType:
                width = colWidth[x]
            else:
                width = colWidth
            fcst = fcst + string.rjust(obvisValues[x], width)
        fcst = fcst + "\n"
        return fcst

    def _getObVisValues(self, subkeyList):
        # Returns the obvis code given the weather subkey list
        if subkeyList is None:
            return ""
        wxValues = []
        for x in xrange(len(subkeyList)):
            wxKey = subkeyList[x]
            wxInten = wxKey.intensity()
            wxCov = wxKey.coverage()
            wxType = wxKey.wxType()

            # Various types of Fog
            if wxType in ["F", "IF", "ZF"]:
                if wxInten == "+":
                    if wxCov == "Patchy":
                        return "PF+"
                    else:
                        return "F+"
                elif wxCov == "Patchy":
                    return "PF"
                else:
                    return "F"

            # Other obvis checks
            else:
                # The following table relates gfe wx type codes to AFM/PFM
                # type codes.
                codes = [("H","H"), ("BS", "BS"), ("K","K"), ("BD", "BD"),
                         ("VA", "AF")]
                for gfecode, symbol in codes:
                    if wxType == gfecode:
                        return symbol


        return ""

    def _calcPeriodLabels(self, periods, colWidth, startPoint, intervalHours,
      timeZone):
        # Calculate the period labels and returns as (date, utc, lt) strings
        #DATE          THU 08/01/02            FRI 08/02/02
        #UTC 3HRLY  09 12 15 18 21 00 03 06 09 12 15 18 21 00 03 06
        #MDT 3HRLY  03 06 09 12 15 18 21 00 03 06 09 12 15 18 21 00

        # determine the column widths
        colWidths = []
        if type(colWidth) is types.ListType:
            colWidths = colWidth
        else:
            for p in periods:
                colWidths.append(colWidth)

        # calculate the zulu labels
        zuluLabels = []
        for period,label in periods:
            zuluLabels.append(self._hour24zuluLabel(period))

        # zulu string
        zulu = "UTC " + `intervalHours` + "HRLY "
        zulu = string.ljust(zulu, startPoint)
        for x in xrange(len(zuluLabels)):
            zulu = self.addColValue(zulu, zuluLabels[x], colWidths[x])

        # set the time zone
        prevTZ = os.environ['TZ']
        os.environ['TZ'] = timeZone
        time.tzset()

        # date and LT string (beginning)
        dateS = string.ljust('DATE', startPoint)
        ltZone = time.strftime("%Z",time.localtime(self._currentTime))
        lt = string.ljust(ltZone, 4) + `intervalHours` + "HRLY "
        lt = string.ljust(lt, startPoint)

        # remainder of DATE and LT strings
        dayOfMonthProcessed = None
        for x in xrange(len(periods)):
            timePeriod, label = periods[x]
            hour = int(label)
            
            # 00LT is considered the end of the day for labeling, so subtract 1                    
            dayOfMonth = (timePeriod.startTime() - 1 + self.determineShift()).day

            # add in local time string
            prevLTlen = len(lt)
            lt = self.addColValue(lt, label, colWidths[x])

            # calculate amount of room to write data
            colAvail = 0
            for y in xrange(x+1,len(periods)):
                colAvail = colAvail + colWidths[y]

            # handle the DATE string
            if dayOfMonth != dayOfMonthProcessed:
                if intervalHours == 3:   #top section
                    if colAvail < 3:
                        format = "%a"
                    elif hour < 6:
                        continue  # label too early in the day
                    elif hour >= 6 and hour < 15:
                        format = "%a %m/%d/%y"
                    elif hour >= 15 and hour < 19:
                        format = " %m/%d/%y"
                    else:
                        continue  #not enough remaining room
                elif intervalHours == 6:   #bottom section
                    if colAvail < 3:
                        format = "%a"
                    elif hour < 1:
                        continue  # label too early in the day
                    elif hour >= 1 and hour < 7:
                        format = "%a %m/%d/%y"
                    elif hour >= 7 and hour < 13:
                        format = " %m/%d/%y"
                    elif hour >= 13 and hour < 17:
                        format = " %m/%d"
                    else:
                        continue  #not enough remaining room
                else:
                    raise Exception, "Expected 3 or 6 intervalHours"

                index = 0
                nfill = prevLTlen - len(dateS) -1 + colWidths[x] - 1
                dateS = dateS + string.ljust(' ',nfill)
                dayTime = timePeriod.startTime() + self.determineShift()
                dString = dayTime.stringFmt(format)
                dateS = dateS + dString

                dayOfMonthProcessed = dayOfMonth   #mark this date processed


        # reset time zone
        os.environ['TZ'] = prevTZ
        time.tzset()

        return (dateS, zulu, lt)

    def _latlonCheck(self, latlon):
        # Routine checks the latlon format for PFM. Returns true if ok.
        # Format must be ll.llN lll.llW
        if len(latlon) != 14:
            return 0
        if latlon[5] != 'S' and latlon[5] != 'N':
            return 0
        if latlon[13] != 'W' and latlon[13] != 'E':
            return 0
        if latlon[2] != '.' or latlon[10] != '.':
            return 0
        digits = [0,1,3,4,8,9,11,12]
        for d in digits:
            if not latlon[d].isdigit():
                return 0
        if latlon[7] != ' ' and latlon[7] != "1":
            return 0
        return 1



    # Returns a list of the Hazards allowed for this product in VTEC format.
    # These are sorted in priority order - most important first.
    #### Removed inland tropical hazards in OB9.3
    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CON"]
        return [
            ('HU.W', allActions, 'Tropical'),     # HURRICANE WARNING
            ('TY.W', allActions, 'Tropical'),     # TYPHOON WARNING
            ('TR.W', allActions, 'Tropical1'),     # TROPICAL STORM WARNING
            ('HU.A', allActions, 'Tropical'),     # HURRICANE WATCH
            ('TY.A', allActions, 'Tropical'),     # TYPHOON WATCH
            ('TR.A', allActions, 'Tropical1'),     # TROPICAL STORM WATCH
            ('HF.W', allActions, 'Marine'),       # HURRICANE FORCE WIND WARNING
            ('BZ.W', allActions, 'WinterWx'),     # BLIZZARD WARNING
            ('IS.W', allActions, 'WinterWx'),     # ICE STORM WARNING
            ('LE.W', allActions, 'WinterWx'),     # LAKE EFFECT SNOW WARNING
            ('WS.W', allActions, 'WinterWx'),     # WINTER STORM WARNING
            ('ZR.Y', allActions, 'WinterWx'),     # FREEZING RAIN ADVISORY
            ('LE.Y', allActions, 'WinterWx'),     # LAKE EFFECT SNOW ADVISORY
            ('WW.Y', allActions, 'WinterWx'),     # WINTER WEATHER ADVISORY
            ('BZ.A', allActions, 'WinterWx'),     # BLIZZARD WATCH
            ('LE.A', allActions, 'WinterWx'),     # LAKE EFFECT SNOW WATCH
            ('WS.A', allActions, 'WinterWx'),     # WINTER STORM WATCH
            ('WC.W', allActions, 'WindChill'),    # WIND CHILL WARNING
            ('WC.Y', allActions, 'WindChill'),    # WIND CHILL ADVISORY
            ('WC.A', allActions, 'WindChill'),    # WIND CHILL WATCH
            ('DS.W', allActions, 'Dust'),         # DUST STORM WARNING
            ('DU.Y', allActions, 'Dust'),         # BLOWING DUST ADVISORY
            ('EC.W', allActions, 'Cold'),         # EXTREME COLD WARNING
            ('EC.A', allActions, 'Cold'),         # EXTREME COLD WATCH
            ('EH.W', allActions, 'Heat'),         # EXCESSIVE HEAT WARNING
            ('EH.A', allActions, 'Heat'),         # EXCESSIVE HEAT WATCH
            ('HT.Y', allActions, 'Heat'),         # HEAT ADVISORY
            ('FG.Y', allActions, 'Fog'),          # DENSE FOG ADVISORY
            ('HZ.W', allActions, 'FrostFreeze'),  # HARD FREEZE WARNING
            ('FZ.W', allActions, 'FrostFreeze'),  # FREEZE WARNING
            ('FR.Y', allActions, 'FrostFreeze'),  # FROST ADVISORY
            ('HZ.A', allActions, 'FrostFreeze'),  # HARD FREEZE WATCH
            ('FZ.A', allActions, 'FrostFreeze'),  # FREEZE WATCH
            ('HW.W', allActions, 'Wind'),         # HIGH WIND WARNING
            ('WI.Y', allActions, 'Wind'),         # WIND ADVISORY
            ('LW.Y', allActions, 'Wind'),         # LAKE WIND ADVISORY
            ('HW.A', allActions, 'Wind'),         # HIGH WIND WATCH
            ('SM.Y', allActions, 'Smoke'),        # DENSE SMOKE ADVISORY
            ('ZF.Y', allActions, 'FreezeFog'),    # FREEZING FOG ADVISORY
            ('FF.A', allActions, 'Flood'),        # FLASH FLOOD WATCH
            ('FA.A', allActions, 'Flood'),        # FLOOD WATCH
            ('FA.W', allActions, 'Flood'),        # FLOOD WARNING
            ('FA.Y', allActions, 'Flood'),        # FLOOD ADVISORY    
            ('CF.W', allActions, 'CoastalFlood'), # COASTAL FLOOD WARNING
            ('LS.W', allActions, 'CoastalFlood'), # LAKESHORE FLOOD WARNING
            ('CF.Y', allActions, 'CoastalFlood'), # COASTAL FLOOD ADVISORY
            ('LS.Y', allActions, 'CoastalFlood'), # LAKESHORE FLOOD ADVISORY
            ('CF.A', allActions, 'CoastalFlood'), # COASTAL FLOOD WATCH
            ('LS.A', allActions, 'CoastalFlood'), # LAKESHORE FLOOD WATCH
            ('UP.W', allActions, 'IceAcc'),       # ICE ACCREATION WARNING
            ('UP.Y', allActions, 'IceAcc'),       # ICE ACCREATION ADVISORY
            ('AS.Y', allActions, 'AirStag'),      # AIR STAGNATION ADVISORY
            ('AS.O', allActions, 'AirStag'),      # AIR STAGNATION OUTLOOK
            ('SU.W', allActions, 'HighSurf'),     # HIGH SURF WARNING
            ('SU.Y', allActions, 'HighSurf'),     # HIGH SURF ADVISORY
            ('AF.Y', allActions, 'Ashfall'),      # VOLCANIC ASHFALL ADVISORY
            ('TO.A', allActions, 'Convective'),   # TORNADO WATCH
            ('SV.A', allActions, 'Convective'),   # SEVERE THUNDERSTORM WATCH
             ]

    ########################################################################
    # UTILITY FUNCTIONS
    ########################################################################
    # time contains, if time range (tr) contains time (t), return 1
    def __containsT(self, tr, t):
        return (t >= tr[0] and t < tr[1])

    # time overlaps, if tr1 overlaps tr2 (adjacent is not an overlap)
    def __overlaps(self, tr1, tr2):
        if self.__containsT(tr2, tr1[0]) or self.__containsT(tr1, tr2[0]):
            return 1
        return 0


    ########################################################################
    # OVERRIDING THRESHOLDS AND VARIABLES
    ########################################################################

    def getDefaultPercentage(self, parmName):
        return 5.0

    
