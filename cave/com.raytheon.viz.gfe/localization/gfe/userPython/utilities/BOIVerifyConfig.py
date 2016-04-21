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
#---------------------------------------------------------------------
#
#  B O I V E R I F Y   C O N F I G U R A T I O N
#
#---------------------------------------------------------------------
#
#  VERDIR = top directory where verification data will be stored
#
VERDIR="/data/verify"
#
#  EDITAREAS = filename with list of edit areas (must be in VERDIR
#              directory)
#
EDITAREAS="EditAreas.dat"
#
#  PERCENT_COLOR = Color table name to use for percentage displays
#
PERCENT_COLOR="Warm To Cold"
#
#  GRIDDAYS = number of days to keep grids in database
#  STATDAYS = number of days to keep stats in database
#
#  These can be changed - but they have serious implications for the
#  size of the files that are created.  Once you have large netCDF
#  datafiles, making these numbers smaller will have no effect.
#  Eventually we'll have a program that will change of a grid database
#  for more or less days.
#
GRIDDAYS=200  # number of days of grids to keep
STATDAYS=500 # number of days to keep stats
#
#  OBSMODELS = Name of databases that can be used for "truth"
#
OBSMODELS=["Obs","RFCQPE"]
#OBSMODELS=["RTMA"]
#
#  VERCONFIG = configuration info for each parms to verify.
#
#  For each parm, specify a tuple of:
#
#    parmType   = 0=SCALAR, 1=VECTOR (we don't do weather yet)
#    verType    = 0=value, 1=probability
#    SaveInt    = 0=save all grids, 3= save 0,3,6,9 UTC grids, etc.
#    Thresholds = a tuple with 5 threshold values
#    BinWidth   = width of bins in histograms
#    MaxErr     = max error to show in displays
#    ColorCurve = color curve to use for error displays
#    obsParm    = observed Parm name.  For probability type parms
#                 this is a tuple with 3 entries:
#                       observed Parm Name
#                       threshold type string (">" ">=" "<" ",=")
#                       threshold value
#
#  If parm is a vector, then each of the config values from Thresholds on
#  are tuples with the first for 'magnitude' and the second for 'direction'
#
VERCONFIG={}
VERCONFIG["T"]      =(0,0,3, (1,3,5,7,10), 0.5, 20, "BV_Change1","T")
VERCONFIG["MaxT"]   =(0,0,0, (1,3,5,7,10), 0.5, 20, "BV_Change1","MaxT")
VERCONFIG["MinT"]   =(0,0,0, (1,3,5,7,10), 0.5, 20, "BV_Change1","MinT")
VERCONFIG["Td"]     =(0,0,3, (1,3,5,7,10), 0.5, 20, "BV_Change2","Td")
VERCONFIG["TdAft"]  =(0,0,0, (1,3,5,7,10), 0.5, 20, "BV_Change2","TdAft")
VERCONFIG["TdMrn"]  =(0,0,0, (1,3,5,7,10), 0.5, 20, "BV_Change2","TdMrn")
VERCONFIG["RH"]     =(0,0,3,(2,4,8,15,30), 1.0, 40, "BV_Change2","RH")
VERCONFIG["MaxRH"]  =(0,0,0,(2,4,8,15,30), 1.0, 40, "BV_Change2","MaxRH")
VERCONFIG["MinRH"]  =(0,0,0,(2,4,8,15,30), 1.0, 40, "BV_Change2","MinRH")
VERCONFIG["Wind"]   =(1,0,3,((1,3,5,7,10),(10,30,50,70,90)),
                          (1.0,10.0),(20,180),("BV_Change1","BV_Change1"),"Wind")
VERCONFIG["PoP"]    =(0,1,0,(5,10,20,30,50), 2.5, 100, "BV_Change2",("QPE06",">=",0.01))
VERCONFIG["QPF"]    =(0,0,0,(0.05,0.10,0.25,0.50,1.00),0.05,1.0,"BV_Change2","QPE06")
#
#  SAVE_MODELS = Names of GFE databases to archive in BOIVerifySave
#
SAVE_MODELS=["GFS40","GFS40BC","ADJMAV","ADJMAVBC","ADJMEX","ADJMEXBC",
            "ADJMEH","ADJMEN","ADJMEL",
            "MOSGuide","MOSGuideBC",
            "NAM12","NAM12BC","ADJMET","ADJMETBC",
            "DGEX","DGEXBC","ADJDGX","ADJDGXBC",
            "NGM80","NGM80BC","ADJFWC","ADJFWCBC",
            "ADJKAF","ADJKAFBC",
            "SREF","SREFBC",
            "Official","ISC",
            ]
#
#  Possible Model Cycles - as strings.  The hours
#  for these cycles, relative to 00 UTC are taken as
#  the int() of these strings.
#
ALLCYCLES=["00","03","06","09","12","15","18","21"]
#
#  Rounding used in scale entries
#
NOMINALSPACING=1.25
#
#  Maximum number of hours in any forecast from any model
#     (you can set it higher than needed - but it will slow things down
#      during the saving of data, and also when searching for data)
#
MAXFORECASTHOUR=240
#
#  If FORECASTER_LIST_TRIMMING is set to 1, then they can only
#  calculate stats for the username that they logged into GFE as
#  (or the entire office).  If set to 0, then they can calculate
#  for anyone in the office
#
FORECASTER_LIST_TRIMMING=0
#
#  Even if FORECASTER_LIST_TRIMMING is turned on, if you list a
#  username in the ADMINISTRATORS list, that user will be able to
#  calculate data for any individual in the office
#
FORECASTER_LIST_TRIMMING_ADMINISTRATORS=['SITE','tbarker']
#
#  The format of the forecaster list can be "number" "id","name",
#  "number-name", or "number-id","
#
FORECASTER_LIST_FORMAT="number-name"
#
#  Sorting of the forecaster list can be done by "number","id", or "name"
#
FORECASTER_LIST_SORT="number"
#
#  Resolution of Accumulation periods (i.e., if this is set to 3, then
#  verification can only be done on periods 3, 6, 9, 12 hours long, etc.
#  whereas, if this is 6, then verification can only be done on periods
#  6, 12, 18, 24 hours long, etc.
#
ACCUM_RESOLUTION=3
#
#  This is the default length of Accumulation periods.  It must be an
#  interval of ACCUM_RESOLUTION above.
#
ACCUM_LENGTH_DEFAULT=12
#
#  This is the default frequency of Accumulation periods.  It must be
#  greater than 1, but should NOT be greater than 24.
#
ACCUM_FREQUENCY_DEFAULT=12
#
#  When looking for grids that are "in" a particular day, we need to
#  be careful about 'long' grids, like MaxT or MinT.  For example, in
#  some timezones, a "UTC day" will have the end of one MaxT grid, then
#  a blank time, and then the start of another MaxT grid.  Which one is
#  the "one" for that particular day?  The OFFSET hours are added to the 
#  start and end of a UTC day - and then grids are checked to see if they
#  intersect this new 'time period' for the day.
#
#  For example, in the mountain timezone, a MaxT grid starts at 14Z and 
#  lasts through 02Z the next UTC day.  When looking at grids for, say,
#  Oct 3rd, we don't want to consider the one that crosses over the first
#  2 hours of the UTC day - we want only the one that intersects the "end"
#  of the day.  So...we set the START_OFFSET_HOURS for "MaxT" to 12.  Only
#  grids that cross over the 12Z to 00Z 'end' of the day will be considered
#  for 'that day'.
#
#  Likewise, some definitions of MaxRH in the mountain timezone go from
#  22Z on one day, through 16Z on the next day (although there is no
#  'standard' for period definitions for MaxRH/MinRH).  We want the MaxRH
#  for a particular day, to be the one that covers the period from 00Z to
#  16Z, not the 2 hours of the next one that starts at 22Z late in the day.
#  So...we set the END_OFFSET_HOURS for MaxRH to -12.  The 'end' of the day
#  is now 12Z, so only grids that intersect the 00Z to 12Z period are
#  considered for that day.
#
#  For parms NOT listed here - the offset is zero - which is the normal
#  situation for parms that don't cover a time period.
#
END_OFFSET_HOURS=  {"MinT":-12,
                    "TdMrn":-12,
                    "MaxRH":-12,
                   }
START_OFFSET_HOURS={"MaxT":12,
                    "TdAft":12,
                    "MinRH":12,
                   }
#
#  BASE_OFFSET is used when figuring out common cases.  For any model
#  listed here, we add the specified number of hours to it's real basetime
#  to get it's "effective" basetime, and compare that to other models
#  when finding common cases.  For example, if we have "SREF":-3, here
#  then for a 15Z run of the SREF model - we will add -3 hours to that,
#  getting 12Z - and check for other 12Z models when finding common cases.
#     
BASE_OFFSET={"DGEX":6,
             "DGEXBC":6,
             "ADJDGX":6,
             "ADJDGXBC":6,
             "SREF":-3,
             "SREFBC":-3,
             }
#
#  When parms are listed in the BOIVerifyInfo - they will be in this order
#  with any other parms added alphabetically after those listed here.
#
PARMINFO_ORDER=["MaxT","MinT","T","PoP","QPF","Wind","MaxRH","MinRH","RH",
                "TdMrn","TdAft","Td"]
#
#  SAVE_MAXVERSIONS is the maximum number of old GFE databases to scan
#  to see if there is any data that has not yet been moved to the
#  BOIVerify databases.  I suppose you could save some time by setting
#  this smaller - but it takes just a second or two to scan a GFE
#  database to see if any of it's grids need to be archived...so I
#  wouldn't change this from 10
#
SAVE_MAXVERSIONS=10
#
#  AUTOCALC_DAYS is the normal number of recent days observations to
#  scan for new data - when pre-calculating scores in BOIVerifyAutoCalc
#
AUTOCALC_DAYS=5
#
#  AUTOCALC_TOO_RECENT is in hours.  Used by BOIVerifyAutoCalc.
#  If an observation grid has been stored within this many hours - it
#  won't calculate scores yet - figuring that this observation grid
#  might get revised later and we don't want to waste time calculating
#  these scores again.
#
AUTOCALC_TOO_RECENT=12
#
#  BIASCORR_DAYS is the normal number of recent days forecasts to use
#  for the Bias Correction regression in BOIVerifyBiasCorr
#
BIASCORR_DAYS=30
#
#  BIASCORR_MINDAYS is the minimum number of recent days that have to exist
#  before performing a Bias Correction regression in BOIVerifyBiasCorr
#
BIASCORR_MINDAYS=14
