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
# NOTE: THIS FILE SHOULD NOT BE USER-MODIFIED.  INSTEAD REFER TO THE
# LOCAL CONFIG DOCUMENTATION ON HOW TO OVERRIDE SETTINGS IN THIS FILE.

#----------------------------------------------------------------------------
# USEFUL DEFINES
#----------------------------------------------------------------------------
#import siteConfig, LogStream, config, imp
import siteConfig,imp

BASELINE = getattr(siteConfig, 'BASELINE', 0)

# imports the named module.  If the module
# does not exist, it is just ignored.  But
# if it exists and has an error, the exception
# is thrown.  If the module was imported returns
# true.
def siteImport(modName):
    try:
        fp, path, des = imp.find_module(modName)
        if fp:
            fp.close()
    except ImportError:
        return 0
    globals()[modName] = __import__(modName)
    return 1

GFESUITE_SITEID = siteConfig.GFESUITE_SITEID
GFESUITE_MHSID = siteConfig.GFESUITE_MHSID
GFESUITE_SERVER =  siteConfig.GFESUITE_SERVER
GFESUITE_HOME   = siteConfig.GFESUITE_HOME
GFESUITE_PORT   = int(siteConfig.GFESUITE_PORT)
#GFESUITE_DATDIR = siteConfig.GFESUITE_DATDIR
GFESUITE_LOGDIR = siteConfig.GFESUITE_LOGDIR
GFESUITE_PRDDIR = siteConfig.GFESUITE_PRDDIR
#GFESUITE_SHPDIR = siteConfig.GFESUITE_SHPDIR
#GFESUITE_TOPODIR = siteConfig.GFESUITE_TOPODIR
#GFESUITE_VTECDIR = siteConfig.GFESUITE_VTECDIR

SID = GFESUITE_SITEID

ALASKA_SITES = ['AFG', 'AJK', 'AICE', 'ALU', 'AER', 'ACR', 'AFC']
GreatLake_SITES = ['LOT', 'MKX', 'GRB', 'DLH', 'MQT', 'APX', 'GRR', 'DTX', 
          'IWX', 'CLE', 'BUF', 'PBZ', 'ILN', 'IND', 'ILX', 'MPX', 'FGF']
CONUS_EAST_SITES = ['ALY', 'AKQ', 'APX', 'BGM', 'BMX', 'BOX', 'BTV', 'BUF',
                    'CAE', 'CAR', 'CHS', 'CLE', 'CTP', 'DTX', 'FFC', 'GRR',
                    'GSP', 'GYX', 'ILM', 'ILN', 'IND', 'JAN', 'JAX', 'JKL',
                    'LCH', 'LMK', 'LWX', 'MEG', 'MFL', 'MHX', 'MLB', 'MOB',
                    'MQT', 'MRX', 'OKX', 'PAH', 'PBZ', 'PHI', 'RAH', 'RLX',
                    'RNK', 'TAE', 'TBW', 'ALR', 'RHA', 'TAR', 'TIR']
RFC_SITES = ["ACR", "FWR", "KRF", "MSR", "ORN", "PTR",
          "RHA", "RSA", "STR", "TAR", "TIR", "TUA"]

#---------------------------------------------------------------------------
#
#  Weather Element configuration section.
#
#---------------------------------------------------------------------------

SCALAR  = 'Scalar'
VECTOR  = 'Vector'
WEATHER = 'Weather'
DISCRETE = 'Discrete'
YES = 1
NO = 0

#SCALAR, VECTOR
# name/type/units/description/max/min/precision/rateParm/
#WEATHER
# name/WEATHER/units/description/
#DISCRETE
# keyDef = [(keySym, keyDesc), (keySym, keyDesc)]
# name/DISCRETE/units/description/overlapCapable/keyDef/

# Standard Public Weather Elements
SID = GFESUITE_SITEID
Temp =    ("T", SCALAR, "F", "Surface Temperature", 120.0, -80.0, 0, NO)
Td =      ("Td", SCALAR, "F", "Dewpoint", 120.0, -80.0, 0, NO)
MaxT =    ("MaxT", SCALAR, "F", "Maximum Temperature", 120.0, -80.0, 0, NO)
MinT =    ("MinT", SCALAR, "F", "Minimum Temperature", 120.0, -80.0, 0, NO)
HeatIndex = ("HeatIndex", SCALAR, "F", "Heat Index", 130.0, -80.0, 0, NO)
WindChill = ("WindChill", SCALAR, "F", "Wind Chill", 120.0, -120.0, 0, NO)
QPF =     ("QPF", SCALAR, "in", "QPF", 5.0, 0.0, 2, YES)
Wind =    ("Wind", VECTOR, "kts", "Surface Wind", 125.0, 0.0, 0, NO)
# special for TPC hurricane winds
HiWind =    ("Wind", VECTOR, "kts", "Surface Wind", 200.0, 0.0, 0, NO)
Weather = ("Wx", WEATHER, "wx", "Weather")
IceAcc = ("IceAccum", SCALAR, "in", "Ice Accumulation", 12.0, 0.0, 1, YES)
SnowAmt = ("SnowAmt", SCALAR, "in", "Snowfall amount", 20.0, 0.0, 1, YES)
StormTotalSnow = ("StormTotalSnow", SCALAR, "in","Storm Total Snow", 50.0,
                  0.0, 1, YES)
PoP     = ("PoP", SCALAR, "%", "Prob of Precip", 100.0, 0.0, 0, NO)
PoP6    = ("PoP6", SCALAR, "%", "Prob of Precip (6hr)", 100.0, 0.0, 0, NO)
PoP12   = ("PoP12", SCALAR, "%", "Prob of Precip (12hr)", 100.0, 0.0, 0, NO)
TstmPrb3 = ("TstmPrb3", SCALAR, "%", "Prob of Tstorm (3hr)", 100.0, 0.0, 0, NO)
TstmPrb6 = ("TstmPrb6", SCALAR, "%", "Prob of Tstorm (6hr)", 100.0, 0.0, 0, NO)
TstmPrb12 = ("TstmPrb12", SCALAR, "%", "Prob of Tstorm (12hr)", 100.0, 0.0, 0,
             NO)
Sky     = ("Sky", SCALAR, "%", "Sky Condition", 100.0, 0.0, 0, NO)
FzLevel = ("FzLevel", SCALAR, "ft", "Freezing level", 30000.0, 0.0, 0, NO)
SnowLevel = ("SnowLevel", SCALAR, "ft", "Snow Level", 18000.0, 0.0, 0, NO)
RH      = ("RH", SCALAR, "%", "Relative Humidity", 100.0, 0.0, 0, NO)

# DR20541 and 20482 - add collaborate PoP, SnowAmt, QPF and ndfd QPF tools
PoP12hr = ("PoP12hr", SCALAR, "%", "12 hr Chance of Precip", 100.0, 0.0, 0, NO)
QPF6hr = ("QPF6hr", SCALAR, "in", "6 hr Precipitation (in)", 5.0, 0.0, 2, YES)
SnowAmt6hr = ("SnowAmt6hr", SCALAR, "in", "6 hr Snowfall", 30.0, 0.0, 1, YES)

# Cobb SnowTool included. 
SnowRatio = ('SnowRatio', SCALAR, '%', 'Snow Ratio', 40.0, 0.0, 1, YES)
#totalVV = ('totalVV', SCALAR, 'ubar/s', 'Total VV', 400.0, 0.0, 0, YES) 
cape = ("cape", SCALAR, "1unit", "CAPE", 8000.0, 0.0, 1, NO)
ApparentT = ("ApparentT", SCALAR, "F", "Apparent Temperature", 130.0, -120.0, 0, NO)
UWaveDir = ("UWaveDir", SCALAR, "m/s", "U WaveDir Comp", 0.50, -0.50, 3, NO)
VWaveDir = ("VWaveDir", SCALAR, "m/s", "V WaveDir Comp", 0.50, -0.50, 3, NO)
LkSfcT = ("LkSfcT", SCALAR, "C", "Lake Surface T", 40.0, -2.0, 1, NO)
SnowMap = ("SnowMap", SCALAR, "in", "Snowfall Map", 20.0, 0.0, 1, YES)
WaveDir = ("WaveDir", VECTOR, "m/s", "Wave Direction", 5.0, 0.0, 2, NO)
StormTotalQPF = ('StormTotalQPF', SCALAR, 'in', 'Storm Total QPF (in)', 10.0, 0.0, 2, YES)
SeasonTotalSnow = ('SeasonTotalSnow', SCALAR, 'in', 'Season Total Snow (in)', 150.0, 0.0, 2, YES)

# Marine Weather Elements
WindWaveHeight = ("WindWaveHgt", SCALAR, "ft", "Wind Wave Height",
  100.0, 0.0, 0, NO)
WaveHeight = ("WaveHeight", SCALAR, "ft", "Total Wave Height",
  100.0, 0.0, 0, NO)
Swell = ("Swell", VECTOR, "ft", "Primary Swell", 100.0, 0.0, 0, NO)
Swell2 = ("Swell2", VECTOR, "ft", "Secondary Swell", 100.0, 0.0, 0, NO)
Period = ("Period", SCALAR, "sec", "Primary Period", 20.0, 0.0, 0, NO)
Period2 = ("Period2", SCALAR, "sec", "Secondary Period", 20.0, 0.0, 0, NO)
WindGust = ("WindGust", SCALAR, "kts", "Wind Gust", 125.0, 0.0, 0, NO)
IceCoverage = ("IceCoverage", SCALAR, "%", "Ice Coverage Amount",
  100.0, 0.0, 0, NO)
SurfHeight = ("SurfHeight", SCALAR, "ft", "Total Wave Height",
  100.0, 0.0, 0, NO)
##########DCS3499
SigWaveHgt = ("SigWaveHgt", SCALAR, "ft", 
  "Significant wave height of combined wind waves and swells",
  30.0, 0.0, 0, NO)
WindWaveHgt = ("WindWaveHgt", SCALAR, "ft", "Significant wave height of wind waves",
  30.0, 0.0, 0, NO)
WindWavePeriod = ("WindWavePeriod", SCALAR, "sec.", "Wind wave peak period", 20.0, 0.0, 0, NO)
WindWaveDir = ("WindWaveDir", VECTOR, "degree", "Direction of wind waves", 100.0, 0.0, 0, NO)

# Fire Weather Weather Elements
LAL = ("LAL", SCALAR, "cat", "Lightning Activity Level", 6.0, 1.0, 0, NO)
CWR = ("CWR", SCALAR, "%", "Chance of Wetting Rain", 100.0, 0.0, 0, NO)
Haines = ("Haines", SCALAR, "cat", "Haines Index", 6.0, 2.0, 0, NO)
MixHgt = ("MixHgt", SCALAR, "ft", "Mixing Height", 20000.0, 0.0, 0, NO)
Wind20ft =    ("Wind20ft", VECTOR, "kts", "20ft. Wind", 125.0, 0.0, 0, NO)
FreeWind = ("FreeWind", VECTOR, "kts", "Free Air Wind", 125.0, 0.0, 0, NO)
TransWind = ("TransWind", VECTOR, "kts", "Transport Wind", 125.0, 0.0, 0, NO)
Stability = ("Stability",SCALAR,"cat","Stability", 6.0,1.0,0, NO)
HrsOfSun = ("HrsOfSun",SCALAR,"hrs","Hours of Sun",24.0,0.0,1, YES)
MarineLayer = ("MarineLayer",SCALAR,"ft","Depth of Marine Layer",
  20000.0,0.0,0,NO)
InvBurnOffTemp = ("InvBurnOffTemp",SCALAR,"F","Inversion Burn-off Temperature",
  120.0,-30.0,0, NO)
VentRate = ("VentRate", SCALAR, "kt*ft", "VentRate", 500000.0, 0.0, 0, NO)
DSI = ("DSI", SCALAR, "index", "DSI", 6.0, 0.0, 0, NO)
MaxRH      = ("MaxRH", SCALAR, "%", "Maximum Relative Humidity",
  100.0, 0.0, 0, NO)
MinRH      = ("MinRH", SCALAR, "%", "Minimum Relative Humidity",
  100.0, 0.0, 0, NO)
Wetflag = ("Wetflag", SCALAR, "yn", "1300LT WetFlag", 1.0, 0.0, 0, NO)
Ttrend = ("Ttrend", SCALAR, "F", "24hr Temperature Trend", 50.0, -50.0, 0, NO)
RHtrend = ("RHtrend", SCALAR, "F", "24hr Relative Humidity Trend",
  100.0, -100.0, 0, NO)

# HPC Delta weather elements
DeltaMinT = ("DeltaMinT", SCALAR, "F", "Delta Minimum Temperature",
  50.0, -50.0, 0, NO)
DeltaMaxT = ("DeltaMaxT", SCALAR, "F", "Delta Maximum Temperature",
  50.0, -50.0, 0, NO)
DeltaWind = ("DeltaWind", VECTOR, "kts", "Surface Delta Wind",
  125.0, 0.0, 0, NO)
DeltaSky = ("DeltaSky", SCALAR, "%", "Delta Sky Condition",
  100.0, -100.0, 0, NO)
DeltaPoP = ("DeltaPoP", SCALAR, "%", "Delta Prob of Precip",
  100.0, -100.0, 0, NO)

# Special LAPS parms
Radar = ("Radar", SCALAR, "dbz", "Radar Reflectivity", 80.0, -20.0, 0, NO)

# RTMA parms
QPE =     ("QPE", SCALAR, "in", "QPE", 5.0, 0.0, 2, YES)
#if SID in ALASKA_SITES: - not sure if this needs to be like that
if SID in ALASKA_SITES or SID in ["HFO", "SJU"]:
    TUnc =     ("TUnc", SCALAR, "F", "Temperature Anl Uncertainty", 20.0, 0.0, 0, NO)
    TdUnc =    ("TdUnc", SCALAR, "F", "Dewpoint Anl Uncertainty", 25.0, 0.0, 0, NO)
else:
    TUnc =     ("TUnc", SCALAR, "F", "Temperature Anl Uncertainty", 10.0, 0.0, 0, NO)
    TdUnc =    ("TdUnc", SCALAR, "F", "Dewpoint Anl Uncertainty", 15.0, 0.0, 0, NO)
WSpdUnc =  ("WSpdUnc", SCALAR, "kts", "WSpd Anl Uncertainty", 12.0, 0.0, 0, NO)
WDirUnc =  ("WDirUnc", SCALAR, "deg", "WDir Anl Uncertainty", 10.0, 0.0, 0, NO)

# NamDNG5 parms
QPF3 =     ("QPF3", SCALAR, "in", "3HR QPF", 3.0, 0.0, 2, YES)
QPF6 =     ("QPF6", SCALAR, "in", "6HR QPF", 5.0, 0.0, 2, YES)
QPF12 =    ("QPF12", SCALAR, "in", "12HR QPF", 10.0, 0.0, 2, YES)
Vis =      ("Vis", SCALAR, "SM", "Visibility", 10.0, 0.0, 2, NO)
SnowAmt6 = ("SnowAmt6", SCALAR, "in", "Snowfall amount (6hr)", 20.0, 0.0, 1, YES)

MaxT3 =  ("MaxT3", SCALAR, "F", "3hr Maximum Temperature", 120.0, -80.0, 0, NO)
MinT3 =  ("MinT3", SCALAR, "F", "3hr Minimum Temperature", 120.0, -80.0, 0, NO)
MaxRH3 = ("MaxRH3", SCALAR, "%", "3hr Maximum Relative Humidity", 100.0, 0.0, 0, NO)

# Parms for Satellite
SatVisE  = ("VisibleE", SCALAR, "count", "Satellite Albdo %",
            255.0, 0.0, 0, NO)
SatIR11E = ("IR11E", SCALAR, "C", "11 micron temperature", 58.0, -111.0, 0, NO)
SatIR13E = ("IR13E", SCALAR, "C", "13 micron temperature", 50.0, -111.0, 0, NO)
SatIR39E = ("IR39E", SCALAR, "C", "3.9 micron temperature", 50.0,
            -111.0, 0, NO)
SatWVE   = ("WaterVaporE", SCALAR, "C", "water vapor temperature",
            -11.0, -62.0, 0, NO)
SatFogE  = ("FogE", SCALAR, "C", "ir11 - ir39", 50.0, -111.0, 0, NO)

SatVisW  = ("VisibleW", SCALAR, "count", "Satellite Albdo %",
            255.0, 0.0, 0, NO)
SatIR11W = ("IR11W", SCALAR, "C", "11 micron temperature", 58.0, -111.0, 0, NO)
SatIR13W = ("IR13W", SCALAR, "C", "13 micron temperature", 50.0, -111.0, 0, NO)
SatIR39W = ("IR39W", SCALAR, "C", "3.9 micron temperature", 50.0,
            -111.0, 0, NO)
SatWVW   = ("WaterVaporW", SCALAR, "C", "water vapor temperature",
            -11.0, -62.0, 0, NO)
SatFogW  = ("FogW", SCALAR, "C", "ir11 - ir39", 50.0, -111.0, 0, NO)

# TPC Wind Probability parms
prob34 = ("prob34", SCALAR, "%", "WS34 CPROB", 100.0, 0.0, 0, NO)
prob50 = ("prob50", SCALAR, "%", "WS50 CPROB", 100.0, 0.0, 0, NO)
prob64 = ("prob64", SCALAR, "%", "WS64 CPROB", 100.0, 0.0, 0, NO)
pws34 = ("pws34", SCALAR, "%", "34WSIPROB", 100.0, 0.0, 0, NO)
pws50 = ("pws50", SCALAR, "%", "50WSIPROB", 100.0, 0.0, 0, NO)
pws64 = ("pws64", SCALAR, "%", "64WSIPROB", 100.0, 0.0, 0, NO)
pwsD34 = ("pwsD34", SCALAR, "%", "Day34WSIPROB", 100.0, 0.0, 0, NO)
pwsN34 = ("pwsN34", SCALAR, "%", "Night34WSIPROB", 100.0, 0.0, 0, NO)
pwsD64 = ("pwsD64", SCALAR, "%", "Day64WSIPROB", 100.0, 0.0, 0, NO)
pwsN64 = ("pwsN64", SCALAR, "%", "Night64WSI PROB", 100.0, 0.0, 0, NO)
pws34int = ("pws34int", SCALAR, "%", "34WSIntPROB", 100.0, 0.0, 0, NO)
pws64int = ("pws64int", SCALAR, "%", "64WSIntPROB", 100.0, 0.0, 0, NO)

# Surge parms for HLS
SurgeHtPlusTide = ("SurgeHtPlusTide", SCALAR, "ft", "SurgeHtPlusTide", 50.0, -100.0, 2, NO)
SurgeHtPlusTideWTopo = ("SurgeHtPlusTideWTopo", SCALAR, "ft", "SurgeHtPlusTideWTopo", 50.0, -100.0, 2, NO)

# Hazards
HazardKeys = []
HazardKeys.append(("<None>", ""))  #1st one must be None
import VTECTable
kys = VTECTable.VTECTable.keys()
kys.sort()
for k in kys:
    HazardKeys.append((k, VTECTable.VTECTable[k]['hdln']))

#H-VTEC keys - will someday add these back in
#("hydroER", "Hydro - Excessive Rainfall"),
#("hydroSM", "Hydro - Snow melt"),
#("hydroRS", "Rain and Snow melt"),
#("hydroDM", "Dam or Levee Failure"),
#("hydroGO", "Glacier-Dammed Lake Outburst"),
#("hydroIJ", "Ice Jam"),
#("hydroIC", "Rain and/or Snow melt and/or Ice Jam"),

Hazards = ("Hazards", DISCRETE, "wwa", "Hazards", YES, HazardKeys, 4)

# Scalar/Vector Weather Elements that Require Extra Precision (due to their
# use in calculations) Either form may be used.
ExtraWEPrecision = []


#---------------------------------------------------------------------------
#
#  Weather configuration section
#
#---------------------------------------------------------------------------

# list of possible visibilities
visibilities = ['<NoVis>', '0SM', '1/4SM', '1/2SM', '3/4SM', '1SM', '11/2SM',
                '2SM', '21/2SM', '3SM', '4SM', '5SM', '6SM', 'P6SM']

# list of possible coverages and probabilities
NOCOV = ('<NoCov>', 'No Coverage')
ISOD = ('Iso', 'Isolated')
SCT = ('Sct', 'Scattered')
NUM = ('Num', 'Numerous')
WIDE = ('Wide', 'Widespread')
OCNL = ('Ocnl', 'Occasional')
SCHC = ('SChc', 'Slight Chance Of')
CHC = ('Chc', 'Chance Of')
LKLY = ('Lkly', 'Likely')
DEFN = ('Def', 'Definite')
PATCHY = ('Patchy', 'Patchy')
AREAS = ('Areas', 'Areas of')
FQT = ('Frq', 'Frequent')
BRIEF = ('Brf', 'Brief')
PERIODS = ('Pds', 'Periods of')
INTM = ('Inter', 'Intermittent')

# list of possible intensities
INTEN_NONE = ('<NoInten>', 'No intensity')
INTEN_VERYLIGHT = ('--', 'Very Light')
INTEN_LIGHT = ('-', 'Light')
INTEN_MOD = ('m', 'Moderate')
INTEN_HEAVY = ('+', 'Heavy')
INTEN_SEVERE = ('+', 'Severe')
INTEN_DENSE = ('+', 'Dense')

# list of optional attributes
FQTLTG = ('FL', 'Frequent Lightning')
GUSTS = ('GW', 'Gusty Winds')
HVYRAFL = ('HvyRn', 'Heavy Rainfall')
DMGWND = ('DmgW', 'Damaging Winds')
SMALLH = ('SmA', 'Small Hail')
LARGEH = ('LgA', 'Large Hail')
OUTLYNG = ('OLA','in the outlying areas')
GRASSY  = ('OGA','on grassy areas')
OVRPASS = ('OBO','on bridges and overpasses')
OR = ('OR', 'or')
DRY = ('Dry', 'dry')
PRIMARY = ('Primary', 'Highest Ranking')
MENTION = ('Mention', 'Include Unconditionally')
TORNADO = ('TOR', 'Tornadoes')

# list of each weather types
NOWX = ('<NoWx>', 'No Weather',
          [NOCOV],
          [INTEN_NONE],
          [])
THUNDER = ('T', 'Thunderstorms',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF,
            PERIODS, INTM],
          [INTEN_NONE, INTEN_SEVERE],
          [PRIMARY, MENTION, FQTLTG, HVYRAFL, GUSTS, DMGWND, DRY,
            LARGEH, SMALLH, TORNADO])
RAIN = ('R', 'Rain',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
RAINSHOWERS = ('RW', 'Rain Showers',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF,
            PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
DRIZZLE = ('L', 'Drizzle',
          [PATCHY, AREAS, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT,
            BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
FZRAIN = ('ZR', 'Freezing Rain',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
FZDRIZZLE = ('ZL', 'Freezing Drizzle',
          [PATCHY, AREAS, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT,
            BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
SNOW = ('S', 'Snow',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
SNOWSHOWERS = ('SW', 'Snow Showers',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT,
            BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
SLEET = ('IP', 'Sleet',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
FOG = ('F', 'Fog',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE, INTEN_DENSE],
          [PRIMARY, MENTION])
FREEZEFOG = ('ZF', 'Freezing Fog',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE, INTEN_DENSE],
          [PRIMARY, MENTION])
ICEFOG = ('IF', 'Ice Fog',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE],
          [PRIMARY, MENTION])
ICECRYSTAL = ('IC', 'Ice Crystals',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE],
          [PRIMARY, MENTION])
HAZE = ('H', 'Haze',
          [DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
BLWGSNOW = ('BS', 'Blowing Snow',
          [AREAS,PATCHY,DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
BLWGSAND = ('BN', 'Blowing Sand',
          [AREAS,PATCHY,DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
SMOKE = ('K', 'Smoke',
          [AREAS, PATCHY, DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
BLWGDUST = ('BD', 'Blowing Dust',
          [AREAS,PATCHY,DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
FROST = ('FR','Frost',
          [AREAS, PATCHY, WIDE],
          [INTEN_NONE],
          [PRIMARY, MENTION, OUTLYNG])
FRZSPRAY = ('ZY','Freezing Spray',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL],
          [INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION])
VOLASH = ('VA','Volcanic Ash',
          [NOCOV],
          [INTEN_NONE],
          [PRIMARY, MENTION])
WATERSPOUT = ('WP','Waterspouts',
          [ISOD, SCHC, CHC, LKLY, DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])


types = [NOWX, THUNDER, WATERSPOUT, RAIN, RAINSHOWERS,
         DRIZZLE, FZRAIN, FZDRIZZLE, SNOW, SNOWSHOWERS,
         SLEET, FOG, FREEZEFOG, ICEFOG, ICECRYSTAL ,HAZE, BLWGSNOW,
         BLWGSAND, SMOKE, BLWGDUST, FROST, FRZSPRAY, VOLASH]


#-----------------------------------
# DO NOT CHANGE THE FOLLOWING SECTION
#------------------------------------
if not BASELINE and siteImport('localWxConfig'):
    types = localWxConfig.types

#---------------------------------------------------------------------------
#
#  Projection Configuration section.
#
#---------------------------------------------------------------------------
NONE = 0
LAMBERT_CONFORMAL = 1
MERCATOR = 2
POLAR_STEREOGRAPHIC = 3
LATLON = 4

# projectionID / projectionType / latLonLL / latLonUR /
# latLonOrigin / stdParallelOne / stdParallelTwo / gridPointLL / gridPointUR
# latIntersect / lonCenter / lonOrigin

Grid201 = ('Grid201',POLAR_STEREOGRAPHIC,
      (-150.00, -20.826), (-20.90846, 30.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (65, 65), 0.0, 0.0, -105.0)

Grid202 = ('Grid202', POLAR_STEREOGRAPHIC,
      (-141.028, 7.838), (-18.576, 35.617),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (65, 43), 0.0, 0.0, -105.0)

Grid203 = ('Grid203', POLAR_STEREOGRAPHIC,
      (-185.837, 19.132), (-53.660, 57.634),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (45, 39), 0.0, 0.0, -150.0)

Grid204 = ('Grid204', MERCATOR,
      (-250.0, -25.0), (-109.129, 60.644),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (93, 68), 0.0, -179.564, 0.0)

Grid205 = ('Grid205', POLAR_STEREOGRAPHIC,
      (-84.904, 0.616), (-15.000, 45.620),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (45, 39), 0.0, 0.0, -60.0)

Grid206 = ('Grid206', LAMBERT_CONFORMAL,
      (-117.991, 22.289), (-73.182, 51.072),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (51, 41), 0.0, 0.0, 0.0)

Grid207 = ('Grid207', POLAR_STEREOGRAPHIC,
      (-175.641, 42.085), (-93.689, 63.976),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (49, 35), 0.0, 0.0, -150.0)

Grid208 = ('Grid208', MERCATOR,
      (-166.219, 10.656), (-147.844, 27.917),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (25, 25), 0.0, -157.082, 0.0)

Grid209 = ('Grid209', LAMBERT_CONFORMAL,
      (-117.991, 22.289), (-73.182, 51.072),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (101, 81), 0.0, 0.0, 0.0)

Grid210 = ('Grid210', MERCATOR,
      (-77.000, 9.000), (-58.625, 26.422),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (25, 25), 0.0, -67.812, 0.0)

Grid211 = ('Grid211', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (93, 65), 0.0, 0.0, 0.0)

Grid212 = ('Grid212', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (185, 129), 0.0, 0.0, 0.0)

Grid213 = ('Grid213', POLAR_STEREOGRAPHIC,
      (-141.028, 7.838), (-18.577, 35.617),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (129, 85), 0.0, 0.0, -105.0)

Grid214 = ('Grid214', POLAR_STEREOGRAPHIC,
      (-175.641, 42.085), (-93.689, 63.975),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (97, 69), 0.0, 0.0, -150.0)

# (new alaska grid)
Grid214AK = ('Grid214AK', POLAR_STEREOGRAPHIC,
             (-178.571, 40.5301), (-93.689, 63.975),
             (0.0, 0.0), 0.0, 0.0, (1,1), (104, 70), 0.0, 0.0, -150.0)

Grid215 = ('Grid215', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (369, 257), 0.0, 0.0, 0.0)

Grid216 = ('Grid216', POLAR_STEREOGRAPHIC,
      (-173.000, 30.000), (-62.850, 70.111),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (139, 107), 0.0, 0.0, -135.0)

Grid217 = ('Grid217', POLAR_STEREOGRAPHIC,
      (-173.000, 30.000), (-62.850, 70.111),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (277, 213), 0.0, 0.0, -135.0)

Grid218 = ('Grid218', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (614, 428), 0.0, 0.0, 0.0)

Grid219 = ('Grid219', POLAR_STEREOGRAPHIC,
      (-119.559, 25.008), (60.339, 24.028),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (385, 465), 0.0, 0.0, -80.0)

Grid221 = ('Grid221', LAMBERT_CONFORMAL,
      (-145.500, 1.000), (-2.566, 46.352),
      (-107.0, 50.0), 50.0, 50.0, (1, 1), (349, 277), 0.0, 0.0, 0.0)

Grid222 = ('Grid222', LAMBERT_CONFORMAL,
      (-145.500, 1.000), (-2.566, 46.352),
      (-107.0, 50.0), 50.0, 50.0, (1, 1), (59, 47), 0.0, 0.0, 0.0)

Grid225 = ('Grid225', MERCATOR,
      (-250.0, -25.0), (-109.129, 60.644),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (185, 135), 0.0, -179.564, 0.0)

Grid226 = ('Grid226', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (737, 513), 0.0, 0.0, 0.0)

Grid227 = ('Grid227', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (1473, 1025), 0.0, 0.0, 0.0)

Grid228 = ('Grid228', LATLON,
      (0.0, 90.0), (359.0, -90.0), (0.0, 0.0), 0.0, 0.0,
      (1, 1), (144, 73), 0.0, 0.0, 0.0)

Grid229 = ('Grid229', LATLON,
      (0.0, 90.0), (359.0, -90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (360, 181), 0.0, 0.0, 0.0)

Grid230 = ('Grid230', LATLON,
      (0.0, 90.0), (359.5, -90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (720, 361), 0.0, 0.0, 0.0)

Grid231 = ('Grid231', LATLON,
      (0.0, 0.0), (359.5, 90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (720, 181), 0.0, 0.0, 0.0)

Grid232 = ('Grid232', LATLON,
      (0.0, 0.0), (359.0, 90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (360, 91), 0.0, 0.0, 0.0)

Grid233 = ('Grid233', LATLON,
      (0.0, -78.0), (358.750, 78.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (288, 157), 0.0, 0.0, 0.0)

Grid234 = ('Grid234', LATLON,
      (-98.000, 15.0), (-65.000, -45.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (133, 121), 0.0, 0.0, 0.0)

Grid235 = ('Grid235', LATLON,
      (0.250, 89.750), (359.750, -89.750),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (720, 360), 0.0, 0.0, 0.0)

HRAP = ('HRAP', POLAR_STEREOGRAPHIC,
      (-119.036, 23.097), (-75.945396, 53.480095),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (801, 881), 0.0, 0.0, -105.0)

NDFD_Oceanic_10K = ('NDFD Oceanic 10km', MERCATOR,
      (-230.094, -30.4192), (10.71, 67.03),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (2517, 1289), 0.0, -109.962, 0.0)

# list of all projections
allProjections = [Grid201, Grid202, Grid203, Grid204, Grid205, Grid206,
 Grid207, Grid208, Grid209, Grid210, Grid211, Grid212, Grid213, Grid214,
 Grid214AK, Grid215, Grid216, Grid217, Grid218, Grid219, Grid221, Grid222,
 Grid225, Grid226, Grid227, Grid228, Grid229, Grid230, Grid231, Grid232,
 Grid233, Grid234, Grid235, HRAP, NDFD_Oceanic_10K]

#---------------------------------------------------------------------------
#
#  Grid Domain configuration section
#
#---------------------------------------------------------------------------
#
# xdim/ydim:  Defines the dimensions of the grids. (GFE grid size)
#
# origin:  Defines the lower-left corner of the grid (point 0,0) in
#   world coordinates.
#
# extent:  Defines the "size" of the grid in world coordinates.  The upper
#   right corner is the origin+extent.
#
# TimeZone: Defines the timezone used by this site in standard TZ format.
# Refer to /usr/share/zoneinfo/zone.tab for the correct settings.
#
# Projection:  Defines the projection identifier to be used for this domain.

# Note that all parameters for an existing database must use the same
# projection, though not necessarily the same grid size and location.

# These values are set up for AWIPS.  There is a script at the end
# of this section that adjusts the resolution for the RPP sites.

#         [xdim, ydim] / (origin) /( extent)  / TimeZone / Projection / OfficeType
SITES = {
#WFOs
    # Experimental combined AFC site
    'AFC' : ([1057, 449], (1.0, 19.00), (66.0, 28.0), 'America/Anchorage',
              Grid214AK, "wfo"),

    'ABQ' : ([145, 145], (36.00, 22.00), (9.0, 9.0), 'MST7MDT', Grid211,"wfo"),
    'ABR' : ([145, 145], (45.00, 35.00), (9.0, 9.0), 'CST6CDT', Grid211,"wfo"),
    'AER' : ([369, 337], (44.00, 23.00), (23.0, 21.0), 'America/Anchorage',
              Grid214AK, "wfo"),
    'AFG' : ([313, 201], (27.0, 39.0), (39.0, 25.0), 'America/Anchorage',
              Grid214AK, "wfo"),
    'AJK' : ([337, 241], (62.0, 23.0), (21.0, 15.0), 'America/Juneau',
              Grid214AK, "wfo"),
    'AKQ' : ([145, 145], (68.00, 25.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ALU' : ([433, 225], (1.0, 19.0), (54.0, 28.0), 'America/Anchorage',
              Grid214AK, "wfo"),
    'ALY' : ([145, 145], (70.00, 33.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'AMA' : ([145, 145], (41.00, 21.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'APX' : ([145, 145], (58.00, 34.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ARX' : ([145, 145], (52.00, 33.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BGM' : ([145, 145], (68.00, 33.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'BIS' : ([145, 145], (43.00, 37.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BMX' : ([145, 145], (58.00, 19.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BOI' : ([177, 177], (25.00, 34.00), (11.0, 11.0), 'MST7MDT', Grid211, "wfo"),
    'BOU' : ([145, 145], (38.00, 27.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'BOX' : ([193, 193], (74.00, 33.00), (12.0, 12.0), 'EST5EDT', Grid211, "wfo"),
    'BRO' : ([145, 145], (44.00, 10.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BTV' : ([145, 145], (72.00, 36.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'BUF' : ([145, 145], (66.00, 32.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'BYZ' : ([145, 145], (36.00, 37.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'CAE' : ([145, 145], (65.00, 20.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CAR' : ([145, 145], (75.00, 39.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CHS' : ([145, 145], (65.00, 18.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CLE' : ([145, 145], (62.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CRP' : ([145, 145], (45.00, 11.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'CTP' : ([145, 145], (67.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CYS' : ([145, 145], (37.00, 31.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'DDC' : ([145, 145], (43.00, 24.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'DLH' : ([145, 145], (50.00, 37.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'DMX' : ([145, 145], (49.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'DTX' : ([161, 161], (57.00, 34.00), (10.0, 10.0), 'EST5EDT', Grid211, "wfo"),
    'DVN' : ([145, 145], (52.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'EAX' : ([145, 145], (50.00, 27.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'EKA' : ([145, 145], (20.00, 31.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'EPZ' : ([145, 145], (36.00, 16.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'EWX' : ([145, 145], (44.00, 12.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'FFC' : ([145, 145], (61.00, 18.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'FGF' : ([145, 145], (45.00, 39.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'FGZ' : ([145, 145], (29.00, 23.00), (9.0, 9.0), 'US/Arizona', Grid211, "wfo"),
    'FSD' : ([177, 177], (43.00, 32.00), (11.0, 11.0), 'CST6CDT', Grid211, "wfo"),
    'FWD' : ([145, 145], (45.00, 17.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'GGW' : ([145, 145], (36.00, 39.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'GID' : ([145, 145], (44.00, 28.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'GJT' : ([145, 145], (34.00, 27.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'GLD' : ([145, 145], (41.00, 26.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'GRB' : ([145, 145], (54.00, 35.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'GRR' : ([145, 145], (58.00, 33.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'GSP' : ([145, 145], (63.00, 21.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'GUM' : ([433, 225], (15.00, 20.00), (27.0, 14.0), 'Pacific/Guam', Grid204, "wfo"),
    'GYX' : ([145, 145], (75.00, 37.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'HFO' : ([321, 225], (7.00,  11.00), (10.0, 7.0), 'Pacific/Honolulu',
             Grid208, 'wfo'),
    'HGX' : ([145, 145], (48.00, 13.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'HNX' : ([145, 145], (22.00, 24.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'HUN' : ([145, 145], (57.00, 20.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'ICT' : ([145, 145], (45.00, 25.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'ILM' : ([145, 145], (67.00, 21.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ILN' : ([145, 145], (60.00, 27.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ILX' : ([145, 145], (55.00, 27.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'IND' : ([145, 145], (58.00, 27.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'IWX' : ([145, 145], (58.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'JAN' : ([145, 145], (54.00, 18.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'JAX' : ([145, 145], (64.00, 14.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'JKL' : ([145, 145], (61.00, 25.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'KEY' : ([145, 145], (66.00, 8.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'LBF' : ([145, 145], (43.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LCH' : ([145, 145], (52.00, 15.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LIX' : ([145, 145], (54.00, 14.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LKN' : ([145, 145], (25.00, 30.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'LMK' : ([145, 145], (59.00, 25.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'LOT' : ([145, 145], (55.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LOX' : ([145, 145], (21.00, 23.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'LSX' : ([145, 145], (52.00, 25.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LUB' : ([145, 145], (39.00, 17.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LWX' : ([145, 145], (67.00, 27.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'LZK' : ([145, 145], (51.00, 20.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MAF' : ([145, 145], (40.00, 16.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MEG' : ([145, 145], (54.00, 22.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MFL' : ([145, 145], (66.00, 9.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MFR' : ([145, 145], (20.00, 34.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'MHX' : ([145, 145], (68.00, 22.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MKX' : ([145, 145], (55.00, 33.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MLB' : ([145, 145], (66.00, 12.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MOB' : ([145, 145], (57.00, 16.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MPX' : ([145, 145], (50.00, 34.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MQT' : ([145, 145], (56.00, 36.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MRX' : ([145, 145], (61.00, 22.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MSO' : ([145, 145], (29.00, 39.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'MTR' : ([145, 145], (20.00, 26.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'OAX' : ([145, 145], (45.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'OHX' : ([145, 145], (58.00, 22.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'OKX' : ([145, 145], (71.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'OTX' : ([145, 145], (25.00, 40.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'OUN' : ([145, 145], (44.00, 21.00), (9.0, 9.0), 'CST6CDT',  Grid211, "wfo"),
    'PAH' : ([145, 145], (56.00, 24.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'PBZ' : ([145, 145], (65.00, 29.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'PDT' : ([145, 145], (23.00, 38.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'PHI' : ([145, 145], (70.00, 28.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'PIH' : ([145, 145], (30.00, 34.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'PQR' : ([145, 145], (19.00, 38.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'PSR' : ([145, 145], (28.00, 20.00), (9.0, 9.0), 'US/Arizona', Grid211, "wfo"),
    'PUB' : ([145, 145], (38.00, 26.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'RAH' : ([145, 145], (66.00, 22.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'REV' : ([145, 145], (23.00, 29.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'RIW' : ([145, 145], (35.00, 33.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'RLX' : ([145, 145], (63.00, 26.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'RNK' : ([145, 145], (64.00, 24.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'SEW' : ([145, 145], (21.00, 42.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'SGF' : ([145, 145], (51.00, 24.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'SGX' : ([145, 145], (24.00, 21.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'SHV' : ([145, 145], (50.00, 17.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'SJT' : ([145, 145], (43.00, 16.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'SJU': ([32, 28], (10.0, 10.0), (8.0, 7.0), 'America/Puerto_Rico',Grid210, "wfo"),
    'SLC' : ([161, 161], (30.00, 28.00), (10.0, 10.0), 'MST7MDT', Grid211, "wfo"),
    'STO' : ([145, 145], (20.00, 28.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'TAE' : ([145, 145], (60.00, 15.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'TBW' : ([145, 145], (64.00, 11.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'TFX' : ([145, 145], (32.00, 39.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'TOP' : ([145, 145], (47.00, 26.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'TSA' : ([145, 145], (48.00, 22.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'TWC' : ([145, 145], (29.00, 20.00), (9.0, 9.0), 'US/Arizona', Grid211, "wfo"),
    'UNR' : ([145, 145], (40.00, 34.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'VEF' : ([145, 145], (26.00, 25.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
#RFCs
    'ACR' : ([271, 173], (20.0, 21.0), (57.0, 40.0), 'America/Anchorage',
             Grid214, "rfc"),    # this grid is at 10 km resolution
    'ALR' : ([299, 278], (59.0, 11.0), (17.0, 19.0), 'CST6CDT', Grid211, "rfc"),
    'FWR' : ([362, 334], (36.0, 11.0), (20.0, 20.0), 'CST6CDT', Grid211, "rfc"),
    'KRF' : ([408, 356], (33.0, 27.0), (26.0, 22.0), 'CST6CDT', Grid211, "rfc"),
    'MSR' : ([381, 304], (43.0, 28.0), (24.0, 20.0), 'CST6CDT', Grid211, "rfc"),
    'ORN' : ([303, 216], (51.0, 16.0), (18.0, 14.0), 'CST6CDT', Grid211, "rfc"),
    'PTR' : ([218, 308], (21.0, 35.0), (17.0, 19.0), 'PST8PDT', Grid211, "rfc"),
    'RHA' : ([132, 140], (69.0, 28.0), (7.0, 10.0), 'EST5EDT', Grid211, "rfc"),
    'RSA' : ([140, 296], (21.0, 23.0), (12.0, 17.0), 'PST8PDT', Grid211, "rfc"),
    'STR' : ([171, 307], (29.0, 20.0), (13.0, 18.0), 'MST7MDT', Grid211, "rfc"),
    'TAR' : ([226, 164], (69.0, 34.0), (13.0, 13.0), 'EST5EDT', Grid211, "rfc"),
    'TIR' : ([220, 171], (59.0, 25.0), (13.0, 12.0), 'EST5EDT', Grid211, "rfc"),
    'TUA' : ([281, 168], (39.0, 22.0), (18.0, 10.0), 'CST6CDT', Grid211, "rfc"),

#Special Sites - Updated NHC and OPC domains in OB9.3
    'US' : ([267, 159], (18.0, 9.5), (67.0, 40.0), 'EDT5EDT', Grid211, "other"),
    'FSL' : ([161, 145], (38.50, 27.00), (10.0, 9.0), 'MST7MDT', Grid211, "other"),
#    'NH1' : ([667, 461], (69.5, 4.5), (52.03125, 35.9375), 'EST5EDT', Grid204, "wfo"),
#    'NH2' : ([950, 289], (-33.0, -7.0), (148.276, 45.0), 'EST5EDT', Grid210, "wfo"),
    'NH1' = ([838, 577], (887.0, 121.0), (837.0, 576.0), 'EST5EDT', NDFD_Oceanic_10K, "wfo") 
    'NH2' = ([1188, 363], (1328.0, 365.0), (1187.0, 362.0), 'EST5EDT', NDFD_Oceanic_10K, "wfo") 
    'ONA' : ([244, 383], (68.9375, 19.5625), (15.1875, 23.875), 'EST5EDT', Grid211, "wfo"),
    'ONP' : ([396, 415], (8.1875, 21.5625), (24.6875, 25.875), 'PST8PDT', Grid211, "wfo"),

#Ice Desk for AFC
    'AICE' : ([560, 340], (9.0, 11.0), (29.0, 19.0), 'America/Anchorage',
       Grid203, "nc"),
#Nested for GUM (future)
    'GUMa': ([193, 193], (23.0, 26.0), (3.0, 3.0), 'Pacific/Guam', Grid204, "other"),
#Regional Offices
    'VUY' : ([337,449], (62.00, 19.00), (21.0, 28.0), 'EST5EDT', Grid211, "ro"),
    'BCQ' : ([145,145], (50.00, 27.00), (9.0, 9.0), 'CST6CDT', Grid211, "ro"),
    'EHU' : ([361,361], (27.00, 10.00), (45.0, 20.0), 'CST6CDT', Grid211, "ro"),
    'VHW' : ([161,161], (30.00, 28.00), (10.0, 10.0), 'MST7MDT', Grid211, "ro"),
    'PBP' : ([321,225], (7.00, 11.00), (10.0, 7.0), 'Pacific/Honolulu', Grid208, "ro"),
    'ARE' : ([369,337], (44.00, 23.00), (23.0, 21.0), 'America/Anchorage', Grid214AK, "ro"),
    'ARW' : ([433,225], (1.00, 19.00), (54.0, 21.0), 'America/Anchorage', Grid214AK, "ro"),
    
#National Centers
    'HAK' : ( [825,553], ( 1.0, 1.0), (103.0, 69.0), 'EST5EDT', Grid214AK, "nc"),
    'HUS' : ([1073,689], (19.0, 8.0), ( 67.0, 43.0), 'EST5EDT', Grid211,   "nc"),                
}


#---------------------------------------------------------------------------
#
#  Time Constraint configuration section
#
#---------------------------------------------------------------------------
HOUR = 3600
DAY  = 24 * HOUR

# Start: is the number of seconds since 0000z for the first grid of the day
# Repeat: is the number of seconds from start until the next grid starts
# Duration: is the length of the grid in number of seconds

# Examples of constraints:
# Hourly temperatures
#     HrTemp = (0, HOUR, HOUR)
# QPF that is 6 hours long, aligned on 0000z, exists for every 6 hours
#     Q = (0, HOUR*6, HOUR*6)
#

# fixed time constraints: start / repeat / duration
TC_1M    = (0, 60, 60) # 1 minute
TC1      = (0, HOUR, HOUR)
TC3      = (0, 3 * HOUR, HOUR)
TC6      = (0, 6 * HOUR, HOUR)
TC12     = (0, 12 * HOUR, HOUR)
TC3NG    = (0, 3 * HOUR, 3 * HOUR)
TC6NG    = (0, 6 * HOUR, 6 * HOUR)
TC12NG   = (0, 12 * HOUR, 12 * HOUR)
TC061212 = (6 * HOUR, 12 * HOUR, 12 * HOUR)
Persistent = (0, 0, 0)     # special time constraint

# Local-time based time constraints.  Does not automatically account for
# daylight savings time.  The dst flag is 0 for standard time and manually
# set to 1 for daylight time (if desired).  The start is specified in
# seconds local time, e.g., 6*HOUR would indicate 6am.
def localTC(start,repeat,duration,dst):
    timezone = SITES[GFESUITE_SITEID][3]
    import pytz
    tz = pytz.timezone(timezone)
    delta = tz.utcoffset(0) - tz.dst(0);
    offset = delta.days*86400 + delta.seconds
    start = start - offset
    if dst == 1:
        start = start - 3600     #daylight savings flag
    if start >= 3600 * 24:
        start = start - 3600 * 24
    elif start < 0:
        start = start + 3600 * 24
    return (start, repeat, duration)

# The following time constraints are based on local standard time.
# Change the last parameter from 0 to 1 to force daylight savings time
# always.
# PWS TCs changed in OB9.3 for new 6 hour data from NHC
MaxTTC     = localTC(7*HOUR, 24*HOUR, 13*HOUR, 0)
MinTTC     = localTC(19*HOUR, 24*HOUR, 14*HOUR, 0)
MaxRHTC    = localTC(15*HOUR, 24*HOUR, 18*HOUR, 0)
MinRHTC    = localTC(3*HOUR, 24*HOUR, 18*HOUR, 0)
LT3NG      = localTC(0*HOUR, 3*HOUR, 3*HOUR, 0)
LT6NG      = localTC(0*HOUR, 6*HOUR, 6*HOUR, 0)
LT12NG     = localTC(6*HOUR, 12*HOUR, 12*HOUR, 0)
LTMOS      = localTC(6*HOUR, 12*HOUR, 12*HOUR, 0)  #special MOS local time
MaxTTCMOS  = localTC(6*HOUR, 24*HOUR, 12*HOUR, 0)  #special MOS maxT
MinTTCMOS  = localTC(18*HOUR, 24*HOUR, 12*HOUR, 0)  #special MOS minT
LT24       = localTC(0*HOUR, 24*HOUR, 24*HOUR, 0)
FireWx1300TC = localTC(13*HOUR, 24*HOUR, 1*HOUR, 0)   #special FireWx 1pm snap
#DR3511 DeltaMaxTTC  = localTC(7*HOUR, 24*HOUR, 16*HOUR, 0)  # just for HPCdeltaMaxT
PWSDTC     = localTC(11*HOUR, 24*HOUR, 12*HOUR, 0)
PWSNTC     = localTC(23*HOUR, 24*HOUR, 12*HOUR, 0)

#---------------------------------------------------------------------------
#
#  Database/(Model) Attribute Configuration
#
#---------------------------------------------------------------------------
#
# name:  The model name of the database
#
# format:  Either 'GRID' or 'DFM'
#
# type:  Optional type of the database
#
# single:  YES or NO. YES if this database always exists and is not
#   based on model-times.  NO if this database is created/destroyed and
#   is based on model-runs.  When created, the names of these databases have
#   time stamps.
#
# official:  YES or NO.  YES if this is an official database from which
#   products can be generated.  NO if this is a conventional database.
#
# numVer:  Number of versions of this database to retain.
#
# purgeAge: Number of hours in the past before grids will be automatically
#   purged from the database.  If 0, then purging is disabled.
#

YES = 1
NO = 0
GRID = 'GRID'
# name /  format / type / single / official / numVer / purgeAge

Fcst        = ('Fcst',         GRID,   '', YES, NO,  1, 24)
Practice    = ('Fcst',         GRID,   'Prac', YES, NO,  1, 24)
TestFcst    = ('Fcst',         GRID,   'Test', YES, NO,  1, 24)
Restore     = ('Restore',      GRID,   '', YES, NO,  1, 24)
Test        = ('Test',         GRID,   'test', NO, NO,  1, 0)
Official    = ('Official',     GRID,   '', YES, YES, 1, 24)
ISC         = ('ISC',          GRID,   '', YES, NO,  1, 12)
LAPS        = ('LAPS',         GRID,   '', YES, NO,  1, 30)
SAT         = ('SAT',          GRID,   '', YES, NO,  1, 12)
HPCGuide    = ('HPCGuide',     GRID,   '', NO,  NO,  2, 0)
NAM12       = ('NAM12',        GRID,   '', NO,  NO,  2, 0)
NAM40       = ('NAM40',        GRID,   '', NO,  NO,  2, 0)
NAM80       = ('NAM80',        GRID,   '', NO,  NO,  2, 0)
NAM95       = ('NAM95',        GRID,   '', NO,  NO,  2, 0)
NGM80       = ('NGM80',        GRID,   '', NO,  NO,  2, 0)
NGM95       = ('NGM95',        GRID,   '', NO,  NO,  2, 0)
GFS40       = ('GFS40',        GRID,   '', NO,  NO,  2, 0)
GFS80       = ('GFS80',        GRID,   '', NO,  NO,  2, 0)
GFS190      = ('GFS190',       GRID,   '', NO,  NO,  2, 0)
GFS75       = ('GFS75',        GRID,   '', NO,  NO,  2, 0)
gfsLR       = ('gfsLR',        GRID,   '', NO,  NO,  2, 0)
RUC13       = ('RUC13',        GRID,   '', NO,  NO,  2, 0)
RUC80       = ('RUC80',        GRID,   '', NO,  NO,  2, 0)
HPCGrid     = ('HPCGRID',      GRID,   '', NO,  NO,  2, 0)
AKwave10    = ('AKwave10',     GRID,   '', NO,  NO,  2, 0)
AKwave4     = ('AKwave4',      GRID,   '', NO,  NO,  2, 0)
EPwave10    = ('EPwave10',     GRID,   '', NO,  NO,  2, 0)
GlobalWave  = ('GlobalWave',   GRID,   '', NO,  NO,  2, 0)
GLWM        = ('GLWM',         GRID,   '', NO,  NO,  2, 0)##########DCS3499
HIRESWarw   = ('HIRESWarw',    GRID,   '', NO,  NO,  2, 0)##########DCS3501
HIRESWnmm   = ('HIRESWnmm',    GRID,   '', NO,  NO,  2, 0)
#### SPC         = ('SPC',          GRID,   '', NO,  NO,  2, 0)###DR20634
WCwave10    = ('WCwave10',     GRID,   '', NO,  NO,  2, 0)
WCwave4     = ('WCwave4',      GRID,   '', NO,  NO,  2, 0)
WNAwave10   = ('WNAwave10',    GRID,   '', NO,  NO,  2, 0)
WNAwave4    = ('WNAwave4',     GRID,   '', NO,  NO,  2, 0)
GWW         = ('GWW',          GRID,   '', NO,  NO,  2, 0)
HPCQPF      = ('HPCQPF',       GRID,   '', NO,   NO,  4, 0)
RFCQPF      = ('RFCQPF',       GRID,   '', NO,   NO,  4, 0)
#DR3511 HPCDelta    = ('HPCdelta',     GRID,   '', NO,   NO,  2, 0)
TPCTCM      = ('TPCtcm',       GRID,   '', NO,   NO,  2, 0)
MSAS        = ('MSAS',         GRID,   '', YES,  NO,  1, 36)
GLERL       = ('GLERL',        GRID,   '', NO,   NO,  2, 0)
AKWAVE      = ('AKWAVE',       GRID,   '', NO,   NO,  2, 0)
WNAWAVE     = ('WNAWAVE',      GRID,   '', NO,   NO,  2, 0)
DGEX        = ('DGEX',         GRID,   '', NO,   NO,  2, 0)
OPCTAFBE    = ('OPCTAFBE',     GRID,   '', NO,   NO,  2, 0)
OPCTAFBNW   = ('OPCTAFBNW',    GRID,   '', NO,   NO,  2, 0)
OPCTAFBSW   = ('OPCTAFBSW',    GRID,   '', NO,   NO,  2, 0)
MOSGuide    = ('MOSGuide',     GRID,   '', NO,   NO,  2, 0)
RTMA        = ('RTMA',         GRID,   '', YES,  NO,  1, 36)
NamDNG5     = ('NamDNG5',      GRID,   '', NO,   NO,  2, 0)
TPCProb     = ('TPCProb',      GRID,   '', NO,   NO, 30, 0)
SREF        = ('SREF',         GRID,   '', NO,   NO,  3, 0)
ENPwave     = ('ENPwave',      GRID,   '', NO,   NO,  2, 0)
#---------------------------------------------------------------------------
#
#  D2D Model Database Version Specification
#
#---------------------------------------------------------------------------
# D2D database retention values - defaults to 2 if not specified
# Dictionary format. Also used for the number of versions of satellite
# images that are seen.
D2DDBVERSIONS = {
      "MSAS": 6,
      "LAPS": 6,
      "Satellite": 6,
      "HPCERP": 5,
      "TPCProb": 30,
      "CRMTopo": 1,
      "NED": 1,
      }

#---------------------------------------------------------------------------
#
#  Search path for D2D (awips) model files.
#
#---------------------------------------------------------------------------
# Alaska OCONUS
if SID in ALASKA_SITES:
    D2DMODELS = [('mesoEta216', 'NAM40'),
                 ('mesoEta217', 'NAM20'),
                 ('AVN203', 'GFS190'),
                 ('MRF203', 'gfsLR'),
                 ('NGM207', 'NGM95'),
                 ('ETA207', 'NAM95'),
                 'GWW233',
                 ('ETA242', 'NAM12'),
                 'ECMWF-LowRes','ECMWF',
                 'UKMET-NorthernHemisphere', 'UKMET',
                 'ENSEMBLE',
                 ('DGEX186', 'DGEX'),
                 ('OPCWave181', 'OPCTAFBNW'),
                 ('AKWAVE239', 'AKWAVE'),
                 'AKwave10',
                 'AKwave4',
                 'GlobalWave',
                 ('AK-RTMA','RTMA'),
                 ('AK-NamDNG5','NamDNG5'),
                 ('MOSGuide-AK', 'MOSGuide'),
                 ('HiResW-ARW-AK', 'HIRESWarw'),
                 ('HiResW-NMM-AK', 'HIRESWnmm'),
                 ('SPCGuide', 'SPC'),
                 ('TPCWindProb', 'TPCProb'),
               ]

# Hawaii OCONUS
elif SID == "HFO":
    D2DMODELS = [('MRF204', 'gfsLR'),
                 ('AVN225', 'GFS75'),
                 'GWW233',
                 'GlobalWave',
                 'EPwave10',
                 'EPwave4',
                 ('HI-RTMA','RTMA'),
                 ('HI-NamDNG5','NamDNG5'),
                 ('HiResW-ARW-HI', 'HIRESWarw'),
                 ('HiResW-NMM-HI', 'HIRESWnmm'),
                 ('SPCGuide', 'SPC'),
                 ('TPCWindProb', 'TPCProb'),
                 ('ECMWF-HiRes','ECMWFHiRes'),
               ]

# San Juan OCONUS
elif SID == "SJU":
    D2DMODELS = [('AVN211', 'GFS80'),
                 ('GFS212', 'GFS40'),
                 ('ETA', 'NAM80'),
                 ('NGM', 'NGM80'),
                 ('MRF205', 'gfsLR'),
                 ('OPCWave180', 'OPCTAFBE'),
                 'HurWind226',
                 'GWW233',
                 'GlobalWave',
                 'WNAwave10',
                 'WNAwave4',
                 ('PR-RTMA','RTMA'),
                 ('PR-NamDNG5','NamDNG5'),
                 ('HiResW-ARW-SJU', 'HIRESWarw'),
                 ('HiResW-NMM-SJU', 'HIRESWnmm'),
                 ('SPCGuide', 'SPC'),
                 ('TPCWindProb', 'TPCProb'),
                 ('ECMWF-HiRes','ECMWFHiRes'),
               ]

# Guam OCONUS
elif SID == "GUM":
    D2DMODELS = [('MRF204', 'gfsLR'),
                 ('AVN225', 'GFS75'),
                 'GWW233',
                 'GlobalWave',
                 ('TPCWindProb', 'TPCProb'),
               ]

#CONUS sites
elif SID in CONUS_EAST_SITES:
    D2DMODELS = [('GFS212', 'GFS40'),
                 ('AVN211', 'GFS80'),
                 ('ETA', 'NAM80'),
                 ('NGM', 'NGM80'),
                 ('MRF', 'gfsLR'),
                 ('RUC130', 'RUC13'),
                 ('RUC', 'RUC80'),
                 ('mesoEta212', 'NAM40'),
                 ('mesoEta215', 'NAM20'),
                 'MSAS',
                 ('LAPS', 'LAPS'),
                 'GWW233',
                 ('HPCqpf', 'HPCQPF'),
                 ('HPCqpfNDFD', 'HPCERP'),
                 ('RFCqpf', 'RFCQPF'),
#DR3511                 'HPCdelta',
                 'GLERL',
                 'WNAWAVE238',
                 ('TPCSurgeProb','TPCStormSurge'), # DCS3462
                 'GlobalWave',
                 'EPwave10',
                 'AKwave10',
                 'AKwave4',
                 'WCwave10',
                 'WCwave4',
                 'WNAwave10',
                 'WNAwave4',
                 'HurWind226',
                 ('DGEX185', 'DGEX'),
                 ('ETA218', 'NAM12'),
                 'HPCGuide',
                 ('OPCWave180', 'OPCTAFBE'),
                 ('OPCWave181', 'OPCTAFBNW'),
                 ('OPCWave182', 'OPCTAFBSW'),
                 'MOSGuide',
                 'RTMA',
                 'NamDNG5',
                 ('TPCWindProb','TPCProb'),
                 ('SREF212', 'SREF'),
               #############DCS3501
                 ('HiResW-ARW-East', 'HIRESWarw'),
                 ('HiResW-NMM-East', 'HIRESWnmm'),
                 ('SPCGuide', 'SPC'),
                 ('ECMWF-HiRes','ECMWFHiRes'),
		 ('ENPWAVE253', 'ENPwave'),
               ]

else:   #######DCS3501 WEST_CONUS

    D2DMODELS = [('GFS212', 'GFS40'),
                 ('AVN211', 'GFS80'),
                 ('ETA', 'NAM80'),
                 ('NGM', 'NGM80'),
                 ('MRF', 'gfsLR'),
                 ('RUC130', 'RUC13'),
                 ('RUC', 'RUC80'),
                 ('mesoEta212', 'NAM40'),
                 ('mesoEta215', 'NAM20'),
                 'MSAS',
                 ('LAPS', 'LAPS'),
                 'GWW233',
                 ('HPCqpf', 'HPCQPF'),
                 ('HPCqpfNDFD', 'HPCERP'),
                 ('RFCqpf', 'RFCQPF'),
#DR3511                 'HPCdelta',
                 'GLERL',
                 'WNAWAVE238',
                 ('TPCSurgeProb','TPCStormSurge'), # DCS3462
                 'GlobalWave',
                 'EPwave10',
                 'WCwave10',
                 'WCwave4',
                 'WNAwave10',
                 'WNAwave4',
                 'AKwave10',
                 'AKwave4',
                 'AKWAVE',
                 'HurWind226',
                 ('DGEX185', 'DGEX'),
                 ('ETA218', 'NAM12'),
                 'HPCGuide',
                 ('OPCWave180', 'OPCTAFBE'),
                 ('OPCWave181', 'OPCTAFBNW'),
                 ('OPCWave182', 'OPCTAFBSW'),
                 'MOSGuide',
                 'RTMA',
                 'NamDNG5',
                 ('TPCWindProb','TPCProb'),
                 ('SREF212', 'SREF'),
               #############DCS3501
                 ('HiResW-ARW-West', 'HIRESWarw'),
                 ('HiResW-NMM-West', 'HIRESWnmm'),
                 ('SPCGuide', 'SPC'),
                 ('ECMWF-HiRes','ECMWFHiRes'),
		 ('ENPWAVE253', 'ENPwave'),
               ]

if SID in GreatLake_SITES:
    D2DMODELS.append(('GRLKwave', 'GLWM'))

#---------------------------------------------------------------------------
#
#  Search path for netCDF data files. 
#  NOTE: This feature was implemented only backward compatibility with existing A1 datasets.
#        New datasets should be generated in a from that can be ingested by A2
#        It shoudl only be used for static datasets. 
#        New files will not be recognized without a server restart. 
#
#---------------------------------------------------------------------------
# Alaska OCONUS
if SID in ALASKA_SITES:
    NETCDFDIRS = []
    
# Hawaii OCONUS
elif SID == "HFO":
    NETCDFDIRS = [('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ]

# San Juan OCONUS
elif SID == "SJU":
    NETCDFDIRS = [('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ]
    

# Guam OCONUS
elif SID == "GUM":
    NETCDFDIRS = []
    
#CONUS sites
elif SID in CONUS_EAST_SITES:
    NETCDFDIRS = [('/awips2/edex/data/gfe/climo/PRISM'),
                  ('/awips2/edex/data/gfe/climo/NCDC'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ]
        

else:   #######DCS3501 WEST_CONUS
    NETCDFDIRS = [('/awips2/edex/data/gfe/climo/PRISM'),
                  ('/awips2/edex/data/gfe/climo/NCDC'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ]
    

#---------------------------------------------------------------------------
#
# Where to find (and what to call) satellite data.
#
#---------------------------------------------------------------------------
#

# This table contains directory names and weather element names.

# Alaska OCONUS
if SID in ALASKA_SITES:
    SATDATA = []

# Hawaii OCONUS
elif SID == "HFO":
    SATDATA = []

# San Juan OCONUS
elif SID == "SJU":
    SATDATA = [("NESDIS/GOES-13(N)/East CONUS/Imager Visible", "visibleEast"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 11 micron IR", "ir11East"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 12 micron IR", "ir13East"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 3.9 micron IR", "ir39East"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 6.7-6.5 micron IR (WV)", "waterVaporEast")]

# Guam OCONUS
elif SID == "GUM":
    SATDATA = []

#CONUS sites
else:
    SATDATA = [("NESDIS/GOES-11(L)/West CONUS/Imager Visible", "visibleWest"),
               ("NESDIS/GOES-11(L)/West CONUS/Imager 11 micron IR", "ir11West"),
               ("NESDIS/GOES-11(L)/West CONUS/Imager 12 micron IR", "ir13West"),
               ("NESDIS/GOES-11(L)/West CONUS/Imager 3.9 micron IR", "ir39West"),
               ("NESDIS/GOES-11(L)/West CONUS/Imager 6.7-6.5 micron IR (WV)", "waterVaporWest"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager Visible", "visibleEast"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 11 micron IR", "ir11East"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 12 micron IR", "ir13East"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 3.9 micron IR", "ir39East"),
               ("NESDIS/GOES-13(N)/East CONUS/Imager 6.7-6.5 micron IR (WV)", "waterVaporEast")]

#---------------------------------------------------------------------------
#
#  Smart Initialization Configuration
#
#---------------------------------------------------------------------------
#
# RFCs
if SID in RFC_SITES:
    INITMODULES = {}    #disable smart inits for RFCs

# Alaska OCONUS
elif SID in ALASKA_SITES:
    INITMODULES = {
#        "AKNAM40" : ["NAM40", "NAM20"],
        "AKWAVE" : ['AKWAVE'],
#        "AKwave4" : ['AKwave4'],
#        "AKwave10" : ['AKwave10'],
#        "GlobalWave" : ["GlobalWave"],
#        "NAM95" : ["NAM95"],
#        "NGM95" : ["NGM95"],
#        "gfsLR" : ["gfsLR"],
        "NAM12" : ["NAM12"],
        "GFS80" : ["GFS80"],
#        "GFS190" : ["GFS190"],
#DCS3501 
        "HIRESWarw" : ["HIRESWarw"],                          
        "HIRESWnmm" : ["HIRESWnmm"],
#DR20634        "SPC" : ["SPC"],
        "LAPS" : ["LAPS"],
        "HPCQPF" : ['HPCQPF'],
        "RFCQPF" : ['RFCQPF'],
        "MSAS" : ['MSAS'],
#        "HPCdelta" : ['HPCdelta'],
        "SAT" : ['Satellite'],
        "DGEX" : ['DGEX'],
#        "GWW" : ["GWW"],
#        "OPCTAFBNW" : ['OPCTAFBNW'],
        "RTMA": ['RTMA'],
        "NamDNG5" : ["NamDNG5"],
        "AKMOSGuide" : ['MOSGuide'],
        }

# Hawaii OCONUS
elif SID == "HFO":
    INITMODULES= {
        "GFS75" : ["GFS75"],
#####DCS3501          
        "HIRESWarw" : ["HIRESWarw"],
        "HIRESWnmm" : ["HIRESWnmm"],
##DR20634        "SPC" : ["SPC"],
#        "GlobalWave" : ["GlobalWave"],
#        "EPwave10" : ["EPwEave10"],
#        "GWW" : ["GWW"],
#        "gfsLR" : ["gfsLR"],
        "RTMA": ['RTMA'],
        "NamDNG5" : ["NamDNG5"],
        }

# San Juan OCONUS
elif SID == "SJU":
    #initialization  module to model mappings
    INITMODULES = {
#        "NAM40" : ["NAM40", "NAM20"],
#        "NAM80" : ["NAM80"],
        "RUC13" : ["RUC13"],
        "RUC80" : ["RUC80"],
#        "gfsLR" : ["gfsLR"],
#        "NGM80" : ["NGM80"],
        "NAM12" : ["NAM12"],
        "GFS80" : ["GFS80"],
        "GFS40" : ["GFS40"],
#####DCS3501 
        "HIRESWarw" : ["HIRESWarw"],
        "HIRESWnmm" : ["HIRESWnmm"], 
#DR20634        "SPC" : ["SPC"],
        "LAPS" : ["LAPS"],
        "HPCQPF" : ['HPCQPF'],
        "RFCQPF" : ['RFCQPF'],
        "MSAS" : ['MSAS'],
#        "HPCdelta" : ['HPCdelta'],
        "SAT" : ['Satellite'],
#        "GWW" : ["GWW"],
#        "OPCTAFBE" : ['OPCTAFBE'],
#        "GlobalWave" : ["GlobalWave"],
#        "EPwave10" : ["EPwEave10"],
        "RTMA": ['RTMA'],
        "NamDNG5" : ["NamDNG5"],
        }

# Guam OCONUS
elif SID == "GUM":
    INITMODULES= {
        "GFS75" : ["GFS75"],
#        "GWW" : ["GWW"],
#        "gfsLR" : ["gfsLR"],
#        "GlobalWave" : ["GlobalWave"],
        "RTMA": ['RTMA'],
        }

#CONUS sites
else:
    #initialization  module to model mappings
    INITMODULES = {
        "RUC13" : ["RUC13"],
        "RUC80" : ["RUC80"],
        "NAM12" : ["NAM12"],
        "GFS40" : ["GFS40"],
        "GFS80" : ["GFS80"],
        "LAPS" : ["LAPS"],
        "HPCQPF" : ['HPCQPF'],
        "RFCQPF" : ['RFCQPF'],
        "MSAS" : ['MSAS'],
        "SAT" : ['Satellite'],
        "DGEX" : ['DGEX'],
        "MOSGuide" : ['MOSGuide'],
        "HPCGuide" : ['HPCGuide'],
        "RTMA": ['RTMA'],
        "NamDNG5" : ["NamDNG5"],
        "SREF" : ["SREF"],
#########DCS3501
        "GLWM" : ["GLWM"],
        "HIRESWarw" : ["HIRESWarw"],
        "HIRESWnmm" : ["HIRESWnmm"],
#DR20634        "SPC" : ["SPC"],
#        "GlobalWave" : ["GlobalWave"],
#        "EPwave10" : ["EPwave10"],
#        "WCwave10" : ["WCwave10"],
#        "WCwave4" : ["WCwave4"],
#        "WNAwave10" : ["WNAwave10"],
#        "WNAwave4" : ["WNAwave4"],
#	 "ENPwave": ["ENPwave"],
        }

#initialization skip certain model runs
INITSKIPS = {
    "RUC13" : [1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23],
    "RUC80" : [1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23]
    }

#---------------------------------------------------------------------------
#
#  D2D Accumulative Weather Elements
#
#---------------------------------------------------------------------------
# This is a listing of D2D model name, and weather elements.  The
# weather elements defined in this list are treated as accumulative
# elements, thus the snapshot time of the grid is converted to be a
# grid with timestep duration, starting the previous model timestep and
# going up to but not including the snapshot time.
D2DAccumulativeElements= {
    "GFS40": ["tp", "cp", "crain", "csnow", "cfrzr", "cicep"],
    "GFS80": ["tp", "cp"],
    "GFS75": ["tp", "cp"],
    "GFS190": ["tp", "cp"],
    "NAM95": ["tp", "cp"],
    "NAM80": ["tp", "cp"],
    "NAM40": ["tp", "cp"],
    "NAM20": ["tp", "cp"],
    "NAM12": ["tp", "cp", "crain", "csnow", "cfrzr", "cicep"],
    "NGM80": ["tp", "cp"],
    "NGM95": ["tp", "cp"],
    "gfsLR": ["tp", "cp"],
    "RUC13": ["tp", "cp"],
    "RUC80": ["tp", "cp"],
    "MSAS": ["tp", "cp"],
    "LAPS": ["pc"],
    "DGEX": ["tp"],
    "HPCQPF": ["tpHPC"],
    "RFCQPF": ["tpHPC"],
#DR3511    "HPCdelta": ["pop", "tcc"],
    "HPCGuide": ["pop"],
    'MOSGuide': ['pop12hr', 'pop6hr', 'thp12hr', 'thp3hr', 'thp6hr', 'tcc', 'tp6hr', 'tp12hr', 'wgs'],
#############DCS3501
    "HIRESWarw": ["tp"],
    "HIRESWnmm": ["tp"],
#DR20634    "SPC": ["tp"],

    #Dummy ones for the transition from Eta to NAM.  These are ignored.
    # These will be removed after OB7.1.
    #"Eta95": [],
    #"Eta80": [],
    #"Eta40": [],
    #"Eta20": [],
    #"Eta12": [],

    }

#---------------------------------------------------------------------------
#
#  Intersite Coordination Configurations
#
#---------------------------------------------------------------------------
# base urls for the ISC Routing Table
ISC_ROUTING_TABLE_ADDRESS = {
    "ANCF" : "http://165.92.30.69:8080/irt",
    "BNCF" : "http://165.92.180.25:8080/irt"
    }


# list of sites that from which you want ISC data (If None, ifpServer will
# automatically calculate the list.)  Should always include your own site.
REQUESTED_ISC_SITES = None

# Overall ISC request flag.  Must be set to 1 in order to request and receive
# ISC data.  Must be 1 to register with the IRT.
REQUEST_ISC = 0

# Sending control flag.  Set to 1 to send isc when data is saved.
SEND_ISC_ON_SAVE = 0

# Sending control flag.  Set to 1 to send isc when data is published.
SEND_ISC_ON_PUBLISH = 0

# List of weather elements to request for ISC.  If set to None, it defaults
# to the list of all weather elements in the Fcst database.
REQUESTED_ISC_PARMS = None

# Transmission script for sending data.  This is the script that iscExtract
# and other routines (e.g., vtec table sharing) will call to perform the
# actual transmission of data.
TRANSMIT_SCRIPT = GFESUITE_HOME + '/bin/gfe_msg_send -s %SUBJECT -a %ADDRESSES -i %WMOID -c 11 -p 0 -e %ATTACHMENTS'


# Extra ISC parms (weather elements).  These are a list of the baseline
# weather elements to be added as extra parms to the ISC database.  This
# is necessary when receiving ISC grids from a site that is a different
# office type than your own.  You never need to add weather elements
# to the ISC database that is your own office type.  The format of this
# entry is a list of tuples.  The tuple is a list of weather elements
# objects (such as Temp and not "T"), and an office type, such as "rfc".
EXTRA_ISC_PARMS = [([QPF], 'rfc'), ([QPF], 'wfo')]

#---------------------------------------------------------------------------
#
#  Misc. Configurations
#
#---------------------------------------------------------------------------
# defines the number of days to keep log files
LOG_FILE_PURGE_AFTER = 28

# auto configure NotifyTextProd -- set after OB6
AUTO_CONFIGURE_NOTIFYTEXTPROD = 1   #0=off,1=on


#-----------------------------------
# DO NOT CHANGE THE FOLLOWING SECTION
#------------------------------------
# import the local config file
localParms = localNAM12Parms = localRUC13Parms = localRUC80Parms = localNGM80Parms = []
localGFS80Parms = localgfsLRParms = localNAM40Parms = localDBs = []
localOPCWavEParms = localOPCWavNWParms = localOPCWavSWParms = []
localMOSGuideParms = localGFS40Parms = []
localNAM80Parms = localLAPSParms = localISCParms = localGWWParms = []
localNAM95Parms = localNGM95Parms = localDGEXParms = []
localMSASParms = localGLERLParms = localWNAWAVEParms = localAKWAVEParms = []
localMOSParms = localHPCQPFParms = localRFCQPFParms = []
#DR3511 localHPCDeltaParms = []
localTCMParms = localSATParms = localGFS75Parms = localGFS190Parms = []
localAKwave10Parms = localAKwave4Parms = localEPwave10Parms = localGlobalWaveParms = []
localWCwave10Parms = localWCwave4Parms = localWNAwave10Parms = localWNAwave4Parms = []
localENPwaveParms = []
localGLWMParms = []     #####DCS3499
localParms = localHIRESWarwParms = localHIRESWnmmParms = []    #######DCS3501
#DR20634 localParms = localSPCParms = []
localHPCGuideParms = []
localRTMAParms = []
localNamDNG5Parms = []
localSREFParms = []
localTPCProbParms = []
localISCExtraParms = []

myOfficeType = SITES[GFESUITE_SITEID][5]

if not BASELINE and siteImport('localConfig'):
    #ensure office type is set properly in localConfig SITES[]
    if len(SITES[GFESUITE_SITEID]) == 5:
        a = list(SITES[GFESUITE_SITEID])
        a.append(myOfficeType)
        SITES[GFESUITE_SITEID] = tuple(a)
    else:
        myOfficeType = SITES[GFESUITE_SITEID]  #probably from localConfig

    localParms = getattr(localConfig, 'parms', localParms)
    localNAM12Parms = getattr(localConfig, 'parmsNAM12', localNAM12Parms)
    localOPCWavEParms = getattr(localConfig, 'parmsOPCWavE', localOPCWavEParms)
    localOPCWavSWParms = getattr(localConfig, 'parmsOPCWavSW',
                                 localOPCWavSWParms)
    localOPCWavNWParms = getattr(localConfig, 'parmsOPCWavNW',
                                 localOPCWavNWParms)
    localNAM40Parms = getattr(localConfig, 'parmsNAM40', localNAM40Parms)
    localNAM80Parms = getattr(localConfig, 'parmsNAM80', localNAM80Parms)
    localNAM95Parms = getattr(localConfig, 'parmsNAM95', localNAM95Parms)
    localRUC13Parms = getattr(localConfig, 'parmsRUC13', localRUC13Parms)
    localRUC80Parms = getattr(localConfig, 'parmsRUC80', localRUC80Parms)
    localNGM80Parms = getattr(localConfig, 'parmsNGM80', localNGM80Parms)
    localNGM95Parms = getattr(localConfig, 'parmsNGM95', localNGM95Parms)
    localGFS40Parms = getattr(localConfig, 'parmsGFS40', localGFS40Parms)
    localGFS80Parms = getattr(localConfig, 'parmsGFS80', localGFS80Parms)
    localGFS190Parms = getattr(localConfig, 'parmsGFS190', localGFS190Parms)
    localGFS75Parms = getattr(localConfig, 'parmsGFS75', localGFS75Parms)
    localgfsLRParms = getattr(localConfig, 'parmsgfsLR', localgfsLRParms)
    localISCParms = getattr(localConfig, 'parmsISC', localISCParms)
    localGWWParms = getattr(localConfig, 'parmsGWW', localGWWParms)
    localGLWMParms = getattr(localConfig, 'parmsGLWM', localGLWMParms)        #########DCS3499
    localHIRESWarwParms = getattr(localConfig, 'parmsHIRESWarw', localHIRESWarwParms)   ########DCS3501
    localHIRESWnmmParms = getattr(localConfig, 'parmsHIRESWnmm', localHIRESWnmmParms)
#DR20634    localSPCParms = getattr(localConfig, 'parmsSPC', localSPCParms)
    localWNAWAVEParms = getattr(localConfig, 'parmsWNAWAVE', localWNAWAVEParms)
    localAKWAVEParms = getattr(localConfig, 'parmsAKWAVE', localAKWAVEParms)
    localLAPSParms = getattr(localConfig, 'parmsLAPS', localLAPSParms)
    localMOSGuideParms = getattr(localConfig, 'parmsMOSGuide',
                                 localMOSGuideParms)
    localSREFParms = getattr(localConfig, 'parmsSREF', localSREFParms)
    localSATParms = getattr(localConfig, 'parmsSAT', localSATParms)
    localMSASParms = getattr(localConfig, 'parmsMSAS', localMSASParms)
    localGLERLParms = getattr(localConfig, 'parmsGLERL', localGLERLParms)
    localDBs = getattr(localConfig, 'dbs', localDBs)
    localMOSParms = getattr(localConfig, 'parmsMOS', localMOSParms)
    localHPCQPFParms = getattr(localConfig, 'parmsHPCQPF', localHPCQPFParms)
    localRFCQPFParms = getattr(localConfig, 'parmsRFCQPF', localRFCQPFParms)
#DR3511    localHPCDeltaParms = getattr(localConfig, 'parmsHPCDelta',
#DR3511                                 localHPCDeltaParms)
    localTCMParms = getattr(localConfig, 'parmsTCM', localTCMParms)
    localDGEXParms = getattr(localConfig, 'parmsDGEX', localDGEXParms)
    localHPCGuideParms = getattr(localConfig, 'parmsHPCGuide',
      localHPCGuideParms)
    localRTMAParms = getattr(localConfig, 'parmsRTMA', localRTMAParms)
    localNamDNG5Parms = getattr(localConfig, 'parmsNamDNG5', localNamDNG5Parms)
    localTPCProbParms = getattr(localConfig, 'parmsTPCProb', localTPCProbParms)
    localAKwave10Parms = getattr(localConfig, 'parmsAKwave10', localAKwave10Parms)
    localAKwave4Parms = getattr(localConfig, 'parmsAKwave4', localAKwave4Parms)
    localEPwave10Parms = getattr(localConfig, 'parmsEPwave10', localEPwave10Parms)
    localGlobalWaveParms = getattr(localConfig, 'parmsGlobalWave', localGlobalWaveParms)
    localWCwave10Parms = getattr(localConfig, 'parmsWCwave10', localWCwave10Parms)
    localWCwave4Parms = getattr(localConfig, 'parmsWCwave4', localWCwave4Parms)
    localWNAwave10Parms = getattr(localConfig, 'parmsWNAwave10', localWNAwave10Parms)
    localWNAwave4Parms = getattr(localConfig, 'parmsWNAwave4', localWNAwave4Parms)
    localENPwaveParms = getattr(localConfig, 'parmsENPwave', localENPwaveParms)
    #note that extraISCparms are not in the standard format. These
    #are a list of ([p, p, p, p], officeType)
    localISCExtraParms = getattr(localConfig, 'extraISCparms', localISCExtraParms)

#---------------------------------------------------------------------------
# Parm groups.  Combine parms with time constraints
# list of ([parms], timeConstraints)
#---------------------------------------------------------------------------

# 6 hourly
STD6_MODEL = [([Temp, Td, RH, Wind, Wind20ft, Sky, FzLevel, SnowLevel], TC6),
             ([Haines, MixHgt, FreeWind, TransWind, VentRate], TC6),
             ([DSI, Stability, Ttrend, RHtrend], TC6),
             ([SnowAmt, PoP, CWR], TC6NG), ([QPF, Weather, IceAcc, LAL], TC6NG),
             ([MarineLayer, HrsOfSun, InvBurnOffTemp], LT24),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MaxT], MaxTTC), ([MinT], MinTTC),
             ([Wetflag], FireWx1300TC)]

# hourly
STD1_MODEL = [([Temp, Td, RH, Wind, Wind20ft, Sky, FzLevel, SnowLevel], TC1),
             ([Haines, MixHgt, FreeWind, TransWind], TC1),
             ([DSI, Stability, VentRate, Ttrend, RHtrend], TC1),
             ([SnowAmt, PoP, CWR], TC1), ([QPF, Weather, IceAcc, LAL], TC1),
             ([MarineLayer, HrsOfSun, InvBurnOffTemp], LT24),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MaxT], MaxTTC), ([MinT], MinTTC),
             ([Wetflag], FireWx1300TC)]

# 3 hourly
STD3_MODEL = [([Temp, Td, RH, Wind, Wind20ft, Sky, FzLevel, SnowLevel], TC3),
             ([Haines, MixHgt, FreeWind, TransWind], TC3),
             ([DSI, Stability, VentRate, Ttrend, RHtrend], TC3),
             ([SnowAmt, PoP, CWR], TC3NG), ([QPF, IceAcc, Weather, LAL], TC3NG),
             ([MarineLayer, HrsOfSun, InvBurnOffTemp], LT24),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MaxT], MaxTTC), ([MinT], MinTTC),
             ([Wetflag], FireWx1300TC)]

######DCS3501
# 3 hourly-HIRESW
STD3_MODEL_HIRESW = [([Temp, Td, RH, Wind, FzLevel], TC3),
             ([MixHgt, FreeWind, TransWind], TC3), ([QPF, CWR], TC3NG),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MaxT], MaxTTC), ([MinT], MinTTC)]

# 12 hourly
STD12_MODEL = [([Temp, Td, RH, Wind, Wind20ft, Sky, FzLevel, SnowLevel], TC12),
             ([Haines, MixHgt, FreeWind, TransWind], TC12),
             ([DSI, Stability, VentRate, Ttrend, RHtrend], TC12),
             ([SnowAmt, PoP, CWR], TC12NG),
             ([QPF, IceAcc, Weather, LAL], TC12NG),
             ([MaxT], MaxTTC), ([MinT], MinTTC),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MarineLayer, HrsOfSun, InvBurnOffTemp], LT24),
             ([Wetflag], FireWx1300TC)]

# MOS (Model)
MOS_MODEL = [([Temp, Td, Wind, Weather, Sky], TC1),
             ([MaxT], MaxTTCMOS), ([MinT], MinTTCMOS),
             ([SnowAmt, PoP], LTMOS), ([QPF], TC6NG)]

# Fcst and official database parameter groupings
OFFICIALDBS = [([Temp, Td, Wind, Weather, Sky, FzLevel, SnowLevel], TC1),
          ([HeatIndex, WindChill, RH, SnowAmt, CWR, QPF], TC1),
          ([PoP, Ttrend, RHtrend, Wind20ft], TC1),
          ([MinT], MinTTC), ([MaxT], MaxTTC),
          ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
          ([WaveHeight, SurfHeight, WindGust, Swell, Swell2, Period, Period2], TC1),
          ([VentRate, LAL, Haines, MixHgt, FreeWind, TransWind], TC1),
          ([WindWaveHeight, DSI, Stability, MarineLayer], TC1),
          ([HrsOfSun, InvBurnOffTemp], LT24),
          ([IceAcc, IceCoverage, Hazards], TC1),
          ([Wetflag], FireWx1300TC),
          ([StormTotalSnow], TC1),
          # Tropical parms
          ([prob34, prob50, prob64], TC1),
          ([pws34,pws50,pws64,SurgeHtPlusTide,SurgeHtPlusTideWTopo], TC1),
          ([pwsD34,pwsD64], PWSDTC),
          ([pwsN34,pwsN64], PWSNTC),
          ([pws34int,pws64int], TC6NG),
          # DR20541 and 20482
          ([PoP12hr], TC12NG),
          ([QPF6hr, SnowAmt6hr], TC6NG),
          ([cape], LT6NG),
          ([ApparentT, HeatIndex, WindChill, UWaveDir, VWaveDir, LkSfcT, SnowMap, WaveDir, SnowRatio, StormTotalQPF], TC1),
          ]

# Global Wave Watch III, WNAWAVE, AKWAVE Model database parameter groupings
WAVEPARMS = [([WindWaveHeight, WaveHeight, SurfHeight, Wind], TC6),
            ([Swell, Swell2, Period, Period2], TC6)]

# GLWM Model database parameter groupings
GLWMPARMS = [([SigWaveHgt, WindWaveHgt, WindWaveDir, WindWavePeriod], TC1)]
######DCS3501
# HIRESW database parameter groupings
HIRESWarwPARMS = [([Temp], TC3)]
HIRESWnmmPARMS = [([Temp], TC3)]
#DR20634 SPCPARMS = [([Temp], TC1)]

# LAPS database parameter groupings
LAPSPARMS = [([Temp, Td, Wind, Weather, Sky, SnowAmt, QPF, Radar], TC1)]

# MOS Guide parameters - this is supposed to be a fix to the baseline error - we'll see
#MOSGuidePARMS = [([Temp, Td, Wind, RH], TC3),
#                 ([MinT], MinTTC), ([MaxT], MaxTTC),
#                 ([PoP6, PoP12, TstmPrb3, TstmPrb6, TstmPrb12, Sky, WindGust, QPF6, QPF12], TC6NG)]
MOSGuidePARMS = [([Temp, Td, Wind, RH], TC1),
                 ([MinT], MinTTC), ([MaxT], MaxTTC),
                 ([TstmPrb3], TC3NG),
                 ([PoP6,  TstmPrb6, Sky, WindGust, QPF6], TC6NG),
                 ([PoP12, PoP, QPF12, QPF, TstmPrb12], TC12NG)]

# OPC TAF parameters (for NW, SW, and E)
OPCTAFBPARMS = [([WindWaveHeight, WaveHeight], TC1)]

# SAT database parameter groupings
SATPARMS = [([SatVisE, SatIR11E, SatIR13E, SatIR39E, SatWVE, SatFogE], TC_1M),
            ([SatVisW, SatIR11W, SatIR13W, SatIR39W, SatWVW, SatFogW], TC_1M)]

# MSAS database parameter groupings
MSASPARMS = [([Temp, Td, Wind], TC1)]

# GLERL database parameter groupings
GLERLPARMS = [([WaveHeight, Period, Swell], TC1)]

# SREF database parameter groupings
SREFPARMS = [([Temp, Td, Wind], TC1)]

HPCQPF_MODEL = [([QPF], TC6NG)]
RFCQPF_MODEL = [([QPF], TC6NG)]

#DR3511 HPCDELTA_MODEL = [([DeltaMinT], MinTTC), ([DeltaMaxT], DeltaMaxTTC),
#DR3511                  ([DeltaWind], TC12),
#DR3511                  ([DeltaPoP, DeltaSky], TC12NG)]
#DR3511
HPCGUIDE_MODEL = [([MaxT], MaxTTC), ([MinT], MinTTC),
                  ([Sky, Td, Wind], TC6), ([PoP], TC12NG)]

# This model has Wind, but we use a different definition here since we
# want to higher maximum velocity to be allowed.
TPCTCM_MODEL = [([HiWind], TC3)]

# RTMA database parameter groupings
#if SID in ALASKA_SITES: - not sure if this is right
if SID in ALASKA_SITES or SID in ["HFO", "SJU"]:
    RTMAPARMS = [([Temp,Td,RH,Wind],TC1),
             ([MinT],MinTTC), ([MaxT],MaxTTC),
             ([MinRH],MinRHTC), ([MaxRH],MaxRHTC),
             ([TUnc,TdUnc,WSpdUnc,WDirUnc],TC1)]
else:
    RTMAPARMS = [([Temp,Td,RH,Wind,QPE,Sky],TC1),
             ([MinT],MinTTC), ([MaxT],MaxTTC),
             ([MinRH],MinRHTC), ([MaxRH],MaxRHTC),
             ([TUnc,TdUnc,WSpdUnc,WDirUnc],TC1)]

# NamDNG5 database parameter groupings
NamDNG5PARMS = [([Temp, Td, RH, Wind, Sky, WindGust, Vis], TC3),
                ([MixHgt, TransWind, SnowLevel], TC3),
                ([MinT], MinTTC), ([MaxT], MaxTTC),
                ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
                ([QPF3, PoP, SnowAmt], TC3NG),
                ([QPF6, PoP6, SnowAmt6], TC6NG), ([QPF12, PoP12], TC12NG),
                ([MaxT3, MinT3, MaxRH3], TC3NG),
                ]

TPCProbPARMS = [([prob34, prob50, prob64], TC1),
                ([pws34,pws50,pws64], TC1),
                ([pwsD34,pwsD64], PWSDTC),
                ([pwsN34,pwsN64], PWSNTC),
                ]

# Cobb snow tool
parmsNAM12 = [([SnowRatio], TC1)]
parmsGFS40 = [([SnowRatio], TC1)]

ENPwave_parms = [([WindWaveHeight, WaveHeight, SurfHeight, Wind], TC6),
            ([Swell, Swell2, Period, Period2], TC6)]
#---------------------------------------------------------------------------
# Databases for a site.
# list of (Database, [parms])
#---------------------------------------------------------------------------
DATABASES = [(Official, OFFICIALDBS + localParms),
             (Fcst, OFFICIALDBS + localParms),
             (Practice, OFFICIALDBS + localParms),
             (TestFcst, OFFICIALDBS + localParms),
             (NAM80, STD6_MODEL + localNAM80Parms),
             (NAM95, STD6_MODEL + localNAM95Parms),
             (RUC13, STD1_MODEL + localRUC13Parms),
             (RUC80, STD1_MODEL + localRUC80Parms),
             (NGM80, STD6_MODEL + localNGM80Parms),
             (NGM95, STD6_MODEL + localNGM95Parms),
             (GFS40, STD6_MODEL + localGFS40Parms),
             (GFS80, STD6_MODEL + localGFS80Parms),
             (GFS75, STD6_MODEL + localGFS75Parms),
             (GFS190, STD6_MODEL + localGFS190Parms),
             (NAM40, STD3_MODEL + localNAM40Parms),
             (NAM12, STD3_MODEL + localNAM12Parms),
             (gfsLR, STD12_MODEL + localgfsLRParms),
             (GWW, WAVEPARMS + localGWWParms),
             (WNAWAVE, WAVEPARMS + localWNAWAVEParms),
             (AKWAVE, WAVEPARMS + localAKWAVEParms),
             (AKwave10, WAVEPARMS + localAKwave10Parms),
             (AKwave4, WAVEPARMS + localAKwave4Parms),
             (EPwave10, WAVEPARMS + localEPwave10Parms),
             (GlobalWave, WAVEPARMS + localGlobalWaveParms),
             (GLWM, GLWMPARMS + localGLWMParms),            #####DCS3499
             (HIRESWarw, STD3_MODEL + localHIRESWarwParms), #####DCS3501
             (HIRESWnmm, STD3_MODEL + localHIRESWnmmParms),
#DR20634             (SPC, SPCPARMS + localSPCParms),
             (WCwave10, WAVEPARMS + localWCwave10Parms),
             (WCwave4, WAVEPARMS + localWCwave4Parms),
             (WNAwave10, WAVEPARMS + localWNAwave10Parms),
             (WNAwave4, WAVEPARMS + localWNAwave4Parms),
             (HPCGrid, MOS_MODEL + localMOSParms),
             (HPCQPF, HPCQPF_MODEL + localHPCQPFParms),
             (RFCQPF, RFCQPF_MODEL + localRFCQPFParms),
#DR3511             (HPCDelta, HPCDELTA_MODEL + localHPCDeltaParms),
             (HPCGuide, HPCGUIDE_MODEL + localHPCGuideParms),
             (TPCTCM, TPCTCM_MODEL + localTCMParms),
             (DGEX, STD6_MODEL + localDGEXParms),
             (LAPS, LAPSPARMS + localLAPSParms),
             (MOSGuide, MOSGuidePARMS + localMOSGuideParms),
             (SREF, SREFPARMS + localSREFParms),
             (OPCTAFBE, OPCTAFBPARMS + localOPCWavEParms),
             (OPCTAFBNW, OPCTAFBPARMS + localOPCWavNWParms),
             (OPCTAFBSW, OPCTAFBPARMS + localOPCWavSWParms),
             (SAT, SATPARMS + localSATParms),
             (MSAS, MSASPARMS + localMSASParms),
             (GLERL, GLERLPARMS + localGLERLParms),
             (RTMA, RTMAPARMS + localRTMAParms),
             (NamDNG5, NamDNG5PARMS + localNamDNG5Parms),
             (TPCProb, TPCProbPARMS + localTPCProbParms),
	     (ENPwave, ENPwave_parms + localENPwaveParms),
             (Test, OFFICIALDBS + localParms)] + localDBs

# Intersite coordination database parameter groupings, based on
# OFFICIALDBS, but time constraint is always TC1
ISCPARMS = []
for wes, tc in (OFFICIALDBS + localISCParms):
    ISCPARMS.append((wes, TC1))
# We also add in any extraISCparms as needed, but only for office
# types other than our own.
for wes, officeType in (EXTRA_ISC_PARMS + localISCExtraParms):
    if myOfficeType == officeType:
        continue
    for we in wes:
        wecopy = list(we)
        wecopy[0] = wecopy[0] + officeType  #rename the weather element
        wecopy = tuple(wecopy)
        ISCPARMS.append(([wecopy], TC1))
        
# Restore database parameter groupings (based on OFFICIALDBS, but TC1)
RESTOREPARMS = []
for wes, tc in (OFFICIALDBS + localParms):
    RESTOREPARMS.append((wes, TC1))

# Now add the ISC and Restore databases to the DATABASES groupings
DATABASES.append((Restore, RESTOREPARMS))
DATABASES.append((ISC, ISCPARMS))

#---------------------------------------------------------------------------
#
#  General server configuration section
#
#---------------------------------------------------------------------------

#----------------------------------------------------------------------------
# Server settings     DO NOT CHANGE THESE DEFINITIONS
#----------------------------------------------------------------------------
from com.raytheon.edex.plugin.gfe.config import IFPServerConfig, SimpleServerConfig
IFPConfigServer = SimpleServerConfig()
#IFPConfigServer.allowedNodes             = []
IFPConfigServer.allowTopoBelowZero       = 1


def doIt():
    # Import the local site configuration file (if it exists)
    import doConfig
#    import VTECPartners
    (models, projections, vis, wx, desDef, allSites, domain, siteId, timeZone,officeTypes) = \
      doConfig.parse(GFESUITE_SITEID, DATABASES, types, visibilities, SITES,
      allProjections)
    IFPConfigServer.models                  = models
    IFPConfigServer.projectionData          = projections
    IFPConfigServer.weatherVisibilities     = vis
    IFPConfigServer.weatherTypes            = wx
    IFPConfigServer.discreteDefinitions     = desDef
    IFPConfigServer.allSites                = allSites
    IFPConfigServer.officeTypes             = officeTypes
    IFPConfigServer.siteID                  = siteId
    IFPConfigServer.timeZone                = timeZone
    IFPConfigServer.d2dModels               = doConfig.d2dParse(D2DMODELS)
    IFPConfigServer.netCDFDirs              = doConfig.netcdfParse(NETCDFDIRS)
    IFPConfigServer.satDirs                 = doConfig.parseSat(SATDATA)
    IFPConfigServer.domain                  = domain

    (serverHost, mhsid, \
    rpcPort, \
    initMethods, accumulativeD2DElements, \
    initSkips, d2dVersions, \
    logFilePurgeAfter, \
    prdDir, baseDir, \
    extraWEPrecision, \
    autoConfigureNotifyTextProd, \
    iscRoutingTableAddress, \
    requestedISCsites, requestISC, \
    sendiscOnSave, sendiscOnPublish, \
    requestedISCparms, \
    transmitScript) \
       = doConfig.otherParse(\
      GFESUITE_SERVER, GFESUITE_MHSID, \
      GFESUITE_PORT, INITMODULES,
      D2DAccumulativeElements,
      INITSKIPS, D2DDBVERSIONS, LOG_FILE_PURGE_AFTER, 
      GFESUITE_PRDDIR, GFESUITE_HOME,
      ExtraWEPrecision, AUTO_CONFIGURE_NOTIFYTEXTPROD, ISC_ROUTING_TABLE_ADDRESS,
      REQUESTED_ISC_SITES, REQUEST_ISC, SEND_ISC_ON_SAVE, SEND_ISC_ON_PUBLISH,
      REQUESTED_ISC_PARMS, TRANSMIT_SCRIPT)
    IFPConfigServer.serverHost = serverHost
    IFPConfigServer.mhsid = mhsid
    IFPConfigServer.rpcPort = rpcPort
    IFPConfigServer.initMethods = initMethods
    IFPConfigServer.accumulativeD2DElements = accumulativeD2DElements
    IFPConfigServer.initSkips = initSkips
    IFPConfigServer.d2dVersions =  d2dVersions
    IFPConfigServer.logFilePurgeAfter = logFilePurgeAfter
    IFPConfigServer.prdDir = prdDir
    IFPConfigServer.baseDir = baseDir
    IFPConfigServer.extraWEPrecision = extraWEPrecision
    IFPConfigServer.autoConfigureNotifyTextProd =  autoConfigureNotifyTextProd
    IFPConfigServer.iscRoutingTableAddress = iscRoutingTableAddress
    IFPConfigServer.requestedISCsites = requestedISCsites
    IFPConfigServer.requestISC = requestISC
    IFPConfigServer.sendiscOnSave = sendiscOnSave
    IFPConfigServer.sendiscOnPublish = sendiscOnPublish
    IFPConfigServer.requestedISCparms = requestedISCparms
    IFPConfigServer.transmitScript = transmitScript

doIt()
