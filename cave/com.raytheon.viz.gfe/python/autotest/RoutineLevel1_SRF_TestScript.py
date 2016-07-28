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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SRF tests
#
# Author: hansen
# ----------------------------------------------------------------------------

#### File Changes

testDefinitions  = """

Definition["tempLocalEffects"] = 1
Definition["windLocalEffects"] = 1
Definition["individualExtended"] = 0
Definition["extendedLabel"] = 1
Definition["ripGrid"] = "LAL"
Definition["waterSpoutGrid"] = "LAL"
Definition["includeOutlook"] = 1
Definition["includeTideHeights"] = 1

"""


testUVI = """

    def getPreviousProduct(self, pil, searchString, version=0):
        print
        print pil
        if pil.find(self._uviPil) >= 0:
            print 'returning uvi'
            return self._uviProduct()
        else:
            print 'returning blank'
            return ""

    def _uviProduct(self):
          return  \"\"\"        
              Ultraviolet Index Forecast

              000
              AEUS41 KWBC 041818
              UVICAC

              NOAA/EPA ULTRAVIOLET INDEX /UVI/ FORECAST
              NWS CLIMATE PREDICTION CENTER CAMP SPRINGS MD
              218 PM EDT TUE OCT 4 2005

              VALID OCT 5 2005 AT SOLAR NOON /APPROXIMATELY NOON
              LOCAL STANDARD TIME OR 100 PM LOCAL DAYLIGHT TIME/

              THE UV INDEX IS CATEGORIZED BY THE WORLD HEALTH ORGANIZATION
              AS FOLLOWS:
                  UVI             EXPOSURE LEVEL
                  0 1 2              LOW
                  3 4 5              MODERATE
                  6 7                HIGH
                  8 9 10             VERY HIGH
                  11 AND GREATER     EXTREME

                  FOR HEALTH RELATED ISSUES...CONTACT EPA AT 1-800-296-1996
                  FOR TECHNICAL INFORMATION ABOUT THE UV INDEX....
                  GO TO THE NATIONAL WEATHER SERVICE UV INDEX WEB PAGE:
                      WWW.CPC.NCEP.NOAA.GOV/PRODUCTS/STRATOSPHERE/UV^INDEX

                      CITY               STATE  UVI       CITY               STATE  UVI
                      ALBUQUERQUE          NM     7       LITTLE ROCK          AR     6
                      ANCHORAGE            AK     0       LOS ANGELES          CA     7
                      ATLANTIC CITY        NJ     5       LOUISVILLE           KY     5
                      ATLANTA              GA     5       MEMPHIS              TN     6
                      BALTIMORE            MD     5       MIAMI                FL     9
                      BILLINGS             MT     4       MILWAUKEE            WI     4
                      BISMARCK             ND     2       MINNEAPOLIS          MN     1
                      BOISE                ID     4       MOBILE               AL     4
                      BOSTON               MA     5       NEW ORLEANS          LA     6
                      BUFFALO              NY     4       NEW YORK             NY     5
                      BURLINGTON           VT     4       NORFOLK              VA     6
                      CHARLESTON           WV     5       OKLAHOMA CITY        OK     6
                      CHARLESTON           SC     3       OMAHA                NE     1
                      CHEYENNE             WY     5       PHILADELPHIA         PA     5
                      CHICAGO              IL     5       PHOENIX              AZ     7
                      CLEVELAND            OH     5       PITTSBURGH           PA     5
                      CONCORD              NH     5       PORTLAND             ME     4
                      DALLAS               TX     7       PORTLAND             OR     1
                      DENVER               CO     5       PROVIDENCE           RI     5
                      DES MOINES           IA     1       RALEIGH              NC     6
                      DETROIT              MI     5       SALT LAKE CITY       UT     5
                      DOVER                DE     5       SAN FRANCISCO        CA     5
                      HARTFORD             CT     4       SAN JUAN             PU     7
                      HONOLULU             HI    10       SEATTLE              WA     2
                      HOUSTON              TX     8       SIOUX FALLS          SD     2
                      INDIANAPOLIS         IN     5       ST. LOUIS            MO     5
                      JACKSON              MS     7       TAMPA                FL     6
                      JACKSONVILLE         FL     5       WASHINGTON           DC     5
                      LAS VEGAS            NV     6       WICHITA              KS     1

            \"\"\"

"""

import os
path, file = os.path.split(__file__)
path = os.path.abspath(os.path.join(path,"../../testdata"))
testTides = """

Definition["tideFiles"] = {
             # For each tide table, list the file where it can
             # be found
             "Venice Inlet": "<testdata>/VeniceInlet2010.txt",
             "Saint Petersburg": "<testdata>/SaintPetersburg2010.txt",
             "Fort Myers": "<testdata>/FortMyers2010.txt",
             "Cedar Key": "<testdata>/CedarKey2010.txt",
             }

"""
testTides = testTides.replace("<testdata>", path)

overrides = """

    def ridgeValleyAreas(self, tree, node):
        # List of edit area names for which we want
        # ridge/valley winds reported:
        #
        # 20-foot winds...
        #     Valleys/lwr slopes...
        #     Ridges/upr slopes....       
        #
        # e.g.
        # return ["Area1"]
        #
        return ["FLZ142", "FLZ242"]

    
    def _surfPeriodAdditionalAreasDict(self):
        return {
            "landSeaArea": {
                "analysisList": [
                   ("Sky", self.minMax, [0]),
                   ("PoP", self._PoP_analysisMethod("SurfPeriod")),
                   ("Wx", self.rankedWx),
                   ("WindGust", self.moderatedMax, [3]),
                   ("WaveHeight", self.maximum, [6]),
                   ("SurfHeight", self.maximum, [6]),
                   ("WindWaveHgt", self.maximum, [6]),
                   ("Wind", self.vectorAvg, [6]),          
                   ("MaxT", self.minMax),
                   ("MinT", self.minMax),
                   ("HeatIndex", self.minMax), 
                   ("T", self.minMax),
                   ("LAL", self.minMax),
                  ],
                "phraseList": [
                    self.skyWeather_byTimeRange_compoundPhrase,
                    self.popMax_phrase,
                    self.dayOrNight_phrase,
                    self.severeWeather_phrase,
                    self.fireWind_compoundPhrase,
                    self.fireWind_label_phrase,
                    self.fireValleyWind_compoundPhrase,  
                    self.fireRidgeWind_compoundPhrase,  
                    self.waveHeight_phrase,
                    self.chop_phrase,
                    self._sst_phrase,
                    self._uvi_phrase,
                    self.rip_phrase,
                    self.heatIndex_phrase,
                    self._lal_phrase,
                  ],
                },        
            "marineArea": {
                "analysisList": [
                   ("Swell", self.vectorModeratedMinMax, [6]),
                   ("Swell2", self.vectorModeratedMinMax, [6]),
                   ("Period", self.moderatedMinMax, [6]),
                   ("Period2", self.moderatedMinMax, [6]),
                   ("LAL", self.minMax),
                  ],
                "phraseList": [
                    self.swell_compoundPhrase,
                    self.period_phrase,
                    self.swell2_compoundPhrase,
                    self.period2_phrase,
                    self._waterSpout_phrase,
                   ],
                },
             }
 

"""

import TestScript
import CreateGrids

scripts = [
    {
    "commentary":"""
    Morning Test at 4 a.m.
    """,
    "name":"SRF_0", 
    "productType":"SRF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
        "SRFABC",

        "Surfzone Forecast for Florida",
        "National Weather Service Tampa Bay Ruskin FL",
        "600 AM EST Fri Jan 1 2010",
        "FLZ139-142-148-149",
        "Levy-Citrus-Hernando-Pasco-",
        ".TODAY...",
        "Sky/weather.........Cloudy (95-100 percent). Patchy dense fog. Widespread thunderstorms.", 
        "Max temperature.....Around 78.", 
        "Beach winds.........Very windy. Southwest winds around 10 mph increasing to southeast around 45 mph in the afternoon.", 
        "Surf along north facing reefs.............Over 20 feet subsiding to 8 to 12 feet in the afternoon.",
        "Surf along south facing reefs.............Over 20 feet subsiding to 8 to 12 feet in the afternoon.",
        "Swell...............Large swells. West swell 20 to 30 feet becoming southeast and increasing to around 40 feet in the afternoon.", 
        "Period..............12 seconds decreasing to 10 seconds in the afternoon.", 
        "Secondary swell.....Large swells. Southeast swell 20 to 30 feet becoming southwest and increasing to around 40 feet in the afternoon.", 
        "Secondary period....7 seconds.", 
        "Water condition.....A light chop increasing to smooth in the afternoon.", 
        "Water temperature...", "insert water temperature here",
        "UVI index...........High.", 
        "Rip current risk....High. High surf and large swells will produce dangerous pounding surf and rip currents at the beaches. People visiting the beaches should stay out of the high surf.",
        "Lightning threat....Very infrequent deadly lightning.", 

        ".TONIGHT...",
        "Very windy.",
        "Partly cloudy.",
        "Widespread thunderstorms.",
        "Warmer.","Lows around 60","Southwest winds around 45 mph.", 
        ".SATURDAY...",
        "Strong winds.",
        "Mostly clear.",
        "Frequent light rain.","Widespread light freezing rain.","Lows around 60.",
        "Highs in the upper 70s.","West winds 45 to 55 mph.",
        ".SUNDAY...","Strong winds.","Mostly cloudy."," Widespread light freezing rain.",
        "Very light snow likely.","Widespread very light sleet.","Lows in the upper 60s.",
        "Highs in the upper 70s.","South winds 55 to 60 mph.",
        ".MONDAY...","Strong winds.","Mostly cloudy.","Widespread very light sleet.",
        "Areas of blowing snow.","Patchy fog.","Lows in the mid 60s.","Highs around 80.",
        "Southwest winds around 65 mph.", 

        "TIDE INFORMATION...",

        "AT VENICE INLET...",

        "High tide at 2:34 AM",
        "Low tide at 10:18 AM",
        "High tide at 4:32 PM",
        "Low tide at 10:23 PM",
        "Pinellas-Hillsborough-Manatee-Sarasota-",
        "Surf................Over 20 feet subsiding to 8 to 12 feet in the afternoon.", 
        ],
    "createGrids": CreateGrids.Public_createGrids + CreateGrids.Marine_createGrids + \
                   CreateGrids.Fire_createGrids,
    "fileChanges": [
       ("SRF_<site>_Overrides", "TextUtility", "add", testUVI, "undo"),
       ("SRF_<site>_Definition", "TextUtility", "add", testTides, "undo"),
       ],
    },
    {
    "commentary":"""
    Morning Test with surfAreas, tempLocalEffects, windLocalEffects
                 individualExtended turned off, includeTrends turned off
                 ripGrid set to LAL, waterSpoutGrid set to LAL
                 includeOutlook
    """,
    "name":"SRF_1", 
    "productType":"SRF",
    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
    "checkStrings": [
        ".TODAY...",
        "Max temperature.....Around 70...except around 30 near the coast.",
        "Beach winds.........",
        "Inland...............Very windy. Southwest winds around 10 mph increasing to southeast around 45 mph in the afternoon.",
        "Coastal..............Very windy. North winds around 45 mph shifting to the southeast in the afternoon.",
        "Rip current risk....Moderate. A moderate risk of rip currents means wind and or wave conditions support stronger or more frequent rip currents. Always have a flotation device with you in the water.", 
        "Waterspout threat...Very slight chance of waterspouts.",  
        ".OUTLOOK...",
        "TIDE INFORMATION...",

        "AT CEDAR KEY...",

        "High tide 3.1 feet at 4:27 AM",
        "Low tide 0.0 feet at 11:15 AM",
        "High tide 3.0 feet at 5:42 PM",
        "Low tide 0.9 feet at 11:42 PM",

        "AT VENICE INLET...",

        "High tide 1.6 feet at 2:34 AM",
        "Low tide -0.1 feet at 10:18 AM",
        "High tide 1.3 feet at 4:32 PM",
        "Low tide 0.6 feet at 10:23 PM",

        "FLZ050-151-155-160", 

    ],
    "createGrids": [
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["Coastal"]),
        ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, ["Inland"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 20, ["Coastal"]),
        ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 50, ["Inland"]),
        ("Fcst", "Wind", "VECTOR", 0, 12, (40, "N"), ["Coastal"]),
        ("Fcst", "Wind", "VECTOR", 0, 12, (10, "SW"), ["Inland"]),
        ],
    "fileChanges": [
       ("SRF_<site>_Overrides", "TextUtility", "add", testUVI, "undo"),
       ("SRF_<site>_Definition", "TextUtility", "add", testTides, "undo"),
       ("SRF_<site>_Definition", "TextUtility", "add", testDefinitions, "undo"),
       ("SRF_<site>_Overrides", "TextUtility", "add", overrides, "undo"),
       ],
    },
##    {
##    "commentary":"""
##    Morning Update
##    """,
##    "name":"SRF_2", 
##    "productType":"SRF",
##    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None }",
##    "checkStrings": [
##                    ],    
##    "createGrids": [], 
##    },
##    {
##    "commentary":"""
##    Afternoon Update Test at 2 p.m.
##    """,
##    "name":"SRF_3", 
##    "productType":"SRF",
##    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Afternoon Update', ('Issued By', 'issuedBy'): None }",
##    "checkStrings": [
##                    ],
##    "createGrids": [], 
##    "drtHour": 14,
##    },
##    {
##    "commentary":"""
##    Evening Update Test at 10 p.m.
##    """,
##    "name":"SRF_4", 
##    "productType":"SRF",
##    "cmdLineVars": "{('Product Issuance', 'productIssuance'): 'Evening (for tomorrow)', ('Issued By', 'issuedBy'): None }",
##    "checkStrings": [],    
##    "createGrids": [],
##    "drtHour": 22,
##    },

    {
    "name":"SRF_CleanUp",
    "commentary": "Clean out grids",
    "productType": None,
    "deleteGrids": TestScript.general_deleteGrids,
    },
    ]

def testScript(self, dataMgr, level="Site"):
    gridsStartTime = self.getAbsFromLocal(2010, 1, 1, 0, 0)
    drtTime = self.getAbsFromLocal(2010, 1, 1, 4, 0)    
    defaults = {
        "gridsStartTime": gridsStartTime,
        "drtTime": drtTime,
        "orderStrings": 1,
        "internalStrip": 1,
        "comboFlag": 1,
        "combinations": [(["FLZ139", "FLZ142", "FLZ148", "FLZ149"], "Region01"),
                         (["FLZ050", "FLZ151", "FLZ155", "FLZ160"], "Region02"),
                         (["FLZ162", "FLZ165"], "Region03"),
                         ],
        }
    for script in scripts:
        drtHour = script.get("drtHour", None)
        if drtHour is not None:
            script["drtTime"] =  self.getAbsFromLocal(2010, 1, 1, drtHour, 0)      
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)


