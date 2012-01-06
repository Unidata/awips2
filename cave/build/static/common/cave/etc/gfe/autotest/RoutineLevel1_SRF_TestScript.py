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

testDefinitions  = [
'Definition["tempLocalEffects"] = 1',
'Definition["windLocalEffects"] = 1',
'Definition["individualExtended"] = 0',
'Definition["extendedLabel"] = 1',
'Definition["ripGrid"] = "LAL"',
'Definition["waterSpoutGrid"] = "LAL"',
'Definition["includeOutlook"] = 1',
'Definition["includeTideHeights"] = 1'
]


testUVI = [
"""def getPreviousProduct(self, pil, searchString, version=0):
        print
        print pil
        if pil.find(self._uviPil) >= 0:
            print 'returning uvi'
            return self._uviProduct()
        else:
            print 'returning blank'
            return ""
""",
"""def _uviProduct(self):
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
]

import os
path, file = os.path.split(__file__)
path = os.path.abspath(os.path.join(path,"../testdata"))
testTides = """Definition["tideFiles"] = {               
             # For each tide table, list the file where it can
             # be found
             "Venice Inlet": "<testdata>/VeniceInlet2010.txt",
             "Saint Petersburg": "<testdata>/SaintPetersburg2010.txt",
             "Fort Myers": "<testdata>/FortMyers2010.txt",
             "Cedar Key": "<testdata>/CedarKey2010.txt",
             }

"""
testTides = testTides.replace("<testdata>", path)

overrides = [
"""def ridgeValleyAreas(self, tree, node):
        # List of edit area names for which we want
        # ridge/valley winds reported:
        #
        # 20-FOOT WINDS...
        #     VALLEYS/LWR SLOPES...
        #     RIDGES/UPR SLOPES....       
        #
        # e.g.
        # return ["Area1"]
        #
        return ["FLZ042"]
""",
"""def _surfPeriodAdditionalAreasDict(self):
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
]

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

        "SURFZONE FORECAST FOR FLORIDA",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
        "600 AM EST FRI JAN 1 2010",
        "FLZ039-042-048-049",
        "LEVY-CITRUS-HERNANDO-PASCO-",
        ".TODAY...",
        "SKY/WEATHER.........CLOUDY (95-100 PERCENT). PATCHY DENSE FOG. WIDESPREAD THUNDERSTORMS.", 
        "MAX TEMPERATURE.....AROUND 78.", 
        "BEACH WINDS.........VERY WINDY. SOUTHWEST WINDS AROUND 10 MPH INCREASING TO SOUTHEAST AROUND 45 MPH IN THE AFTERNOON.", 
        "SURF ALONG NORTH FACING REEFS.............OVER 20 FEET SUBSIDING TO 8 TO 12 FEET IN THE AFTERNOON.",
        "SURF ALONG SOUTH FACING REEFS.............OVER 20 FEET SUBSIDING TO 8 TO 12 FEET IN THE AFTERNOON.",
        "SWELL...............LARGE SWELLS. WEST SWELL 20 TO 30 FEET BECOMING SOUTHEAST AND INCREASING TO AROUND 40 FEET IN THE AFTERNOON.", 
        "PERIOD..............12 SECONDS DECREASING TO 10 SECONDS IN THE AFTERNOON.", 
        "SECONDARY SWELL.....LARGE SWELLS. SOUTHEAST SWELL 20 TO 30 FEET BECOMING SOUTHWEST AND INCREASING TO AROUND 40 FEET IN THE AFTERNOON.", 
        "SECONDARY PERIOD....7 SECONDS.", 
        "WATER CONDITION.....A LIGHT CHOP INCREASING TO SMOOTH IN THE AFTERNOON.", 
        "WATER TEMPERATURE...", "INSERT WATER TEMPERATURE HERE",
        "UVI INDEX...........HIGH.", 
        "RIP CURRENT RISK....HIGH. HIGH SURF AND LARGE SWELLS WILL PRODUCE DANGEROUS POUNDING SURF AND RIP CURRENTS AT THE BEACHES. PEOPLE VISITING THE BEACHES SHOULD STAY OUT OF THE HIGH SURF.",
        "LIGHTNING THREAT....VERY INFREQUENT DEADLY LIGHTNING.", 

        ".TONIGHT...",
        "VERY WINDY.",
        "PARTLY CLOUDY.",
        "WIDESPREAD THUNDERSTORMS.",
        "WARMER.","LOWS AROUND 60","SOUTHWEST WINDS AROUND 45 MPH.", 
        ".SATURDAY...",
        "STRONG WINDS.",
        "MOSTLY CLEAR.",
        "FREQUENT LIGHT RAIN.","WIDESPREAD LIGHT FREEZING RAIN.","LOWS IN THE UPPER 60S.",
        "HIGHS IN THE UPPER 70S.","WEST WINDS 45 TO 55 MPH.",
        ".SUNDAY...","STRONG WINDS.","MOSTLY CLOUDY."," WIDESPREAD LIGHT FREEZING RAIN.",
        "VERY LIGHT SNOW LIKELY.","WIDESPREAD VERY LIGHT SLEET.","LOWS IN THE MID 60S.",
        "HIGHS IN THE UPPER 70S.","SOUTH WINDS 55 TO 60 MPH.",
        ".MONDAY...","STRONG WINDS.","MOSTLY CLOUDY.","WIDESPREAD VERY LIGHT SLEET.",
        "AREAS OF BLOWING SNOW.","PATCHY FOG.","LOWS IN THE MID 60S.","HIGHS AROUND 80.",
        "SOUTHWEST WINDS AROUND 65 MPH.", 

        "TIDE INFORMATION...",

        "AT VENICE INLET...",

        "HIGH TIDE AT 2:34 AM",
        "LOW TIDE AT 10:18 AM",
        "HIGH TIDE AT 4:32 PM",
        "LOW TIDE AT 10:23 PM",
        "PINELLAS-HILLSBOROUGH-MANATEE-SARASOTA-",
        "SURF................OVER 20 FEET SUBSIDING TO 8 TO 12 FEET IN THE AFTERNOON.", 
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
        "TODAY",
        "MAX TEMPERATURE.....AROUND 70...EXCEPT AROUND 30 NEAR THE COAST.",
        "BEACH WINDS.........",
        "INLAND...............VERY WINDY. SOUTHWEST WINDS AROUND 10 MPH INCREASING TO SOUTHEAST AROUND 45 MPH IN THE AFTERNOON.",
        "COASTAL..............VERY WINDY. NORTH WINDS AROUND 45 MPH SHIFTING TO THE SOUTHEAST IN THE AFTERNOON.",
        "RIP CURRENT RISK....MODERATE. A MODERATE RISK OF RIP CURRENTS MEANS WIND AND OR WAVE CONDITIONS SUPPORT STRONGER OR MORE FREQUENT RIP CURRENTS. ALWAYS HAVE A FLOTATION DEVICE WITH YOU IN THE WATER.", 
        "WATERSPOUT THREAT...VERY SLIGHT CHANCE OF WATERSPOUTS.",  
        ".OUTLOOK",
        "TIDE INFORMATION...",

        "AT CEDAR KEY...",

        "HIGH TIDE 3.1 FEET AT 4:27 AM",
        "LOW TIDE 0.0 FEET AT 11:15 AM",
        "HIGH TIDE 3.0 FEET AT 5:42 PM",
        "LOW TIDE 0.9 FEET AT 11:42 PM",

        "AT VENICE INLET...",

        "HIGH TIDE 1.6 FEET AT 2:34 AM",
        "LOW TIDE -0.1 FEET AT 10:18 AM",
        "HIGH TIDE 1.3 FEET AT 4:32 PM",
        "LOW TIDE 0.6 FEET AT 10:23 PM",

        "FLZ050-051-055-060", 

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
        "combinations": [(["FLZ039", "FLZ042", "FLZ048", "FLZ049"], "Region01"),
                         (["FLZ050", "FLZ051", "FLZ055", "FLZ060"], "Region02"),
                         (["FLZ062", "FLZ065"], "Region03"),
                         ],
        }
    for script in scripts:
        drtHour = script.get("drtHour", None)
        if drtHour is not None:
            script["drtTime"] =  self.getAbsFromLocal(2010, 1, 1, drtHour, 0)      
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults, level=level)


