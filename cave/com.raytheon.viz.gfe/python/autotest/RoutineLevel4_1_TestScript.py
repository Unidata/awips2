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
# RoutineLevel4_1_TestScript    Local Effects
#
# Author:
# ----------------------------------------------------------------------------

# First run setupTextEA



windLE1 = """Definition["windLE_list"] = 1"""
windLE2 = """TextProduct.Definition["windLE_list"] = 2"""

tempLE1 = """Definition["tempLE_list"] = 1"""
tempLE2 = """TextProduct.Definition["tempLE_list"] = 2"""

periodLE1 = """Definition["Period_1_version"] = 1"""
periodLE2 = """TextProduct.Definition["Period_1_version"] = 2"""
periodLE3 = """TextProduct.Definition["Period_1_version"] = 3"""

tempLE_method1 = """Definition["tempLE_method"] = 1"""
tempLE_method2 = """TextProduct.Definition["tempLE_method"] = 2"""
                   

snowLE1 = """##                   (self.weather_phrase,self._wxLocalEffects_list()),             
##                   (self.snow_phrase,self._snowAmtLocalEffects_list()),
##                   (self.total_snow_phrase,self._totalSnowAmtLocalEffects_list()),
"""

snowLE2 = """def Period_1_version1(self):
    return { 
        "type": "component",
        "methodList": [
                         self.consolidateSubPhrases,
                          self.assemblePhrases,   
                          self.wordWrap,          
                          ],
        "analysisList": [
                       #("MinT", self.avg),
                       #("MaxT", self.avg),
                       ("MaxT", self.stdDevMinMax),
                       ("T", self.hourlyTemp),
                       ("T", self.minMax),
                       ("Sky", self.median, [3]),
                       ("Sky", self.binnedPercent, [6]),
                       ("PoP", self._PoP_analysisMethod("Period_1"), [3]),
                       ("PoP", self.binnedPercent, [3]),
                       ("SnowAmt", self.accumMinMax),
                       ("IceAccum", self.accumMinMax),
                       ("Wind", self.vectorMedianRange, [6]),
                       ("Wind", self.vectorMinMax, [6]),
                       ("WindGust", self.maximum, [6]),
                       ("Wx", self.rankedWx, [3]),
                       ],
        "phraseList":[            
                   (self.weather_phrase,self._wxLocalEffects_list()),             
                   (self.snow_phrase,self._snowAmtLocalEffects_list()),
                   (self.total_snow_phrase,self._totalSnowAmtLocalEffects_list()),
                   (self.highs_phrase, self._tempLocalEffects_list()),                    
                   (self.wind_withGusts_phrase, self._windLocalEffects_list()),                   
                   ],
        "intersectAreas": [ 
                   ("MaxT", ["AboveElev", "BelowElev"]),
                   ("Wind", ["AboveElev", "BelowElev"]),
                   ("WindGust", ["AboveElev", "BelowElev"]),
                   ("SnowAmt", ["AboveElev", "BelowElev"]),
                   ("Wx", ["AboveElev", "BelowElev"]),
                   ("PoP", ["AboveElev", "BelowElev"]),
             ],
        }
"""

snow2LE1 = """##            ("Period_2_3", 12), """

snow2LE2 = """def _10_503_issuance_list(self, argDict):
        seriesDefAM = [
            ("Period_1", "period1"),    
            ("Period_2_3", 12), ("Period_2_3", 12), ("Period_4_5", 12), ("Period_4_5", 12),  
            ]
        seriesDefPM = [
            ("Period_1", "period1"),
            ("Period_2_3", 12), ("Period_2_3", 12), 
            ("Period_4_5", 12), ("Period_4_5", 12), 
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), 
            ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), ("Period_6_14", 12), 
            ("Period_6_14", 12),
            ]
        return [
            ("Morning", self.DAY(), self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM), 
            ("Morning with Pre-1st Period", self.DAY()-2, self.NIGHT(), self.NIGHT(),
             ".TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM), 
            ("Morning Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning", "late in the afternoon",
             1, seriesDefAM), 
            ("Afternoon Update", "issuanceHour", self.NIGHT(), self.NIGHT(),
             ".REST OF TODAY...", "early in the morning","late in the afternoon",
             1, seriesDefAM), 
            ("Afternoon", self.NIGHT(), 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM), 
            ("Afternoon with Pre-1st Period", self.NIGHT()-2, 24 + self.DAY(), 24 + self.DAY(),
             ".TONIGHT...", "late in the night", "early in the evening",
             1, seriesDefPM), 
            ("Evening Update", "issuanceHour", 24 + self.DAY(), 24 + self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","early in the evening",
             1, seriesDefPM),
            ("Early Morning Update", "issuanceHour", self.DAY(), self.DAY(),
             ".REST OF TONIGHT...", "early in the morning","late in the afternoon",
             0, seriesDefPM),  
            ]
"""

# Runs LE_Test_Local for each test

scripts = [
    {
    "name": "LE1",
    "commentary": "Local Effects: MaxT (21,40), Wind (N30,N10), Gust 0",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (30,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (10,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "HIGHS AROUND 40...EXCEPT IN THE LOWER 20S IN THE MOUNTAINS",
       "NORTH WINDS AROUND 10 MPH...EXCEPT NORTH AROUND 35 MPH IN THE MOUNTAINS",
       ],
    },
    {
    "name": "LE2",
    "commentary": "Local Effects: Wind (N20,N10) -> (N30,N20), Gust 0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (20,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6,  (10,"N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (20,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "NORTH WINDS AROUND 10 MPH INCREASING TO AROUND 25 MPH IN THE AFTERNOON",
       "IN THE MOUNTAINS...NORTH WINDS AROUND 25 MPH INCREASING TO AROUND 35 MPH IN THE AFTERNOON",
       ],
    },
    {
    "name": "LE3",
    "commentary": "Local Effects: Wind (N20,0), Gust 0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 12, (20,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (0,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "LIGHT WINDS...EXCEPT NORTH AROUND 25 MPH IN THE MOUNTAINS",
       ],
    },
    {
    "name": "LE4",
    "commentary": "Local Effects: Wind (N20,0) -> (N30,0), Gust 0",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (20,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6,  (0,"N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (0,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "LIGHT WINDS",
       "IN THE MOUNTAINS...NORTH WINDS AROUND 25 MPH INCREASING TO AROUND 35 MPH IN THE AFTERNOON",
       ],
    },
    {
    "name": "LE5",
    "commentary": "Local Effects: Wind (N20,N10), Gust 0, windLE_list=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (20,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (10,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "NORTH WINDS AROUND 25 MPH IN THE MOUNTAINS...OTHERWISE NORTH AROUND 10 MPH",
       ],
    "fileChanges": [("LE_Test_Local", "TextProduct", "replace", windLE2,
                     "undo")],
                    
    },

    {
    "name": "LE6",
    "commentary": "Local Effects: Wind (N20,N10) -> (N30,N20), Gust 0, windLE_list=1",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (20,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6,  (10,"N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (20,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "IN THE MOUNTAINS...NORTH WINDS AROUND 25 MPH INCREASING TO AROUND 35 MPH IN THE AFTERNOON",
       "IN THE VALLEYS...NORTH WINDS AROUND 10 MPH INCREASING TO AROUND 25 MPH IN THE AFTERNOON",
       ],
    "fileChanges": [("LE_Test_Local", "TextProduct", "replace", windLE2,
              "undo")],
                    
    },

    {
    "name": "LE7",
    "commentary": "Local Effects: Temp (21, 40), Wind (N20,N10), Gust 0, tempLE_list=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 21, ["AboveElev"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (20,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 12, (10,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [
       "HIGHS AROUND 40...EXCEPT IN THE LOWER 20S IN THE MOUNTAINS",
       "NORTH WINDS AROUND 10 MPH...EXCEPT NORTH AROUND 25 MPH IN THE MOUNTAINS",
       ],
    "fileChanges": [("LE_Test_Local", "TextProduct", "replace", tempLE2,
                     "undo")],
                    
    },

    {
    "name": "LE8",
    "commentary": "Local Effects: MaxT (20,20,20), Period_1_version=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area2"]),
       ],
    "checkStrings": [
       "HIGHS AROUND 20",
       ],
    "fileChanges": [("LE_Test_Local", "TextProduct", "replace", periodLE2,
                     "leave")],                    
    },
    {
    "name": "LE9",
    "commentary": "Local Effects: MaxT (20,20,40), Period_1_version=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "HIGHS AROUND 20...EXCEPT AROUND 40 IN THE BENCHES",
       ],
    "fileChanges": [("LE_Test_Local", "TextProduct", "replace", periodLE2,
                     "undo")],                    
    },
    {
    "name": "LE10",
    "commentary": "Local Effects: MaxT (20,30,40), Period_1_version=1",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "HIGHS AROUND 20...EXCEPT AROUND 30 IN THE RUSH VALLEY",
       ],
    "fileChanges": [("LE_Test_Local", "TextProduct", "replace", periodLE2,
                     "undo")],                    
    },
    
    {
    "name": "LE11",
    "commentary": "Local Effects: MaxT (20,30,40), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 30, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "HIGHS AROUND 20 IN THE CITY...AND AROUND 30 IN THE RUSH VALLEY...AND AROUND 40 IN THE BENCHES",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE2,"undo"),
       ("LE_Test_Local", "TextProduct", "replace",
        tempLE_method2,"undo"),
       ],                    
    },
    {
    "name": "LE12",
    "commentary": "Local Effects: MaxT (20,40,20), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area2"]),
       ],
    "checkStrings": [
       "HIGHS AROUND 20 IN THE CITY AND IN THE BENCHES...AND AROUND 40 IN THE RUSH VALLEY",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE2,"undo"),
       ("LE_Test_Local", "TextProduct", "replace",
        tempLE_method2,"undo"),
       ],                    
    },
    {
    "name": "LE13",
    "commentary": "Local Effects: MaxT (20,40,40), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "HIGHS AROUND 20 IN THE CITY...AND AROUND 40 IN THE RUSH VALLEY AND IN THE BENCHES",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE2,"undo"),
       ("LE_Test_Local", "TextProduct", "replace",
        tempLE_method2,"undo"),
       ],                    
    },
    {
    "name": "LE14",
    "commentary": "Local Effects: MaxT (20,20,40), Period_1_version=2",
    "createGrids": [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area3"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 20, ["area1"]),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 40, ["area2"]),
       ],
    "checkStrings": [
       "HIGHS AROUND 20 IN THE CITY AND IN THE RUSH VALLEY...AND AROUND 40 IN THE BENCHES",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE2,"undo"),
       ("LE_Test_Local", "TextProduct", "replace",
        tempLE_method2,"undo"),
       ],                    
    },
    {
    "name": "LE15",
    "commentary": "Local Effects: SnowAmt",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Lkly:S:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 3, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 3, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 3, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 5, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 5, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 5, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["area3"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["BelowElev"]),
       ],
    "checkStrings": [
       ".TODAY...", "SNOW ACCUMULATION AROUND 3 INCHES",
       ".TONIGHT...", "SNOW ACCUMULATION AROUND 5 INCHES",
       "...", "SNOW ACCUMULATION AROUND 1 INCH",
       "...", "NO SNOW ACCUMULATION",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", snowLE2, "undo"), 
       ("LE_Test_Local", "TextProduct", "replace", snow2LE2, "undo"),
       ],
    "stringOrder": "yes", 
    },
    {
    "name": "LE16",
    "commentary": "Local Effects: SnowAmt",
    "createGrids": [
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Lkly:S:-:<NoVis>:", "all"),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 5, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 0, 12, 2, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 4, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 24, 1, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 3, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 24, 36, 1, ["BelowElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 36, 48, 0, ["BelowElev"]),
       ],
    "checkStrings": [
       ".TODAY...", "SNOW ACCUMULATION AROUND 2 INCHES...EXCEPT AROUND 5 INCHES ABOVE TIMBERLINE",
       ".TONIGHT...", "SNOW ACCUMULATION AROUND 1 INCH...EXCEPT AROUND 4 INCHES ABOVE TIMBERLINE",
       "...", "SNOW ACCUMULATION OF 1 TO 3 INCHES",
       "TOTAL SNOW ACCUMULATION AROUND 4 INCHES...EXCEPT AROUND 12 INCHES ABOVE TIMBERLINE",
       "...", "NO SNOW ACCUMULATION",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", snowLE2, "undo"), 
       ("LE_Test_Local", "TextProduct", "replace", snow2LE2, "undo"),
       ],
    "stringOrder": "yes", 
    },
    
    {
    "name": "LE17",  # Wade and Ballard
    "commentary": "Local Effects: Wind (N20,N10) -> (N30,N10)",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (20,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6,  (10,"N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (10,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [ 
       "NORTH WINDS AROUND 10 MPH. IN THE MOUNTAINS...NORTH WINDS AROUND 25 MPH INCREASING TO AROUND 35 MPH IN THE AFTERNOON.",
       ],
                    
    },
    {
    "name": "LE18",  # Wade and Ballard
    "commentary": "Local Effects: Wind (N10,N20) -> (N10,N30)",
    "createGrids": [
       ("Fcst", "Wind", "VECTOR", 0, 6,  (10,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 0, 6,  (20,"N"), ["BelowElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (10,"N"), ["AboveElev"]),
       ("Fcst", "Wind", "VECTOR", 6, 12, (30,"N"), ["BelowElev"]),
       ("Fcst", "WindGust", "SCALAR", 0, 12, 0, "all"),
       ],
    "checkStrings": [ 
       #"NORTH WINDS AROUND 25 MPH INCREASING TO AROUND 35 MPH IN THE AFTERNOON. NORTH WINDS AROUND 10 MPH IN THE MOUNTAINS.",
       "NORTH WINDS AROUND 25 MPH INCREASING TO AROUND 35 MPH IN THE AFTERNOON. IN THE MOUNTAINS...NORTH WINDS AROUND 10 MPH.",
       ], 
    },

    {
    "name": "LE19",
    "commentary": "Local Effects for non-intersecting areas -- CASE 3 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "NoWx", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "A 50 PERCENT CHANCE OF SHOWERS IN THE RUSH VALLEY...PATCHY FOG IN THE RUSH VALLEY...A 50 PERCENT CHANCE OF SNOW SHOWERS IN THE BENCHES...PATCHY FOG IN THE BENCHES.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE3,"undo"),
       ],
    "stringOrder": "yes", 
    },
    {
    "name": "LE20",
    "commentary": "Local Effects for non-intersecting areas -- CASE 3 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area2"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:SW:-:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "IN THE RUSH VALLEY...CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SHOWERS IN THE AFTERNOON.",
       "IN THE BENCHES...CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SNOW SHOWERS IN THE AFTERNOON.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE3,"undo"),
       ],
    "stringOrder": "yes", 
    },

    {
    "name": "LE21",
    "commentary": "Local Effects for non-intersecting areas -- CASE 3 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 12, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:T:<NoInten>:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 6, "Chc:T:<NoInten>:<NoVis>:", ["area2"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:RW:-:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 6, 12, "Chc:SW:-:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "IN THE CITY...A 50 PERCENT CHANCE OF THUNDERSTORMS.",
       "IN THE RUSH VALLEY...CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SHOWERS IN THE AFTERNOON.",
       "IN THE BENCHES...CHANCE OF THUNDERSTORMS IN THE MORNING...THEN CHANCE OF SNOW SHOWERS IN THE AFTERNOON.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE3,"undo"),
       ],
    "stringOrder": "yes", 
    },
    
    {
    "name": "LE22",
    "commentary": "Local Effects for non-intersecting areas -- CASE 2 for sub-phrase consolidation",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Patchy:F:<NoInten>:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "A 50 PERCENT CHANCE OF SHOWERS IN THE RUSH VALLEY...A 50 PERCENT CHANCE OF SNOW SHOWERS IN THE BENCHES...CHANCE OF SHOWERS IN THE RUSH VALLEY...CHANCE OF SNOW SHOWERS IN THE BENCHES.",
       "PATCHY FOG.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE3,"undo"),
       ],
    "stringOrder": "yes", 
    },
    {
    "name": "LE23",
    "commentary": "Local Effects for non-intersecting areas",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "NoWx", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "A 50 PERCENT CHANCE OF SHOWERS IN THE RUSH VALLEY...A 50 PERCENT CHANCE OF SNOW SHOWERS IN THE BENCHES.",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE3,"undo"),
       ],
    "stringOrder": "yes", 
    },
    {
    "name": "LE24",
    "commentary": "Local Effects for non-intersecting areas -- no consolidation necessary",
    "createGrids": [
       ("Fcst", "Sky", "SCALAR", 0, 48, 30, "all"),
       ("Fcst", "PoP", "SCALAR", 0, 48, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:RW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area1"]),
       ("Fcst", "Wx", "WEATHER", 0, 48, "Chc:SW:-:<NoVis>:^Patchy:F:<NoInten>:<NoVis>:", ["area2"]),
       ],
    "checkStrings": [
       "MOSTLY SUNNY.",
       "A 50 PERCENT CHANCE OF SHOWERS IN THE CITY AND IN THE RUSH VALLEY...A 50 PERCENT CHANCE OF SNOW SHOWERS IN THE BENCHES",
       ],
    "fileChanges": [
       ("LE_Test_Local", "TextProduct", "replace", periodLE3,"undo"),
       ],
    "stringOrder": "yes", 
    },

    ]


import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issuance Type', 'issuanceType'): 'ROUTINE', ('Issued By', 'issuedBy'): None}",
        "productType": "LE_Test_Local",
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)



