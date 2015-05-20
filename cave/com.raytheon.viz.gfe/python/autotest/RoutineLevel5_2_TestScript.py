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
# Miscellaneous Tests for various products
# Can have "definitions" and "overrides"
#
# Author: Dave Metze, hansen
# ----------------------------------------------------------------------------
###########
## FWF LAL
## "editAreaSuffix" Test
## Humidity Recovery Local Effects
import TestScript, CreateGrids

# Overrides to FWF for Humidity Recovery Local Effects
humRec = """

    def getFirePeriod_phraseList(self):
        if self._useRH:
            dayRH = "RH"
            nightRH = "RH"
        else:
            dayRH = "MinRH"
            nightRH = "MaxRH"
        phraseList =  [
            self.skyWeather_byTimeRange_compoundPhrase,
            self.lal_phrase,
            (self.dayOrNight_phrase, ["MaxT", "MinT", 1, 1],
             self._tempLocalEffects_list()),
            (self.trend_DayOrNight_phrase, ["MaxT", "MinT", "Ttrend", 1, 1],
             self._tempTrendLocalEffects_list()),
            (self.dayOrNight_phrase, [dayRH, nightRH, 1, 1], self._rhLocalEffects_list()),
            (self.trend_DayOrNight_phrase, [dayRH, nightRH, "RHtrend", 1, 1],
             self._rhTrendLocalEffects_list()),
            (self.humidityRecovery_phrase, self._humRecLocalEffects_list()),
            #self.humidityRecovery_phrase,
            self.fireWind_compoundPhrase,
            self.fireWind_label_phrase,  
            self.fireValleyWind_compoundPhrase,  
            self.fireRidgeWind_compoundPhrase,  
            self.haines_phrase,
            self.smokeDispersal_phrase,
            self.mixingHgt_phrase,
            self.transportWind_phrase,
            #self.freeWind_phrase,  
            self.cwr_phrase,
            #self.marineLayer_phrase,
            ]
        # Remove trend methods
        if self._includeTrends != 1:
            newList = []
            for phrase in phraseList:
                if type(phrase) is types.TupleType:
                    phraseMethod = phrase[0]
                    if phraseMethod == self.trend_DayOrNight_phrase:
                       continue
                newList.append(phrase)
            phraseList = newList
        # Add multipleElementTable
        if self._includeMultipleElementTable_perPeriod:
            phraseList.append(self.multipleElementTable_perPeriod_phrase)
        return phraseList
            
    def _humRecLocalEffects_list(self):
        leArea1 = self.LocalEffectArea("Valleys", "")
        leArea2 = self.LocalEffectArea("Ridges", " on the ridges")
        return [self.LocalEffect([leArea1, leArea2], self._humRecTrigger, "...except ")]

    def _humRecTrigger(self, tree, node, localEffect, leArea1Label, leArea2Label):
        leNode = self._makeLEnode(tree, node, leArea1Label)
        self.humidityRecovery_words(tree, leNode)
        words1 = leNode.get("words")
        leNode = self._makeLEnode(tree, node, leArea2Label)
        self.humidityRecovery_words(tree, leNode)
        words2 = leNode.get("words")
        if words1 == words2:
            return 0
        else:
            return 1

    def _makeLEnode(self, tree, node, leArea):
        leNode = tree.makeNode([],[],node.parent)
        timeRange = node.getTimeRange()
        if self._useRH:
            elementName = "RH"
        else:
            elementName = "MaxRH"
        stats = tree.stats.get(
            elementName, timeRange, leArea, mergeMethod="Average")
        statDict = {}
        statDict[elementName] = stats
        leNode.set("statDict", statDict)
        leNode.set("timeRange", timeRange)
        return leNode

    # Need to add humidity recovery local effect areas
    def getFirePeriod_intersectAreas(self):
        tempList = []
        windList = []
        if self._tempLocalEffects:
            tempList = [
                ("MinT", ["BelowElev", "AboveElev"]),
                ("MaxT", ["BelowElev", "AboveElev"]),
                ("MinRH", ["BelowElev", "AboveElev"]),
                ("MaxRH", ["BelowElev", "AboveElev"]),
                ("RH", ["BelowElev", "AboveElev"]),
                ("Ttrend", ["BelowElev", "AboveElev"]),
                ("RHtrend", ["BelowElev", "AboveElev"]),
                ]
        if self._windLocalEffects:
            windList = [
                ("Wind", ["Valleys", "Ridges"]),
                ("Wind20ft", ["Valleys", "Ridges"]),
                ("WindGust", ["Valleys", "Ridges"]),
                ]
        humRecList = [
               ("RH", ["Valleys", "Ridges"]),
               ("MaxRH", ["Valleys", "Ridges"]),
               ]
        return tempList + windList + humRecList

"""


localHazard = """

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        tropicalActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", 
          "EXP"]
        return [
            ('LocalHazard1', allActions, 'Convective'),   # SEVERE THUNDERSTORM WATCH
             ]
"""

hazardHook = """
    def hazard_hook(self, tree, node, hazardPhen, hazardSig, hazardAct,
                    hazardStart, hazardEnd):
        hazTR = self.makeTimeRange(hazardStart, hazardEnd)
        phenSig = hazardPhen + "." + hazardSig
        print "hazTr, phenSig", hazTR, phenSig
        return "in the mountains"

"""

localHeadlinesTiming1 = """  

    def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
        return "NONE", "FUZZY4"

"""
localHeadlinesTiming2 = """

    def startPhraseMethod(self, issueTime, eventTime, timeZone, timeType):
        return "NONE", None
    
    def endPhraseMethod(self, issueTime, eventTime, timeZone, timeType):
        return "FUZZY4", self.timingWordTableFUZZY4(issueTime, eventTime, timeZone, timeType)

    def headlinesTiming(self, tree, node, key, timeRange, areaLabel, issuanceTime):
        return (self.startPhraseMethod, self.endPhraseMethod)

"""

scripts = [
    
    ### FWF LAL
    
##########
#LAL of 1#
##########

    {
    "name":"FWF_LAL1",
    "commentary": "LAL 1 from LAL1, PoP 0, NoWx",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "LAL.................1",".TONIGHT..."],
    "createGrids": [
       ("Fcst", "LAL", "SCALAR", 0, 12,  1, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 0, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
     ],
    },

    {
    "name":"FWF_LAL2", 
    "commentary": "LAL 1 from LAL1, PoP 10, SChc RW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "LAL.................1",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  1, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 10, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:RW:-:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL3", 
    "commentary": "LAL 1 from LAL1, PoP 20, Iso RW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...",
         "Isolated showers",
         "LAL.................1",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  1, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:RW:-:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL4", 
    "commentary": "LAL 1 from LAL1, PoP 30, Chc RW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...",
         ("Chance of rain showers", "Chance of showers"),
         "LAL.................1",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  1, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:RW:-:<NoVis>:", "all"),
     ],
    },
  


##########
#LAL of 2#
##########

     {
    "name":"FWF_LAL5", 
    "commentary": "LAL 2 from LAL 2, PoP 20, SChc T SChc RW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Slight chance of showers and thunderstorms",
         "LAL.................2",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", "all"),
     ],
    },


     {
    "name":"FWF_LAL6", 
    "commentary": "LAL 2 from LAL 2, PoP 20, Iso TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "LAL.................2",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:T:<NoInten>:<NoVis>:^Iso:RW:-:<NoVis>:", "all"),
     ],
    },

   
##########
#LAL of 3#
##########

    {
    "name":"FWF_LAL7", 
    "commentary": "LAL 3 from LAL 3, PoP 20, SChc TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Slight chance of showers and thunderstorms",
         "LAL.................3",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  3, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", "all"),
     ],
    },


    {
    "name":"FWF_LAL8", 
    "commentary": "LAL 3 from LAL 3, PoP 20, Iso TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Isolated showers and thunderstorms",
         "LAL.................3", ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  3, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:T:<NoInten>:<NoVis>:^Iso:RW:-:<NoVis>:", "all"),
     ],
    },


##########   
#LAL of 4#
##########

    {
    "name":"FWF_LAL9", 
    "commentary": "LAL 4 from LAL 4, PoP 40, Chc TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Chance of showers and thunderstorms",
         "LAL.................4",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:T:<NoInten>:<NoVis>:^Chc:RW:-:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL10", 
    "commentary": "LAL 4 from LAL 4, PoP 40, Sct TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Scattered showers and thunderstorms",
         "LAL.................4",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 40, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", "all"),
     ],
    },


##########   
#LAL of 5#
##########

    {
    "name":"FWF_LAL11", 
    "commentary": "LAL 5 from LAL 5, PoP 60, Lkly TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Showers and thunderstorms likely",
         "LAL.................5",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  5, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Lkly:T:<NoInten>:<NoVis>:^Lkly:RW:-:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL12", 
    "commentary": "LAL 5 from LAL 5, PoP 60, Num TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Numerous showers and thunderstorms",
         "LAL.................5",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  5, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:T:<NoInten>:<NoVis>:^Num:RW:-:<NoVis>:", "all"),
     ],
    },


##########   
#LAL of 6#
##########

    {
    "name":"FWF_LAL13", 
    "commentary": "LAL 6 from LAL 6, PoP 20, SChc T (Dry)",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Slight chance of dry thunderstorms",
         "LAL.................6",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  6, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "SChc:T:<NoInten>:<NoVis>:Dry", "all"),
     ],
    },

    {
    "name":"FWF_LAL14", 
    "commentary": "LAL 6 from LAL 6, PoP 20, Iso T (Dry)",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Isolated dry thunderstorms",
         "LAL.................6",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  6, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:T:<NoInten>:<NoVis>:Dry", "all"),
     ],
    },



#########################################################
# More Complex Cases - temporally, spatially and weather#
#########################################################

    #----------------------
    # changing through time
    #----------------------

    {
    "name":"FWF_LAL15", 
    "commentary": "Changing LAL 1->2, PoP 0->20, NoWx->IsoTRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Isolated showers and thunderstorms after 0900",
         "LAL.................1 until 0900...then 2",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 3,  1, "all", 1),
       ("Fcst", "LAL", "SCALAR", 3, 12,  2, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3, 0, "all", 1),
       ("Fcst", "PoP", "SCALAR", 3, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 3, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Iso:T:<NoInten>:<NoVis>:^Iso:RW:-:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL16", 
    "commentary": "Changing LAL 1->2, PoP 0->20, NoWx->SChcTRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Slight chance of showers and thunderstorms after 0900",
         "LAL.................1 until 0900...then 2",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 3,  1, "all", 1),
       ("Fcst", "LAL", "SCALAR", 3, 12,  2, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3, 0, "all", 1),
       ("Fcst", "PoP", "SCALAR", 3, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 3, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12,
        "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL17", 
    "commentary": "Changing LAL 3->4, PoP 20->40, Iso TRW- -> Sct RW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Isolated showers and thunderstorms until 1100...then scattered showers and thunderstorms",
         "LAL.................3 until 1100...then 4",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 5,  3, "all", 1),
       ("Fcst", "LAL", "SCALAR", 5, 12,  4, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 5, 20, "all", 1),
       ("Fcst", "PoP", "SCALAR", 5, 12, 40, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 5,
        "Iso:T:<NoInten>:<NoVis>:^Iso:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 5, 12,
        "Sct:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", "all"),
     ],
    },


    {
    "name":"FWF_LAL18", 
    "commentary": "Changing LAL 5->3, PoP 70->20, Lkly TRW- -> SChc RW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...",
         "Showers and thunderstorms likely until 1500...then slight chance of showers and thunderstorms",
         "LAL.................5 until 1500...then 3",
         ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 9,  5, "all", 1),
       ("Fcst", "LAL", "SCALAR", 9, 12,  3, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 9, 70, "all", 1),
       ("Fcst", "PoP", "SCALAR", 9, 12, 20, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 9, "Lkly:T:<NoInten>:<NoVis>:^Lkly:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
        "SChc:T:<NoInten>:<NoVis>:^SChc:RW:-:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL19", 
    "commentary": "Changing LAL 1->2->4, PoP 5->20->50, NoWx -> Iso TRW- -> Sct TRW-",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...",
         "Isolated showers and thunderstorms after 0900 until 1300...then scattered showers and thunderstorms",
         "LAL.................1 until 0900...then 2 until 1300...then 4",
         ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 3,  1, "all", 1),
       ("Fcst", "LAL", "SCALAR", 3, 7,  2, "all", 1),
       ("Fcst", "LAL", "SCALAR", 7, 12,  4, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 3, 5, "all", 1),
       ("Fcst", "PoP", "SCALAR", 3, 7, 20, "all", 1),
       ("Fcst", "PoP", "SCALAR", 7, 12, 50, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 3, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 7, "Iso:T:<NoInten>:<NoVis>:^Iso:RW:-:<NoVis>:", "all"),
       ("Fcst", "Wx", "WEATHER", 7, 12, "Sct:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", "all"),

     ],
    },


    #-------------------------
    # Varous weather occurring
    #-------------------------

    {
    "name":"FWF_LAL20", 
    "commentary": "LAL4, PoP 60, Num RW- Sct T",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Numerous showers and scattered thunderstorms",
         "LAL.................4",".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, "all", 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 60, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Num:RW:-:<NoVis>:^Sct:T:<NoInten>:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL21", 
    "commentary": "LAL3, PoP 30, Sct RW- Iso T",
    "productType":"FWF",
    "comboFlag": 1, 
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Scattered showers and isolated thunderstorms",
         "LAL.................3", ".TONIGHT..."],
    "createGrids" : [
      ("Fcst", "LAL", "SCALAR", 0, 12,  3, "all", 1),
      ("Fcst", "PoP", "SCALAR", 0, 12, 30, "all", 1),
      ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", "all"),
     ],
    },


    {
    "name":"FWF_LAL22", 
    "commentary": "LAL with local effects (2,4)",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Chance of showers and thunderstorms",
         "LAL.................4", ".TONIGHT..."],
    "createGrids" : [ 
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, ["BelowElev"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, ["AboveElev"], 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", "all"),
     ],
    },
    
    {
    "name":"FWF_LAL23", 
    "commentary": "LAL with local effects (2,2)",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Chance of showers and thunderstorms",
         "LAL.................2", ".TONIGHT..."],
    "createGrids": [ 
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, ["BelowElev"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, ["AboveElev"], 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Chc:RW:-:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", "all"),
     ],
    },
    
    {
    "name":"FWF_LAL24", 
    "commentary": "LAL with local effects (2,2), NoWx, results LAL1",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "LAL.................1", ".TONIGHT..."],
    "createGrids" : [ 
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, ["BelowElev"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, ["AboveElev"], 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "NoWx", "all"),
     ],
    },
    
    {
    "name":"FWF_LAL25", 
    "commentary": "LAL 1->4, NoWx -> Chc T",
    "productType":"FWF",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...","Chance of thunderstorms after 0900",
         "LAL.................1 until 0900...then 4", ".TONIGHT..."],
    "createGrids" : [ 
       ("Fcst", "LAL", "SCALAR", 0, 3,  1, "all", 1),
       ("Fcst", "LAL", "SCALAR", 3, 12,  4, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 3, "NoWx", "all"),
       ("Fcst", "Wx", "WEATHER", 3, 12, "Chc:T:<NoInten>:<NoVis>:", "all"),
     ],
    },

    {
    "name":"FWF_LAL26", 
    "productType":"FWF",
    "commentary": "Iso T does not get into wording",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Scattered showers",
         "LAL.................1", ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  1, ["area3"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, ["area3_pt"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["area3"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["area3_pt"], 1),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Sct:RW:-:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 12, "Iso:T:<NoInten>:<NoVis>:", ["area3_pt"]),
     ],
    }, 

    {
    "name":"FWF_LAL27", 
    "productType":"FWF",
    "commentary": "Matching Iso T, Takes Max LAL = 2 even tho for small area",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Scattered showers and isolated thunderstorms",
         "LAL.................2", ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  1, ["area3"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  2, ["area3_pt"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Sct:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["area3"]),
     ],
    },

    # Matching Iso T, can't find 3.
    # If the resultValue value (4) is greater than the binHighVal (3), use binHighVal 
    {
    "name":"FWF_LAL28", 
    "productType":"FWF",
    "commentary": "Matching Iso T, can't find 3. If the resultValue value (4) is greater than the binHighVal (3), use binHighVal", 
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Scattered showers and isolated thunderstorms",
         "LAL.................3", ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  1, ["area3"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, ["area3_pt"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Sct:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["area3"]),
     ],
    },

    {
    "name":"FWF_LAL29", 
    "productType":"FWF",
    "commentary": "Matching Iso T, LAL = 2",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Scattered showers and isolated thunderstorms",
         "LAL.................3", ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  3, ["area3"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, ["area3_pt"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, "all", 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Sct:RW:-:<NoVis>:^Iso:T:<NoInten>:<NoVis>:", ["area3"]),
     ],
    },

    
    {
    "name":"FWF_LAL30", 
    "productType":"FWF", 
    "commentary": "LAL (4,5), PoP (50,30), (Sct TRW-,Num TRW-)",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Scattered showers and thunderstorms",
         "LAL.................4", ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, ["area3"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  5, ["area3_pt"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, ["area3"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 30, ["area3_pt"], 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Sct:T:<NoInten>:<NoVis>:^Sct:RW:-:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 12,
        "Num:T:<NoInten>:<NoVis>:^Num:RW:-:<NoVis>:", ["area3_pt"]),
     ],
    },
    
    {
    "name":"FWF_LAL31", 
    "productType":"FWF", 
    "commentary": "LAL (4,6), PoP (50,20), (Chc TRW-,Chc TRW- Dry)",
    "comboFlag": 1,
    "orderStrings": 1,
    "checkStrings": [".TODAY...", "Chance of thunderstorms",
         "LAL.................4", ".TONIGHT..."],
    "createGrids" : [
       ("Fcst", "LAL", "SCALAR", 0, 12,  4, ["area3"], 1),
       ("Fcst", "LAL", "SCALAR", 0, 12,  6, ["area3_pt"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 50, ["area3"], 1),
       ("Fcst", "PoP", "SCALAR", 0, 12, 20, ["area3_pt"], 1),
       ("Fcst", "Wx", "WEATHER", 0, 12,"Chc:T:<NoInten>:<NoVis>:", ["area3"]),
       ("Fcst", "Wx", "WEATHER", 0, 12,"Chc:T:<NoInten>:<NoVis>:Dry", ["area3_pt"]),
     ],
    },
    
    #### editAreaSuffix
    {
    "name":"EditAreaSuffix1", 
    "productType":"ZFP",
    "commentary": "PoP 0%, Sky (0,75), (WS.W,None)",
    "comboFlag": 1,
     # checkStrings
    "checkStrings": [
       "WINTER STORM WARNING",
       ".TODAY...", "Mostly cloudy",
       ".TONIGHT...",
      ],  
    "orderStrings": 1,
     # createGrids
    "createGrids" : [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, ["area3"]),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "WS.W", ["area3"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "<None>", ["area3_pt"]),
     ],     
    "fileChanges": [("ZFP_<site>_Definition", "TextUtility", "add",
           "\nDefinition['editAreaSuffix'] = '_pt'\n", "delete")],
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },

    {  # Tests for TK4577
    "name":"EditAreaSuffix2", 
    "productType":"ZFP",
    "commentary": "Set up editAreaSuffix via commandline (tk4577)",
    "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, 'editAreaSuffix':'_pt'}",
    "comboFlag": 1,
     # checkStrings
    "checkStrings": [
       "WINTER STORM WARNING",
       ".TODAY...", "Mostly cloudy",
       ".TONIGHT...",
      ],  
    "orderStrings": 1,
     # createGrids
    "createGrids" : [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, ["area3"]),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "WS.W", ["area3"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "<None>", ["area3_pt"]),
     ],     
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },
    {  # Tests for TK4577
    "name":"EditAreaSuffix3", 
    "productType":"ZFP",
    "commentary": "Set up editAreaSuffix via commandline (tk4577)",
    "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None, ('Enter Edit Area Suffix if desired:', 'editAreaSuffix'):'_pt'}",
    "comboFlag": 1,
     # checkStrings
    "checkStrings": [
       "WINTER STORM WARNING",
       ".TODAY...", "Mostly cloudy",
       ".TONIGHT...",
      ],  
    "orderStrings": 1,
     # createGrids
    "createGrids" : [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, ["area3"]),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "WS.W", ["area3"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "<None>", ["area3_pt"]),
     ],     
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },

     # TK 4674
     {
    "name":"EditAreaSuffix4", 
    "productType":"ZFP",
    "commentary": """
         Edit Areas Suffix with Local Hazard using hazard_hook (tk4674)
         """,
    "comboFlag": 1,
    "checkStrings": [
       "...LOCAL HAZARD IN EFFECT THROUGH THIS AFTERNOON IN THE MOUNTAINS...",
       ".TODAY...", "Mostly cloudy",
       ".TONIGHT...",
      ],  
    "orderStrings": 1,
    "createGrids" : [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, ["area3"]),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "LocalHazard1", ["area3"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "<None>", ["area3_pt"]),
     ],     
    "fileChanges": [
        ("ZFP_<site>_Definition", "TextUtility", "add",
        "\nDefinition['editAreaSuffix'] = '_pt'\n", "delete"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", localHazard, "undo"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", hazardHook, "undo"),
        ],
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },
     
    {
    "name":"EditAreaSuffix5", 
    "productType":"ZFP",
    "commentary": """
         Edit Areas Suffix with Local Hazard calling "getLocalHeadlinesTiming"
         instead of using hard-coded phrasing for local headlines
         """,
    "comboFlag": 1,
    "checkStrings": [
       "...LOCAL HAZARD IN EFFECT THROUGH THIS AFTERNOON...",
       ".TODAY...", "Mostly cloudy",
       ".TONIGHT...",
      ],  
    "orderStrings": 1,
    "createGrids" : [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, ["area3"]),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "LocalHazard1", ["area3"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "<None>", ["area3_pt"]),
     ],     
    "fileChanges": [
        ("ZFP_<site>_Definition", "TextUtility", "add",
        "\nDefinition['editAreaSuffix'] = '_pt'\n", "delete"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", localHazard, "undo"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", localHeadlinesTiming1, "undo"),
        ],
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },
    {
    "name":"EditAreaSuffix6", 
    "productType":"ZFP",
    "commentary": """
         Edit Areas Suffix with Local Hazard using methods for headinesTiming
         """,
    "comboFlag": 1,
    "checkStrings": [
       "...LOCAL HAZARD IN EFFECT THROUGH THIS AFTERNOON...",
       ".TODAY...", "Mostly cloudy",
       ".TONIGHT...",
      ],  
    "orderStrings": 1,
    "createGrids" : [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, ["area3"]),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "<None>", ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "LocalHazard1", ["area3"]),
     ],     
    "fileChanges": [
        ("ZFP_<site>_Definition", "TextUtility", "add",
        "\nDefinition['editAreaSuffix'] = '_pt'\n", "delete"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", localHazard, "undo"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", localHeadlinesTiming2, "undo"),
        ],
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },



    # Allow lower case TK 4669 
    {
    "name":"LowerCaseHeadlines", 
    "productType":"ZFP",
    "commentary": """
         Lower case with Local Hazard
         """,
    "comboFlag": 1,
     # checkStrings
    "checkStrings": [
       "...LOCAL HAZARD IN EFFECT THROUGH THIS AFTERNOON...",
       ".TODAY...", "Mostly cloudy. Patchy dense fog.",
       "Ice accumulation around 2 inches.",
       # DR_18363 "Highs in the upper 70s.",
       "Near steady temperature in the mid 50s.",
       ".TONIGHT...", "Very windy.", "Widespread thunderstorms.",
       # DR_18363 "Lows around 60.",
       "Near steady temperature in the mid 40s.",
       ],  
    "orderStrings": 1,
    "createGrids" : CreateGrids.Public_createGrids + [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, ["area3"]),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, ["area3_pt"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "LocalHazard1", ["area3"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "<None>", ["area3_pt"]),
     ],     
    "fileChanges": [
        ("ZFP_<site>_Definition", "TextUtility", "add",
        "\nDefinition['editAreaSuffix'] = '_pt'\n", "delete"),
        ("ZFP_<site>_Definition", "TextUtility", "add",
        "\nDefinition['lowerCase'] = 1\n", "undo"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", localHazard, "undo"),
        ("ZFP_<site>_Overrides", "TextUtility", "add", localHeadlinesTiming1, "undo"),
        ],
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },
    {
    "name":"LowerCaseHazards", 
    "productType":"ZFP",
    "commentary": """
         Lower Case with a Hazard
         """,
    "comboFlag": 1,
     # checkStrings
    "checkStrings": [
       "...WINTER STORM WARNING IN EFFECT UNTIL 6 PM EST THIS EVENING...",
       ".TODAY...", "Mostly cloudy. Patchy dense fog.",
       "Ice accumulation around 2 inches.",
       # "Highs in the upper 70s.",
       "Near steady temperature in the mid 50s.",
       ".TONIGHT...", "Very windy.", "Widespread thunderstorms.",
       #"Lows around 60.",
       "Near steady temperature in the mid 40s.",
      ],  
    "orderStrings": 1,
     # createGrids
    "createGrids" : CreateGrids.Public_createGrids + [      
       ("Fcst", "PoP", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  0, "all"),
       ("Fcst", "Sky", "SCALAR", 0, 12,  75, "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 12,  "WS.W", "all"),
     ],     
    "fileChanges": [
        ("ZFP_<site>_Definition", "TextUtility", "add",
        "\nDefinition['lowerCase'] = 1\n", "delete"),
        ],
    "gridsStartTime": "6am Local",
    "drtTime": "4am Local",
     },
     
     ## Humidity Recovery Local Effect
     {
     "name":"FWF_HumRecLE1",
     "commentary": "Testing Humidity Recovery Local Effect: No Local Effect",
     "productType":"FWF",
     "comboFlag": 1,
     "orderStrings": 1,
     "checkStrings": ["HUMIDITY RECOVERY...Poor"],
     "createGrids": [
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin-24", "MaxRHEnd-24", 10, ["Valleys"], 1),
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin-24", "MaxRHEnd-24", 10, ["Ridges"], 1),
       ("Fcst", "MaxRH", "SCA16LAR", "MaxRHBegin", "MaxRHEnd", 20, ["Valleys"], 1),
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 20, ["Ridges"], 1),
       ],
       "fileChanges": [("FWF_<site>_Overrides", "TextUtility", "add", humRec, "delete")],
      },

     ## Humidity Recovery Local Effect
     {
     "name":"FWF_HumRecLE2",
     "commentary": "Testing Humidity Recovery Local Effect: Local Effect",
     "productType":"FWF",
     "comboFlag": 1,
     "orderStrings": 1,
     "checkStrings": ["HUMIDITY RECOVERY...Poor...except Excellent on the ridges."],
     "createGrids": [
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin-24", "MaxRHEnd-24", 10, ["Valleys"], 1),
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin-24", "MaxRHEnd-24", 10, ["Ridges"], 1),
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 10, ["Valleys"], 1),
       ("Fcst", "MaxRH", "SCALAR", "MaxRHBegin", "MaxRHEnd", 75, ["Ridges"], 1),
       ],
       "fileChanges": [("FWF_<site>_Overrides", "TextUtility", "add", humRec, "delete")],
      },
      

      {    
      "name":"General_CleanUp",
      "commentary": "Clean out grids",
      "productType": None,
      "deleteGrids": TestScript.general_deleteGrids,
      "gridsStartTime": "6am Local",
      },

    ]     


import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" :"{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}",
        }
    # Necessary to do drt for scripts 
    # that start the product with current time
    time_4am = self.getAbsFromLocal(2010, 1, 1, 4, 0)
    time_6am = self.getAbsFromLocal(2010, 1, 1, 6, 0)
    for script in scripts:
        if script.get("gridsStartTime", None) == "6am Local":
            script["gridsStartTime"] = time_6am
        if script.get("drtTime", None) == "4am Local":
            script["drtTime"] = time_6am  # Should this be time_4am?
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)

