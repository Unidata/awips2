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
########################################################################
# Hazard_WSW.py
#
#
##########################################################################
import GenericHazards
import string, time, re, os, types, copy

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_WSW_<MultiPil> (Winter Wx Product)"

    Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"
    Definition["mapNameForCombinations"] = "Zones_<site>" # Map background for creating Combinations

    #Special multiple product domains for certain sites:
    if "<site>" == "AFG":
        if "_<MultiPil>" == "_AFG":
            Definition["subDomainUGCs"] = ["AKZ218","AKZ219","AKZ220","AKZ221",
                                           "AKZ222","AKZ223","AKZ224","AKZ225",
                                           "AKZ226"]
        elif "_<MultiPil>" == "_NSB":
            Definition["subDomainUGCs"] = ["AKZ201","AKZ202","AKZ203","AKZ204",
                                           "AKZ205","AKZ206"]
        elif "_<MultiPil>" == "_WCZ":
            Definition["subDomainUGCs"] = ["AKZ207","AKZ208","AKZ209","AKZ210",
                                           "AKZ211","AKZ212","AKZ213","AKZ214",
                                           "AKZ215","AKZ216","AKZ217","AKZ227"]


    # Header configuration items
    Definition["productName"] = "URGENT - WINTER WEATHER MESSAGE"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "STATENAME"  # Name of state, such as "GEORGIA"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/WSW_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    Definition["debug"] = 1
    #Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 1    # Cities included in area header
    Definition["accurateCities"] = 0  # All cities are included in header;
                                      # if 1, cities are based on grids
    Definition["cityLocation"] = "CityLocation" # City lat/lon dictionary to use
    Definition["cityDescriptor"] = "INCLUDING THE CITIES OF"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    Definition["lineLength"] = 66 # Zone names will be included in the area header
    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    Definition["includeOverview"] = 1   #If 1, the overview section is templated
    Definition["bulletProd"] = 1   #If 1, the product is bulletted

    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    #
    # These are the products allowed in the Winter Weather Products
    #

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
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
        ]

    #####################################################################
    ###
    ### Bullet Configuration Section
    ###
    ### Set the default bullets
    def _bulletDict(self):
        return {
        "WS" : ("TIMING,MAIN IMPACT,OTHER IMPACTS"),         ## Winter Storm  
        "WW" : ("TIMING,MAIN IMPACT,OTHER IMPACTS"),         ## Winter Weather
        "LE" : ("TIMING,SNOW ACCUMULATIONS,OTHER IMPACTS"),  ## Lake Effect
        "BZ" : ("TIMING,WINDS/VISIBILITY,SNOW ACCUMULATIONS"),## Blizzard
        "ZR" : ("TIMING,ICE ACCUMULATIONS,OTHER IMPACTS"),   ## Freezing Rain
        "IS" : ("TIMING,ICE ACCUMULATIONS,OTHER IMPACTS"),   ## Ice Storm
        "WC" : ("WIND CHILL VALUES,OTHER IMPACTS"),          ## Wind Chill
        }

    def _bulletOrder(self):
        return [
            "TIMING",
            "WINDS/VISIBILITY",
            "SNOW ACCUMULATIONS",
            "ICE ACCUMULATIONS",
            "WIND CHILL VALUES",
            "MAIN IMPACT",
            "OTHER IMPACTS",
            ]

    ###
    ### end configuration section
    #####################################################################
    
    #
    # Overridden to allow for attribution statement
    #

    def _makeProduct(self, fcst, segmentAreas, argDict):
        argDict["language"] = self._language

        #
        # This section generates the headline on the segment
        #
        # stuff argDict with the segmentAreas for DiscretePhrases
        argDict['segmentAreas'] = segmentAreas

        editArea = segmentAreas[0]
        areaLabel = editArea
        headlines = self.generateProduct("Hazards", argDict, area = editArea,
                                         areaLabel=areaLabel,
                                         timeRange = self._timeRange)
        fcst = fcst + headlines

        #
        # This section generates the attribution statements and calls-to-action
        #

        hazardsC = argDict['hazards']
        listOfHazards = hazardsC.getHazardList(segmentAreas)
        fcst = fcst + self.hazardBodyText(listOfHazards, argDict)

        #
        # If an overview exists for this product, calculate it
        #
        self.overviewText(listOfHazards, "WSW")


        #
        # Clean up and return
        #
        
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst

