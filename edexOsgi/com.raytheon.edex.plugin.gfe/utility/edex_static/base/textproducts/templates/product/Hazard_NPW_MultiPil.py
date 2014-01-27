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
# Hazard_NPW.py
#
##
##########################################################################
import GenericHazards
import string, time, re, os, types, copy

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_NPW_<MultiPil> (Non-Precipitation)"

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
    Definition["productName"] = "URGENT - WEATHER MESSAGE"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "STATENAME"  # Name of state, such as "GEORGIA"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/NPW_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["debug"] = 1
    Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 1    # Cities included in area header
    Definition["accurateCities"] = 0  # If 1, cities are based on grids;
                                      # otherwise full list is included
    Definition["cityLocation"] = "CityLocation" # City lat/lon dictionary to use
    Definition["cityDescriptor"] = "INCLUDING THE CITIES OF"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    Definition["lineLength"] = 66
    Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    Definition["includeOverview"] = 1   #If 1, the overview section is templated
    Definition["bulletProd"] = 1   #If 1, the product will have a bulletted format
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('DS.W', allActions, 'Dust'),         # DUST STORM WARNING
            ('DU.Y', allActions, 'Dust'),         # BLOWING DUST ADVISORY
            ('EC.W', allActions, 'Cold'),         # EXTREME COLD WARNING
            ('EC.A', allActions, 'Cold'),         # EXTREME COLD WATCH
            ('EH.W', allActions, 'Heat'),         # EXCESSIVE HEAT WARNING
            ('EH.A', allActions, 'Heat'),         # EXCESSIVE HEAT WATCH
            ('HT.Y', allActions, 'Heat'),          # HEAT ADVISORY
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
            ('ZF.Y', allActions, 'FreezeFog'),     # FREEZING FOG ADVISORY
            ('AF.W', allActions, 'Ashfall'),       # VOLCANIC ASHFALL WARNING
            ('AF.Y', allActions, 'Ashfall'),       # ASHFALL ADVISORY
            ('AS.Y', allActions, 'AirStagnation'), # AIR STAGNATION ADVISORY
            ('AS.O', allActions, 'AirStagnation'), # AIR STAGNATION OUTLOOK
           ]
    
    #####################################################################
    ###
    ### Bullet Configuration Section
    ###
    ### set the default bullets
    def _bulletDict(self):
        return {
        "LW" : ("TIMING,WINDS,IMPACTS"),            ## Lake Wind
        "WI" : ("TIMING,WINDS,IMPACTS"),            ## Wind Advisory
        "HW" : ("TIMING,WINDS,IMPACTS"),            ## High Wind
        "DU" : ("TIMING,WINDS,VISIBILITY,IMPACTS"), ## Blowing Dust
        "DS" : ("TIMING,WINDS,VISIBILITY,IMPACTS"), ## Dust Storm
        "FG" : ("VISIBILITY,IMPACTS"),              ## Dense Fog
        "FZ" : ("TEMPERATURE,IMPACTS"),             ## Freeze
        "AF" : ("ASH INFO,IMPACTS"),                ## Ashfall
        "AS" : ("AIR QUALITY,IMPACTS"),             ## Air Stagnation
        "EH" : ("TEMPERATURE,IMPACTS"),             ## Excessive Heat
        "EC" : ("TEMPERATURE,IMPACTS"),             ## Excessive Cold
        "HZ" : ("TEMPERATURE,IMPACTS"),             ## Hard Freeze
        "ZF" : ("TIMING,VISIBILITY,IMPACTS"),       ## Freezing Fog
        "FR" : ("TEMPERATURE,IMPACTS"),             ## Frost
        "HT" : ("TEMPERATURE,IMPACTS"),             ## Heat
        "SM" : ("VISIBILITY,IMPACTS"),              ## Dense Smoke
        }

    def _bulletOrder(self):
        return [
            "TIMING",
            "ASH INFO",
            "WINDS",
            "VISIBILITY",
            "AIR QUALITY",
            "TEMPERATURE",
            "IMPACTS",
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
        # If an overview exists for this product, calculate  it
        #
        
        self.overviewText(listOfHazards, "NPW")

        #
        # Clean up and return
        #
        
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst

