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
# Hazard_MWW.py
#
##
##########################################################################
import GenericHazards
import string, time, re, os, types, copy

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_MWW_<MultiPil> (Marine Weather)"

    Definition["defaultEditAreas"] = "EditAreas_MarineZones_<site>_<MultiPil>"
    Definition["mapNameForCombinations"] = "Marine_Zones_<site>" # Map background for creating Combinations

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
    Definition["productName"] = "URGENT - MARINE WEATHER MESSAGE"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "Statename"  # Name of state, such as "Georgia"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/MWW_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["debug"] = 1
    #Definition["headlineEditAreaGroup"] = "MZones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8        # Maximum hours for expireTime from issueTime
    #Definition["includeCities"] = 1    # Cities included in area header
    #Definition["cityDescriptor"] = "Including the cities of"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    Definition["lineLength"] = 66
    Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    Definition["includeOverview"] = 1   #If 1, the overview section is templated
    Definition["bulletProd"] = 1   #If 1, the product has a bullet format

    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    
    ###
    ### Text to insert below the last $$ of the product (WFO URL)
    ### use "" if you do not want text to appear
##    Definition["urlText"] = "http://www.weather.gov/miami"
    ### no additional text example
    Definition["urlText"] = ""
    ### multiple line example
##    Definition["urlText"] = "For more information from NOAA/s National Weather Service visit...\n" + \
##                            "http://weather.gov/saltlakecity"
    ###    

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [

            ('HU.W',  allActions, 'Tropical'), # HURRICANE WARNING
            ('TR.W',  allActions, 'Tropical1'),# TROPICAL STORM WARNING
            ('GL.W',  allActions, 'Marine3'),  # GALE WARNING
            ('HF.W',  allActions, 'Marine'),   # HURRICANE FORCE WIND WARNING
            ('MH.W',  allActions, 'Ashfall'),  # VOLCANIC ASHFALL WARNING
            ('SE.W',  allActions, 'Marine4'),  # HAZARDOUS SEAS
            ('SR.W',  allActions, 'Marine'),   # STORM WARNING
            ('UP.W',  allActions, 'IceAccr'),  # HEAVY FREEZING SPRAY WARNING

            ('MH.Y', allActions, 'Ashfall'),   # VOLCANIC ASHFALL ADVISORY
            ('BW.Y', allActions, 'Marine'),    # BRISK WIND ADVISORY
            ('MF.Y', allActions, 'Fog'),       # DENSE FOG ADVISORY
            ('LO.Y', allActions, 'LowWater'),  # LOW WATER ADVISORY
            ('RB.Y', allActions, 'Marine1'),   # ROUGH BAR
            ('SI.Y', allActions, 'Marine2'),   # SMALL CRAFT ADVISORY
            ('SC.Y', allActions, 'Marine3'),   # SMALL CRAFT ADVISORY
            ('MS.Y', allActions, 'Smoke'),     # DENSE SMOKE ADVISORY
            ('SW.Y', allActions, 'Marine4'),   # SMALL CRAFT ADVISORY
            ('UP.Y', allActions, 'IceAccr'),   # HEAVY FREEZING SPRAY ADVISORY

            ('HU.A',  allActions, 'Tropical'), # HURRICANE WATCH
            ('TR.A',  allActions, 'Tropical1'),# TROPICAL STORM WATCH
            ('GL.A',  allActions, 'Marine3'),  # GALE WATCH
            ('HF.A',  allActions, 'Marine'),   # HURRICANE FORCE WIND WATCH
            ('SE.A',  allActions, 'Marine4'),  # HAZARDOUS SEAS WATCH
            ('SR.A',  allActions, 'Marine'),   # STORM WATCH
            ('UP.A',  allActions, 'IceAccr'),  # HEAVY FREEZING SPRAY WATCH

           ]

    def _bulletDict(self):
        return {
            "HU" : ("Winds,Waves/seas"),                 ### hurricane warning, watch
            "TR" : ("Winds,Waves/seas"),                 ### tropical storm warning, watch
            "GL" : ("Winds,Waves/seas"),                 ### gale warning, watch
            "HF" : ("Winds,Waves/seas"),                 ### hurricane force wind warnings, watch
            "MH" : ("Volcanic ash info"),                ### volcanic ashfall warning, advisory
            "SE" : ("Waves/seas"),                       ### hazardous seas warning, watch
            "SR" : ("Winds,Waves/seas"),                 ### storm warning, watch
            "UP" : ("Ice accumulations"),                ### heavy freezing spray warnings, advisory, watch
            "BW" : ("Winds"),                            ### brisk wind advisory
            "MF" : ("Visibility"),                       ### dense fog advisory
            "LO" : ("Water levels"),                     ### low water advisory
            "RB" : ("Waves/seas,First ebb,Second ebb"),  ### small craft advisory for rough bar
            "SI" : ("Winds"),                            ### small craft advisory for winds
            "SC" : ("Winds,Waves/seas"),                 ### small craft advisory
            "MS" : ("Visibility"),                       ### dense smoke advisory
            "SW" : ("Waves/seas"),                       ### small craft advisory hazardous seas
            }

    def _bulletOrder(self):
        return [
            "Winds",
            "Waves/seas",
            "Volcanic ash info",
            "Ice accumulations",
            "First ebb",
            "Second ebb",
            "Water levels",
            "Visibility",
            ]


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
        
        self.overviewText(listOfHazards, "MWW")

        #
        # Clean up and return
        #
        
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst


    def _postProcessProduct(self, fcst, argDict):
        #
        # If an overview exists for this product, insert it
        #
        overview = self.finalOverviewText()
        overviewSearch = re.compile(r'Default overview section', re.DOTALL)
        fcst = overviewSearch.sub(overview, fcst)
        # Added to place line feeds in the CAP tags to keep separate from CTAs

        fcst = string.replace(fcst, \
                              r"PRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.", \
                              r"\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\n")
        fcst = string.replace(fcst, ".:", ".")
        fcst = string.replace(fcst, "\n ","\n")
        fcst = string.replace(fcst, "&&", "\n&&\n")

        # Prevent empty Call to Action Tags
        fcst = re.sub(r'\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\s*&&\n', \
                      "", fcst)
        ### to remove any empty framing code
        fcst = re.sub("\|\*\s*\*\|", "", fcst)


        ### indent the bullet text
        fcst = self._indentBulletText(fcst)


        #
        # Clean up multiple line feeds
        #

        fixMultiLF = re.compile(r'(\n\n)\n*', re.DOTALL)
        fcst = fixMultiLF.sub(r'\1', fcst)

        #
        # Finish Progress Meter
        #

        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        ### add the url text from the configuration section
        fcst = fcst + "\n" + self._urlText

        return fcst
