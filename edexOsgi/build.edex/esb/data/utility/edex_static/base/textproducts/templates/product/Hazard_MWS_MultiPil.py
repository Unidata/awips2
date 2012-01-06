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
# Hazard_MWS.py
#
#
##########################################################################
import GenericHazards
import string, time, re, os, types, copy

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_MWS_<MultiPil> (Marine Weather Statement)"

    Definition["defaultEditAreas"] = "EditAreas_MarineZones_<site>"
    Definition["mapNameForCombinations"] = "Marine_Zones_<site>_<MultiPil>" # Map background for creating Combinations

    # Header configuration items
    Definition["productName"] = "MARINE WEATHER STATEMENT"  # name of product
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "STATENAME"  # Name of state, such as "GEORGIA"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/MWS_<MultiPil>.txt"

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["debug"] = 1
    #Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    #Definition["purgeTime"] = 6        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 0    # Cities included in area header
    Definition["cityDescriptor"] = "INCLUDING THE CITIES OF"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    Definition["lineLength"] = 66 # Zone names will be included in the area header
    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header

    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

    Definition['includeOverviewHeadline'] = 0   #must be 0 to be compliant
    Definition['includeOverview'] = 0    #must be 0 to be compliant
   

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    #
    # These are the products allowed in the Marine Weather Statements

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('MA.S', allActions, 'MarineStatement'), # MARINE STATEMENT
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
        # Clean up and return
        #
        
        fcst = self.endline(fcst, linelength=self._lineLength, breakStr=[" ", "-", "..."])
        return fcst
 
    
