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
# Hazard_CFW.py
#
#
##########################################################################
import GenericHazards
import string, time, re, os, types, copy

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "BaselineHazard_CFW_<MultiPil> (Coastal/LakeShore Flooding)"

    Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"
    Definition["mapNameForCombinations"] = "Zones_<site>" # Map background for creating Combinations

    # Header configuration items
    Definition["productName"] = "COASTAL HAZARD MESSAGE"  # Warning! DO NOT CHANGE.
    # The productName gets substituted later in the formatter!
    
    Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
    Definition["wmoID"] = "<wmoID>"        # WMO ID
    Definition["pil"] = "<pil>"          # product pil
    #Definition["areaName"] = "STATENAME"  # Name of state, such as "GEORGIA"
    Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
    Definition["wfoCity"] = "<wfoCity>"       # WFO Name as it should appear in a text product
    Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
    Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
    Definition["outputFile"] =  "{prddir}/TEXT/CFW_<MultiPil>.txt"
    Definition["bulletProd"] = 1   #If 1, the product has a bullet format

    # OPTIONAL CONFIGURATION ITEMS
    #Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    Definition["debug"] = 1
    #Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 0    # Cities included in area header
    Definition["accurateCities"] = 0  # If 1, cities are based on grids;
                                      # otherwise full list is included
    Definition["cityLocation"] = "CityLocation" # City lat/lon dictionary to use
    #Definition["cityDescriptor"] = "INCLUDING THE CITIES OF"
    Definition["includeZoneNames"] = 1 # Zone names will be included in the area header
    Definition["lineLength"] = 66 # line length
    Definition["easPhrase"] = "URGENT - IMMEDIATE BROADCAST REQUESTED" 
    Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    Definition["includeOverview"] = 1   #If 1, the overview section is templated

    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    
        ###
    ### Text to insert below the last $$ of the product (WFO URL)
    ### use "" if you do not want text to appear
##    Definition["urlText"] = "HTTP://WWW.WEATHER.GOV/MIAMI"
    ### no additional text example
    Definition["urlText"] = ""
    ### multiple line example
##    Definition["urlText"] = "FOR MORE INFORMATION FROM NOAA/S NATIONAL WEATHER SERVICE VISIT...\n" + \
##                            "HTTP://WEATHER.GOV/SALTLAKECITY (ALL LOWER CASE)"
    ###

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    #
    # These are the products allowed in the Coastal Flood Products
    #

    def allowedHazards(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
            ('CF.W', allActions, 'CoastalFlood'),     # COASTAL FLOOD WARNING
            ('CF.Y', allActions, 'CoastalFlood'),     # COASTAL FLOOD ADVISORY 
            ('CF.A', allActions, 'CoastalFlood'),     # COASTAL FLOOD WATCH
            ('CF.S', allActions, 'CoastalFloodStatement'),     # COASTAL FLOOD STATEMENT
            ('LS.W', allActions, 'CoastalFlood'),     # LAKESHORE FLOOD WARNING
            ('LS.Y', allActions, 'CoastalFlood'),     # LAKESHORE FLOOD ADVISORY
            ('LS.A', allActions, 'CoastalFlood'),     # LAKESHORE FLOOD WATCH
            ('LS.S', allActions, 'CoastalFloodStatement'),     # LAKESHORE FLOOD STATEMENT
            ('SU.W', allActions, 'HighSurf'),         # HIGH SURF WARNING
            ('SU.Y', allActions, 'HighSurf'),         # HIGH SURF ADVISORY
        ]

    def _bulletDict(self):
        return {
            "CF" : ("COASTAL FLOODING,TIMING,IMPACTS"),  ### coastal flood warning, advisory, watch
            "LS" : ("LAKE SHORE FLOODING,TIMING,IMPACTS"),  ### lake shore flood warning, advisory, watch
            "SU" : ("WAVES AND SURF,TIMING,IMPACTS"),  ### high surf warning, advisory
               }


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
        
        self.overviewText(listOfHazards, "CFW")
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
        overviewSearch = re.compile(r'DEFAULT OVERVIEW SECTION', re.DOTALL)
        fcst = overviewSearch.sub(overview, fcst)


        urgent = 0
        followup = 1
        prodNameKey = ''
        fullKeyList = []
        newList = ['NEW', 'EXA', 'EXB']
        
        hazardsC = argDict['hazards']
        segmentList = self.organizeHazards(hazardsC.rawAnalyzedTable())
        for segmentAreas in segmentList:
            listOfHazards = hazardsC.getHazardList(segmentAreas)
            for eachHazard in listOfHazards:
                if eachHazard['phensig'] not in fullKeyList:
                        fullKeyList.append(eachHazard['phensig'])
                if eachHazard['phensig'] in ['CF.W', 'CF.A', 'LS.W', 'LS.A']:
                    if eachHazard['act'] in newList:
                        urgent = 1
                       
        # remove eas line if not urgent
        if urgent == 0 and len(self._easPhrase):
            fcst = fcst.replace(self._easPhrase + '\n', '', 1)
        
        # rename the product if necessary based on VTEC codes
        for each in fullKeyList:
            if each in ['LS.W', 'LS.A', 'LS.Y', 'LS.S']:
                    productName = "LAKESHORE HAZARD MESSAGE"
                    fcst = fcst.replace(self._productName, productName, 1) 
                    break

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
    

