########################################################################
# Hazard_TCVNHC.py
#-------------------------------------------------------------------------
# Description:  This product will generate a Tropical Cyclone
#  Watch/Warning (TCV) product for areas covered by the U.S. National
#  Weather Service.
#-------------------------------------------------------------------------
# Copying:
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#-------------------------------------------------------------------------
# Standard and Local file names and Locations:
# Hazard_TCV, Hazard_TCV_Definition
#-------------------------------------------------------------------------
# Version: 2015.4.7  (04/07/2015)
#-------------------------------------------------------------------------
# Weather Elements Needed:  Hazards
#-------------------------------------------------------------------------
# Edit Areas Needed:        One sample area for each segment between
#                           official NHC breakpoints
#-------------------------------------------------------------------------
# Programmers and Support including product team leader's email:
#  Matthew H. Belk   NOAA/NWS Taunton, MA    Matthew.Belk@noaa.gov
#-------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 07, 2015           mbelk     Initial creation
# Sep 19, 2016  19293    randerso  Initial baseline check in
# Oct 31, 2016  25946    randerso  Changed to keep Hazard_TCVNHC from 
#                                  overwriting Hazard_TCV
# Nov 11, 2016  19293    randerso  Fix issuing office line
#
########################################################################


import string, time, re, os, types, copy, sets, pprint

import GenericHazards
import LogStream
import TropicalHazards


#===============================================================================
#  Define a series of commands to get data for all available active storms
##class TextProduct(GenericHazards.TextProduct):
class TextProduct(TropicalHazards.TropicalHazards, GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition['displayName'] = "Hazard_<pil>"
    Definition["outputFile"] =  "{prddir}/TEXT/<pil>.txt"
    Definition["database"] = "Official"
    Definition["mapNameForCombinations"] = "Zones_<site>"
    Definition["defaultEditAreas"] = "Combinations_<MultiPil>_<site>"
    Definition["showZoneCombiner"] = 0

    # Header configuration items
    Definition["productName"] = "PROTOTYPE TCV"  # Warning! DO NOT CHANGE.
    # The productName gets substituted later in the formatter!

    Definition["fullStationID" ]    = "<fullStationID>"
    Definition["wmoID" ]            = "<wmoID>"
    Definition["wfoCityState" ]     = "<wfoCityState>"
    Definition["pil" ]              = "<pil>"
    Definition["textdbPil" ]        = "<textdbPil>"
    Definition["awipsWANPil" ]      = "<awipsWANPil>"
    Definition["site"]              = "<site>"
    Definition["wfoCity"]           = "<wfoCity>"

    # OPTIONAL CONFIGURATION ITEMS

    # Source database. "Official", "Fcst", or "ISC"
    #Definition["displayOutputDialog"] = 0  # If 1 will display results when finished
    #Definition["headlineEditAreaGroup"] = "Zones" # Name of EditAreaGroup for sampling headlines

    Definition["purgeTime"] = 8        # Maximum hours for expireTime from issueTime
    Definition["includeCities"] = 0    # Cities included in area header
    Definition["accurateCities"] = 0   # If 1, cities are based on grids;
                                       # otherwise full list is included
    Definition["cityLocation"] = ""    # City lat/lon dictionary to use
    #Definition["cityDescriptor"] = ""
    Definition["includeZoneNames"] = 0 # Zone names will be included in the area header
    Definition["lineLength"] = 68 # line length
    Definition["easPhrase"] = ""
    Definition["includeOverviewHeadline"] = 0   #If 1, the overview header is templated
    Definition["includeOverview"] = 0   #If 1, the overview section is template
    Definition["bulletProd"] = 0   #If 1, the product will have a bullet format

    ### Survey text to insert below the last $$ of the product
    Definition["urlText"] = ""
    ###

#     Definition["areaDictionary"] = "TropicalAreaDictionary"


    Definition["hazardSamplingThreshold"] = (3, None)  #(%cov, #points)


    Definition["debug"] = {"generateForecast": 0,
                           "_preProcessProduct":0,
                           "_makeProduct":0,
                           "_postProcessProduct":0,
                           "_formatTCVline":0,
#                            "":0,
                           }


    #===========================================================================
    #  Define a variableList to get answers to questions we need

#     VariableList = [(("Storm Name","stormName"),"","radio", stormNames)]


    def __init__(self):
        GenericHazards.TextProduct.__init__(self)
        TropicalHazards.TropicalHazards.__init__(self)


    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas

        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Get the sampled hazards
        self._sampledHazards = argDict["hazards"]

        print "="*70
        print "raw analyzed hazards"
        print pprint.pformat(self._sampledHazards.rawAnalyzedTable())
        print "="*70

        #  Get the combinations where there are any tropical hazards
        segmentList = self.organizeHazards(self._sampledHazards.rawAnalyzedTable())

        #  If there is nothing to do - leave now
        if len(segmentList) == 0:
            return "NO COASTAL TROPICAL WATCHES OR WARNINGS TO REPORT"

        #=======================================================================
        # Determine time ranges
        error = self._determineTimeRanges(argDict)

        if error is not None:
            return error

        #=======================================================================
        #  Now organize all hazards by phenomena and significance

        rawHazards = self._sampledHazards.rawAnalyzedTable()

        hazardPhenSig = \
            self._sampledHazards._HazardsTable__organizeByPhenSig(rawHazards)

        #=======================================================================
        # Initialize the output string

        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        #=======================================================================
        #  Make the shortcut hazard dictionary along with its sorted keys

        (self._hazardAreaDict, self._hazardAreaDictKeyList) = \
                     self._constructHazardDict(hazardPhenSig, self._filterETN)


        #=======================================================================
        #  Make a list of segments we need to process

        segmentList = self._constructSegmentList(self._hazardAreaDict,
                                                 self._hazardAreaDictKeyList)

        # Generate the product for each segment in the segmentList
        fraction = 0
        if len(segmentList) > 0:
            fractionOne = 1.0/float(len(segmentList))
        percent = 50.0
        self.setProgressPercentage(50)
        for segmentAreas in segmentList:

            #  Separate out the various edit area types
            (segments, ugcZones, zones, islands, water) = segmentAreas

            #  If this is the international TCV
            if "Intl" in self._displayName:

                #  Do not permit any UGC zones
                ugcZones = []

            self.debug_print("\n" + "*"*90, 1)
            self.debug_print("In generateForecast\n", 1)
            self.debug_print("segments:\n %s" % (pprint.pformat(segments)), 1)
            self.debug_print("ugcZones:\n %s" % (pprint.pformat(ugcZones)), 1)
            self.debug_print("zones:\n %s" % (pprint.pformat(zones)), 1)
            self.debug_print("islands:\n %s" % (pprint.pformat(islands)), 1)
            self.debug_print("water:\n %s" % (pprint.pformat(water)), 1)

            self.progressMessage(fraction, percent, "Making Product for Segment")

            #-------------------------------------------------------------------
            #  Decide which set of zones we want to process for header

            if len(ugcZones) > 0:
                activeZones = ugcZones
            else:
                activeZones = zones

            self.debug_print("+"*90, 1)
            self.debug_print("header zones =\n%s" %
                             (pprint.pformat(activeZones)), 1)

            #  If there are no zones to process - move on to the next segment
            if len(activeZones) == 0:
                print "No active zones - moving on"
                continue

            #  Format UGC header using the UGC zones
            fcst = self._preProcessArea(fcst, activeZones, self._expireTime,
                                        argDict)

            #-------------------------------------------------------------------
            #  Decide which set of areas we want to process for body

            # CHANGE to get zones, not breakpoint segments
            if len(ugcZones) > 0:
                activeAreas = ugcZones
            else:
                activeAreas = zones

            self.debug_print("body zones =\n%s" % (activeAreas), 1)

            #  Make the product body using the segment areas
            fcst  = self._makeProduct(fcst, activeAreas, argDict)

            #  Finish off the product block using the zones
            fcst = self._postProcessArea(fcst, activeAreas, argDict)

            fraction = fractionOne
        fcst = self._postProcessProduct(fcst, argDict)
        return fcst


    #
    #  Overridden to produce product header using info for specific storms
    #

    def _preProcessProduct(self, fcst, argDict):

        # Product header
        if self._areaName != "":
            self._areaName = " FOR " + self._areaName
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict,
                                         self._productName + self._areaName)

        #  Get the storm info that goes with this PIL
        pil = self._pil[-3:]
        self.debug_print("pil '%s'" % (pil), 1)

        # Pull needed storm info from the stormDict based on selected stormName
        stormDict = self._loadAdvisory(pil)

        #  if we do not have the correct storm information
        if stormDict is None:

            #  There is no point continuing
            fcst = "Could not find valid storm information for the " + \
                   "TCV%s product.  " % (pil) + \
                   "Please run the StormInfo procedure for this storm, " + \
                   "if needed, and try again."
            return fcst

        # Pull needed storm info from the stormDict based on selected stormName
        stormName = stormDict["stormName"]
        stormType = stormDict["stormType"]
        advType = stormDict["advisoryType"]
        advNum = stormDict["advisoryNumber"]

        #  Ensure the VTEC ETN is correct for a national center
        stormNum = stormDict["stormNumber"]
        self._filterETN = int(stormNum)
        if self._filterETN < 1000:
            self._filterETN += 1000

        self.debug_print("*" * 80, 1)
        self.debug_print("my ETN is: %s" % (self._filterETN), 1)
        self.debug_print("*" * 80, 1)

        #  Correct the WMO header based on selected PIL
        pilNumber = pil[-1:]
        wmoID = self._wmoID[:-1] + pilNumber

        #  Mofify the product name to include the advisory package info
        productName = re.sub("(?i)PROTOTYPE TCV",
                             "%s Watch/Warning Breakpoints" % (stormName) +
                             "/Routine Advisory Number %s" % (advNum),
                             productName)

        #  Handle intermediate or special advisories
        if advType in ["Special", "Intermediate"]:

            productName = re.sub("(?i)Routine", advType, productName)

        #  Otherwise, remove the "routine" wording
        else:

            productName = re.sub("(?i)Routine ", "", productName)


        #  Insert the EAS phrase - if it exists
        if len(self._easPhrase) != 0:
            eas = self._easPhrase + '\n'
        else:
            eas = ''

        #  Format a storm code for this product
        stormCode = "AL%02d%4d" % (int(stormNum), time.gmtime().tm_year)

        #  Set aside a variable to track impacted WFOs
        self._wfoList = []

        #  Construct the MND header text
        s = wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + "TCV" + pil + "\n\n"
        
        fcst = fcst + s.upper() +\
               eas + productName + "\n" +\
               "NWS " + self._wfoCityState + \
               "     %s\n" % (stormCode) + issuedByString + self._timeLabel + \
               "\n\n"

        fcst = fcst + ".%s %s\n\n" % (stormType, stormName)

        #  Now add the disclaimer
        fcst += """
Caution...this product only approximately conveys the extent of
tropical cyclone wind and surge watches and warnings. Please see
the latest public advisory from the National Hurricane Center for
the precise lateral extent of wind watches and warnings along the
coast...as well as the approximate lateral extent of surge watches
and warnings. The precise extent of surge watches and warnings
can be found in the NWS National Digital Forecast Database Hazard
grids.

"""

#---|----1----|----2----|----3----|----4----|----5----|----6----|----7
        #  Return the completed text we have so far
        return fcst

    #
    #  Overridden to produce breakpoint end points within each segment
    #

    def _makeProduct(self, fcst, segmentAreas, argDict):

        # Set the language
        argDict["language"] = self._language

        #  Make a list of any impacted WFOs from all breakpoint segments
        self._getAffectedWFOs(segmentAreas)

        #  Get ready to define the segment text for this group
        startLine = ""
        endLine = ""

        #===================================================================
        #  Determine if we just want to list the end points of this segment,
        #  or all of the segments in this list

        self.debug_print("\nLast chance, segmentAreas =\n%s" %
                         (pprint.pformat(segmentAreas)), 1)

        #  If this segment is something other than an island
        if len(segmentAreas) > 1:

            self.debug_print("using endpoints", 1)

            #  Start the display list with the first segment in this list
            displayList = [segmentAreas[0]]

            #  If the last segment in the list is a different segment
            if segmentAreas[-1] not in displayList:

                #  Add this segment as well
                displayList.append(segmentAreas[-1])

        #  Otherwise, display all of the points
        else:
            displayList = segmentAreas[:]

        self.debug_print("\ndisplayList =\n%s" %
                         (pprint.pformat(displayList)), 1)


        #  Do not include non-UGC codes in mainland USA TCV
        self.debug_print("displayName = '%s'" % (self._displayName), 1)
        if self._displayName.find("Intl") == -1 and displayList != [] and \
           len(displayList[0]) >= 3 and displayList[0][3] != "Z":

            self.debug_print("Do not display non-Z codes in USA TCV", 1)
            displayList = []


        #===================================================================
        #  Look through each zone of this hazard group

        for id in displayList:

            index = displayList.index(id)
            result = abs(len(displayList) - index)

            #  Get the entry for this area
            if self._tropicalAreaDict.has_key(id):
                entry = self._tropicalAreaDict[id]
            else:
                entry = {}
                LogStream.logProblem(\
                  "AreaDictionary missing definition for [" + id + "].")

            #---------------------------------------------------------------
            #  If we have not already constructed the info for the starting
            #  point of this segment, or this is an island point

            if len(startLine) == 0: # or result > 1:

                #  Construct it now
                startLine += self._formatTCVline(entry)

            #---------------------------------------------------------------
            #  If we need the ending point info

            elif len(endLine) == 0: # or result == 1:

                #  Construct it now
                endLine = self._formatTCVline(entry, "end")

        #  Prevent both lines from being the same thing
        if startLine == endLine:

            #  Reset the endline - we don't need it
            endLine = ""

        return fcst


    #
    #  Overridden to include affected WFO list
    #

    def _postProcessProduct(self, fcst, argDict):
        #
        # If an overview exists for this product, insert it
        #
        overview = self.finalOverviewText()
        overviewSearch = re.compile(r'DEFAULT OVERVIEW SECTION', re.DOTALL)
        fcst = overviewSearch.sub(overview, fcst)
        #
        # Added to place line feeds in the CAP tags to keep separate from CTAs
        fcst = fcst.replace(r"PRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.", \
                            r"\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\n")
        fcst = fcst.replace(".:", ".")
        fcst = fcst.replace("\n ","\n")
        fcst = fcst.replace("&&", "\n&&\n")

        #  Add the impacted WFO list at the bottom of the TCV
        wfoListText = "ATTN...WFO..."

        self._wfoList.sort()

        for wfo in self._wfoList:
            wfoListText += "%s..." % (wfo)

        fcst += "\n" + self.endline(wfoListText) + "\n"


        #  Now handle the EAS urgency coding
        urgent = 0
        followup = 1
        prodNameKey = ''
        fullKeyList = []
        newList = ['NEW', 'EXA', 'EXB']

        #  Remove EAS line if not urgent
        if urgent == 0 and len(self._easPhrase):
            fcst = fcst.replace(self._easPhrase + '\n', '', 1)

        # Prevent empty Call to Action Tags
        fcst = re.sub(r'\nPRECAUTIONARY/PREPAREDNESS ACTIONS\.\.\.\s*&&\n', \
                      "", fcst)

        # Remove any empty framing code
        fcst = re.sub("\|\*\s*\*\|", "", fcst)

        # Indent the bullet text
        fcst = self._indentBulletText(fcst)

        #
        # Clean up multiple line feeds
        #

        fixMultiLF = re.compile(r'(\n\n)\n*', re.DOTALL)
        fcst = fixMultiLF.sub(r'\1', fcst)

        #
        #  Clean up the JSON file for this storm - if it is all over
        #
        if self._allCAN:
            # set allCAN flag in json file
            advisoryName = self._pil[-3:]
            advisoryDict = self._loadAdvisory(advisoryName)
            advisoryDict['AllCAN'] = True
            self._saveAdvisory(advisoryName, advisoryDict)

        #
        # Finish Progress Meter
        #
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")

        # Add the url text from the configuration section
        fcst = fcst + "\n" + self._urlText

        return fcst


################################################################################
#  New common utility methods for tropical hazard formatters
################################################################################

    #===========================================================================
    #  Define a method to format a breakpoint text line within a TCV segment

    def _formatTCVline(self, entry, type="start"):
        """TropicalHazards addition of _formatTCVline.

        This method will produce a list of NWS WFOs impacted by tropical
        hazards with the specified identifiers.

        Arguments:
            entry -> TropicalAreaDictionary entry for an edit area
            type -> type of breakpoint to produce (optional, defaults to start)
        """

        #  If this is not an ending point
        if type != "end":

            #  Get the information we need
            point = entry["startBreakpoint"].strip()
            lat = entry["startLat"].strip()
            lon = entry["startLon"].strip()
            state = entry["startState"].strip()

        #  Otherwise - get the end point info for this segment
        else:

            #  Get the information we need
            point = entry["endBreakpoint"].strip()
            lat = entry["endLat"].strip()
            lon = entry["endLon"].strip()
            state = entry["endState"].strip()


        #  Clean up the state so there are no spaces or dashes
        state = re.sub("[ _-]+", "", state)

        #-----------------------------------------------------------------------
        #  If this is not the border of a state or country

        if re.search("(?i)border", point) is None:

            #  Add the state/country
            point += "-" + state

        #-----------------------------------------------------------------------
        #  Append the appropriate hemisphere of the latitude

        if lat.find("-") != -1:
            lat += "S"
            lat = lat.replace("-", "")
        else:
            lat += "N"

        #-----------------------------------------------------------------------
        #  Append the appropriate hemisphere of the longitude

        if lon.find("-") != -1:
            lon += "W"
            lon = lon.replace("-", "")
        else:
            lon += "E"

        #-----------------------------------------------------------------------
        # Now construct the final formatted line

        text = "%-36s%6s%7s\n" % (re.sub("[ _]+", "-", point) + " ", lat, lon)

        #  Return the text
        return text

