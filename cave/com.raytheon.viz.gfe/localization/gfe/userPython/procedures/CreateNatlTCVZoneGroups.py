## ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CreateNatlTCVZoneGroups - Version 3.0
#
# Author: Matthew Belk (BOX)
#
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 12, 2016                     cleanup code and refactor to use
#                                  GridManipulation and TropicalUtility
# Apr 29, 2016                     added a popup banner with instructions to run the
#                                  specific text formatter, for a particular storm
# Sep 19, 2016  19293    randerso  Initial baseline check in
#
########################################################################

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["None"]
import os, re, time

import AbsTime
import LocalizationSupport
import LogStream
import ProcessVariableList
import TropicalUtility
import numpy as np


class Procedure (TropicalUtility.TropicalUtility):

    def __init__(self, dbss):
        TropicalUtility.TropicalUtility.__init__(self, dbss)

        #=======================================================================
        #  Define the UGC zone code prefix for each state in the domain. The
        #  keys are the edit area name for each state in the domain we want.

        self.searchUGCdict = {
            "Alabama":"ALZ", "Arkansas":"ARZ", "Connecticut":"CTZ",
            "Delaware":"DEZ", "DistrictofColumbia":"DCZ", "Florida":"FLZ",
            "Georgia":"GAZ", "Louisiana":"LAZ", "Maine":"MEZ", "Maryland":"MDZ",
            "Massachusetts":"MAZ", "Mississippi":"MSZ", "Missouri":"MOZ",
            "NewHampshire":"NHZ", "NewJersey":"NJZ", "NewMexico":"NMZ",
            "NewYork":"NYZ", "NorthCarolina":"NCZ", "Oklahoma":"OKZ",
            "Pennsylvania":"PAZ", "PuertoRico":"PRZ", "RhodeIsland":"RIZ",
            "SouthCarolina":"SCZ", "Tennessee":"TNZ", "Texas":"TXZ",
            "VirginIslands":"VIZ", "Virginia":"VAZ", "WestVirginia":"WVZ",
        }


    #  Define a method to construct a mask which identifies all areas impacted
    #  by hazards associated with a particular storm
    def constructStormHazardMask(self, searchEtn):

        # Get ready to store the composite mask
        finalStormHazardMask = self.empty(np.bool)

        #  Examine entire inventory of the Hazards grids
        for tr in self.GM_getWEInventory("Hazards"):

            print "+"*90
            hazards = self.getGrids("Fcst", "Hazards", "SFC", tr)

            #  Split the Hazards data into its component parts
            hazardBytes, hazardKeys = hazards

            #  Look at each of the hazards keys
            for hazIndex, key in enumerate(hazardKeys):
                print "key =", key

                #  Ignore the default hazard keys which do not have ETN's
                if key in ["<None>", "<Invalid>"]:
                    continue

                #  Get the ETN for this hazard
                curETN = self.getETN(key)
                print "curETN =", curETN, "\tsearchEtn =", searchEtn

                #  If this ETN does not match the storm we are interested in
                if curETN is None or searchEtn is None or \
                   int(curETN) != int(searchEtn):
                    continue            # move on

                #  If we made it this far, mask where this hazard exists
                hazMask = hazardBytes == hazIndex

                finalStormHazardMask[hazMask] = True

        #  Return the completed hazard mask for this storm
        return finalStormHazardMask


    #  Define a method to search breakpoint segment edit areas for hazards
    #  associated with a specific storm
    def searchHazardsBySegment(self, hazardMask):

        #  Get ready to track our results
        results = set()

        #  Make a regular expression to look for breakpoint segment areas
        segmentPattern = re.compile(
            "^(LN\d\d\d\d|ISL\d\d\d|KEY\d\d\d|WT[A-Z][A-Z]\d\d|" +
               "NAI\d\d\d|USC\d\d\d)_?")

        #  Now examine every search area
        for searchArea in self.editAreaList():

            #  If this is not a breakpoint segment area we care about
            if segmentPattern.search(searchArea) is None:
                continue            #  Move on
#                 print searchArea

            #  Get the edit area as a mask
            try:
                searchMask = self.encodeEditArea(searchArea)
            except:
                LogStream.logEvent("Could not encode a mask for %s" %
                                   (searchArea))
                continue

            #  See if these areas overlap
            overlap = hazardMask & searchMask

            #  If these areas do overlap
            if overlap.any():

                #  Add this search area to the results list, if it is not
                #  already included
                results.add(searchArea)

        #  Return the results we found
        return results


    #  Define a method to search state edit areas for hazards associated with a
    #  specific storm
    def searchHazardsByState(self, hazardMask):

        #  Get ready to track our results
        results = set()

#         print "-"*60
#         print "Start state search"

        #  Now examine every search area
        for searchArea in self.searchUGCdict.keys():
#             print "State searchArea =", searchArea

            #  Get the edit area as a mask
            try:
                searchMask = self.encodeEditArea(searchArea)
            except:
                LogStream.logEvent("Could not encode a mask for %s" %
                                   (searchArea))
                continue

            #  See if these areas overlap
            overlap = hazardMask & searchMask

            #  If these areas do overlap
            if overlap.any():

                 #  Add this search area to the results list, if it is not
                #  already included
                results.add(searchArea)

        #  Return the results we found
        return results


    #  Define a method to search state forecast zone edit areas for hazards
    #  associated with a specific storm
    def findZones(self, hazardMask, allEditAreaNames, results, keep=True):

        print "\nI'm starting with %d edit areas" % (len(allEditAreaNames))

        #  Now look for all zones associated with this state
        for (index, searchArea) in enumerate(allEditAreaNames):
#             print "Zone searchArea =", searchArea, index

            #  Get this edit area as a mask
            try:
                searchMask = self.encodeEditArea(searchArea)
            except:
                LogStream.logEvent("Could not encode a mask for %s" %
                                   (searchArea))
                continue

            #  See if these areas overlap
            overlap = hazardMask & searchMask

            #  If these areas do overlap
            if overlap.any():

                #  Add this search area to the results list, if it is not
                #  already included
                if keep:
                    results.add(searchArea)
#                     print "Kept Zone searchArea =", searchArea, index
#                 else:
#                     print "Removing Zone searchArea =", searchArea, index
#
#                 #  Since we already found this zone, do not look for it
#                 #  with future hazard searches
#                 allEditAreaNames.remove(searchArea)
#             else:
#                 print "Ignoring Zone searchArea ->", searchArea, index

        print "I now have %d edit areas" % (len(allEditAreaNames))
        print "with %d results" % (len(results))

        #  Return the results we determined
        return (results, allEditAreaNames)


    #  Define a method to search state forecast zone edit areas for hazards
    #  associated with a specific storm
    def searchHazardsByZone(self, hazardMask, stateList):

        #  Get ready to track our results
        stateZones = set()
        results = set()

        #  Make a list of all know edit area names
        print "Getting all edit area names"
        allEditAreaNames = self.editAreaList()
        print "I have %d edit areas, to start" % (len(allEditAreaNames))

        #  Make a set of zone codes prefixes we need to search further
        for state in stateList:

            #  Get the string expression to look for UGC zone codes
            #  associated with this state
            try:
                stateZones.add(self.searchUGCdict[state])
            except KeyError:
                LogStream.logEvent("Could not find this state %s" % (state))
                continue

        print "\tExamining zones in ->", stateZones

        #  Make a copy of all edit area names, so we can remove some without
        #  crashing the following loop
        copyEditAreaNames = allEditAreaNames[:]

        #  Let's eliminate edit areas which do not match the pattern of UGC
        #  zone code names.  All other edit area names will be removed.
        for editArea in copyEditAreaNames:

            if len(editArea) != 6 or editArea[:3] not in stateZones:
                allEditAreaNames.remove(editArea)
#                 print "Removing ->", editArea

        #  Now that we've removed the areas we know we will not need, take a
        #  closer look at the areas still left
        (results, allEditAreaNames) = self.findZones(hazardMask,
                                                     allEditAreaNames, results)

        #  Return the results we found
        return results


    def execute(self, varDict):

        if varDict is None:
            varDict={}

        #  Let's start timing this
        print "*" *80
        t0 = time.time()
        print time.gmtime(t0), "CreateNatlTCVZoneGroups Starting"

        stormList = self.extractStormInfo()
        stormNames = []
        for sDict in stormList:
            stormNames.append(sDict["stormName"])

        # Make the variableList dynamically based on the storm info
        bogusStormName = "xyz"
        variableList = []
        variableList.append(("StormName", bogusStormName, "radio", stormNames))

        # Display the GUI
        processVarList = ProcessVariableList.ProcessVariableList(
            "Create Text Formatter Sampling Combinations", variableList, varDict)
        status = processVarList.status()
        if status.upper() != "OK":
            self.cancel()

        print "varDict =", varDict
        #  Create a new time range
        now = int(self._gmtime().unixTime() / 3600) * 3600
        timeRange = self.GM_makeTimeRange(now, now + 48 * 3600)

        #  Get the name of our selected storm
        stormName = varDict["StormName"]
        if stormName == bogusStormName:
            self.statusBarMsg("Please select a storm name.", "U")
            return

        #  Extract storm number for selected storm
        for sDict in stormList:
            if sDict["stormName"] == stormName:
                stormNum = int(sDict["stormNumber"])
                lastModified = sDict["lastModified"]
                pil = sDict["pil"]

        #  Get the segment number and filter for valid characters
        if stormNum is None:
            self.abort("You must supply the storm number!")

        #  Ensure this is a national VTEC number
        if stormNum < 1000:
            stormNum = stormNum + 1000

        #  Make sure that the storm info has been updated within the last 7 hours
        if self._gmtime().unixTime() - lastModified > 7 * 3600:
            self.statusBarMsg("StormInfo for " + stormName + " is old. " + \
                              "Please update StormInfo first.", "U")
            return

#         LogStream.logEvent("Got this data\n\tpil = %s\tnumber = %s\n" %
#                            (pil, stormNum))
#         print "Got this data\n\tpil = %s\tnumber = %s\n" % (pil, stormNum)

        #=======================================================================
        #  Get ready to make a list of all states which need to be examined
        #  more closely

        closerLookStates = []
#         finalResults = set()

        #  Find all areas with hazards associated with this storm
        hazardMask = self.constructStormHazardMask(stormNum)

        #  Look for breakpoint segments
#         segments = self.searchHazardsBySegment(hazardMask)
#         print "segments = ", segments
#
#         for segment in segments:
#             if segment not in finalResults:
#                  finalResults.add(segment)

        #  If there are any areas still impacted by this storm
        if  hazardMask.any():

            #  Make a list of states we need to examine more closely
            closerLookStates = self.searchHazardsByState(hazardMask)
            print "CloserLookStates = ", closerLookStates

            #  If there are any states we need to look more closely at
            results = self.searchHazardsByZone(hazardMask, closerLookStates)
        else:
            results = set()

        print "results =", results

        #  Make a filename for this output
        name = "Combinations_%s_%s" % (pil, "NHA")

        #  Get the previous version of this combinations file
        prevCombo = None
        try:
            prevCombo = self.loadCombinationsFile("prev" + name)
        except:
            LogStream.logProblem("Error loading previous combinations file: %s\n" % name, LogStream.exc())

        # if prevCombo is None or empty
        if not prevCombo:
            prevCombo = [[]]

        #  Add back any cancelled zones
        finalSet = results | set(prevCombo[0])
        print "finalSet =", finalSet

       
        #  Now make the final combinations file
        self.saveCombinationsFile(name, [list(finalSet)])

        t1 = time.time()
        print "\n\n%f milliseconds to update combinations file" % ((t1 - t0)*1000.0)


        #=======================================================================
        #  Notify user which formatter to run if there were any zones left

        if (len(finalSet) > 0):
            msg = "You may now create the national TCV for %s " % (stormName) +\
                  "through the GFE Formatter Launcher (In GFE, Products->" +\
                  "Formatter Launcher). In the Formatter Launcher, Products->" +\
                  "Hazard->Hazard_TCV%s." % (pil) + "Click on the gear icon " +\
                  "(second from the right).  Transmit the product when " +\
                  "satisfied it is correct."
    
            self.statusBarMsg(msg, "A")

        #  Let's see how long this took
        t3 = time.time()

        print "\n\n%f milliseconds for total process" % ((t3 - t0) * 1000.0)
        print self._gmtime().timetuple(), "CreateNatlTCVZoneGroups Done"       