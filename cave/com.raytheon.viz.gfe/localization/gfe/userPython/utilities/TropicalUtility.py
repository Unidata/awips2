# ------------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TropicalUtility - Version 3.0
#
# Authors:  Matthew H. Belk (BOX), Shannon White (AWIPS), Pablo Santos (MFL),
# Tom LeFebvre (GSD)
#
# Created:  03/03/2012        Last Modified:  04/26/2016
#
#  04/26/2016 - Modified to add the displayProduct method supplied by Ron
#               Anderson (Raytheon)
# ------------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Mar 03, 2012                        Initial creation
# Apr 26, 2016            mbelk       Modified to add the displayProduct method
#                                     supplied by Ron Anderson (Raytheon)
# Sep 19, 2016 19293      randerso    Initial baseline check in
# Feb 21, 2017 29544      randerso    Fix possible RuntimeError caused by
#                                     discarding from set while looping over it
#
################################################################################

import collections
import errno
import os
import re
import string

from awips.dataaccess import DataAccessLayer

import GridManipulation
import HazardUtils
import JsonSupport
import LocalizationSupport
import LogStream
import ProcessVariableList
import TimeRange
import numpy as np
import pprint as pp


class TropicalUtility(GridManipulation.GridManipulation):

    def __init__(self, dbss):
        GridManipulation.GridManipulation.__init__(self, dbss)
        self._dbss = dbss

        #  Make an instance of the HazardUtils
        self._hazUtils = HazardUtils.HazardUtils(dbss, None)

        #  Define a base for the ETN issued by a national center
        self._natlBaseETN = 1000    #  Not used in current tools/procedures

        #  Get the current mutable database ID
        self._mutableID = self.mutableID()

        #  Make lists of all WFOs we might want to send a message to from NHC.
        #  The offices are split into coastal offices which deal with storm
        #  surge, and inland offices which only deal with wind hazards from NHC
        self._surgeWfos = ["CAR", "GYX", "BOX", "OKX", "PHI", "LWX", "AKQ",
                           "MHX", "ILM", "CHS", "JAX", "MLB", "MFL", "KEY",
                           "TBW", "TAE", "MOB", "LIX", "LCH", "HGX", "CRP",
                           "BRO"]
        self._windWfos = ["ALY", "MRX", "FFC", "OHX", "HUN", "BMX", "MEG",
                          "JAN", "LZK", "SHV", "TSA", "FWD", "OUN", "SJT",
                          "EWX", "MAF"]

        #  Toggle for debugging output
        self._debug = False                 #  True = On / False = Off

        #  Define test mode for procedure which communicate with WFOs
##        self._testMode = True    # if True, the command is only printed (test)
        self._testMode = False     # if False, messages get sent to WFOs (live)


    #===========================================================================
    #  Utility methods to create common dialog buttons and actions

    ### Makes the Run button
    def makeRunButton(self, buttonFrame):
        Tkinter.Button(buttonFrame, text="Run", width=10,
                       command=self.runCommand,
                       state=Tkinter.NORMAL).pack(side=Tkinter.LEFT, pady=5,
                                                  padx=50, fill=Tkinter.X)


    ### Makes the Run/Dismiss button
    def makeRunDismissButton(self, buttonFrame):
        Tkinter.Button(buttonFrame, text="Run/Dismiss", width=10,
                       command=self.runDismissCommand,
                       state=Tkinter.NORMAL).pack(side=Tkinter.LEFT, pady=5,
                                                  padx=50, fill=Tkinter.X)


    ### Makes the Cancel button
    def makeCancelButton(self, buttonFrame):
        Tkinter.Button(buttonFrame, text="Cancel", width=10,
                       command=self.cancelCommand,
                       state=Tkinter.NORMAL).pack(side=Tkinter.LEFT, pady=5,
                                                  padx=50, fill=Tkinter.X)


    ### Action when "Run" button is clicked
    def runCommand(self):
        LogStream.logUse("Run")
        self.makeHazardGrid()
        return


    ### Action when "Run/Dismiss" button is clicked
    def runDismissCommand(self):
        LogStream.logUse("Run/Dismiss")
        if self.makeHazardGrid() == 1:
            self.cancelIt()


    ### Action when "Cancel" button is clicked
    def cancelCommand(self):
        # unregister the maps
        LogStream.logUse("Cancel")
        self.cancelIt()


    ### Actual steps required to cancel/exit
    def cancelIt(self):
        # unregister the maps
        for key in self._registeredMaps:
            self._mapManager.unregisterMapSet(self._registeredMaps[key].mapId())
        self.__master.destroy()


    def getSubKeys(self, key):
        parts = key.split("^")
        if "<None>" in parts:
            parts = parts.remove("<None>")
        return parts


    #===========================================================================
    #  Utility methods to manipulate Hazard grids

    # Returns the phen for specified hazard key.
    # If not a VTEC hazard, returns ""
    def keyPhen(self, key):
        pos = key.find(".")
        if pos == -1:   # not found
            return ""

        return key[0:pos]


    # Parses specified key and returns the sig field.
    def keySig(self, key):
        pos = key.find(".")
        if pos == -1:   # not found
            return ""

        return key[pos + 1]


    # Parse the specified key and return the ETN. If none found,
    # return an empty string ("")
    def getETN(self, key):

        subKeys = key.split("^")
        subKey = subKeys[0]
        parts = subKey.split(":")
        if len(parts) < 2:
            return ""
        else:
            return parts[1]


    # Checks the specified hazard and proposed keys over the selectedMask
    # for any conflicting hazards. If found, returns True, otherwise
    # return False.
    def anyHazardConflicts(self, hazard, proposed, selectedMask):

        # Make the list of tropical hazards
        tropicalHazList = ["TR.W", "TR.A", "HU.W", "HU.A", "SS.A", "SS.W"]

        hazGrid, hazKeys = hazard
        propGrid, propKeys = proposed

        # Make the list of hazard subKeys found in the hazard grid over the
        # selectedMask
        hazList = []
        for hazKey in hazKeys:
            if hazKey == "<None>":      #  Ignore the <None> key
                continue
            
            #  Identify the area where this hazard exists
            hazIndex = self.getIndex(hazKey, hazKeys)
            mask = hazGrid == hazIndex

            #  Check for overlapping points 
            overlap = mask & selectedMask
            
            #  If there is any overlap
            if overlap.any():
                
                # These keys can have subKeys so separate those, too
                subKeyList = self.getSubKeys(hazKey)
                for subKey in subKeyList:
                    if subKey not in hazList:
                        hazList.append(subKey)

        #  Look over the proposed hazards keys
        for propKey in propKeys:
            if propKey == "<None>":     #  Ignore the <None> key
                continue

            # Check for overlapping points
            propIndex = self.getIndex(propKey, propKeys)
            propMask = propGrid == propIndex
            overlap = propMask & selectedMask

            if not overlap.any():  # no points in selectedMask
                continue

            # Parse the phen, sig, and ETN
            propPhen = self.keyPhen(propKey)
            propSig = self.keySig(propKey)
            propETN = self.getETN(propKey)

            for hazKey in hazList:
                # See if this hazard overlaps with the current proposed hazard
                hazIndex = self.getIndex(hazKey, hazKeys)
                hazMask = hazGrid == hazIndex
                hazOverlap = hazMask & propMask
                if not hazOverlap.any():
                    continue

                # Parse the hazKey
                hazETN = self.getETN(hazKey)
                hazPhen = self.keyPhen(hazKey)
                hazSig = self.keySig(hazKey)

                # reconstruct the phen and sig
                hazPhenSig = hazPhen + "." + hazSig
                propPhenSig = propPhen + "." + propSig

                # If the hazard keys are both tropical one the ETNs must match
                if hazPhenSig in tropicalHazList and \
                   propPhenSig in tropicalHazList:
                    if hazETN != propETN:
                        return True

                # Otherwise if the phenSigs match, the ETNs must match
                if hazPhenSig == propPhenSig:
                    if propETN != hazETN:
                        return True
        return False


    # Check for hazard conflicts on a point by point basis.  Uses the method
    # anyHazadConflicts to do the logic for checking hazard phen, sig and ETN.
    def anyHazardConflictsByPoint(self, hazardGrid, proposedGrid, selectedMask):
        print "Inside MergeTool."
        # Make the list of tropical hazards
        tropicalHazList = ["TR.W", "TR.A", "HU.W", "HU.A", "SS.A", "SS.W"]

        hazGrid, hazKeys = hazardGrid
        propGrid, propKeys = proposedGrid

        # Make the list of hazards found in the hazard grid over the
        # selectedMask
        hazList = []
        for hazKey in hazKeys:
            if hazKey == "<None>":
                continue

            hazIndex = self.getIndex(hazKey, hazKeys)
            hazMask = hazGrid == hazIndex

            # Now check propKeys
            for propKey in propKeys:
                if propKey == "<None>":
                    continue
                propIndex = self.getIndex(propKey, propKeys)
                propMask = propGrid == propIndex
                overlap = hazMask & propMask & selectedMask
                if overlap.any():
                    if self.anyHazardConflicts(hazardGrid, proposedGrid, overlap):
                        start = int(self._gmtime().unixTime() / 3600) * 3600
                        end = start + 3600
                        timeRange = self.GM_makeTimeRange(start, end)
                        return True
        return False

    #  Calculates a difference grid (added versus removed)
    #  Calculates a difference grid (added versus removed)
    def calcDiffGrid(self, initialGrid, proposedGrid, diffName, timeRange, 
                     isWFO=False):

        #  If this is a WFO
        if isWFO:
            print"Computing a diff grid for WFO"
            
            #  Filter the Hazards to only keep the Storm Surge hazards
            ssKeys = ["SS.W", "SS.A"]
        
            initialGrid = self.filterHazardGrid(initialGrid, ssKeys)
            proposedGrid = self.filterHazardGrid(proposedGrid, ssKeys)
        
        #  Split these grids into their components
        initGrid, initKeys = initialGrid
        propGrid, propKeys = proposedGrid

        #  Identify where there are no hazards in both grids
        initNone = self.getIndex("<None>", initKeys)
        propNone = self.getIndex("<None>", propKeys)

        #  Mask of these areas
        initNoneMask = (initGrid == initNone)
        propNoneMask = (propGrid == propNone)

        #  Make an empty grid to hold difference indicator
        diffGrid = np.zeros(self.getGridShape(), np.float32)

        # Calculate hazards that were removed
        diffGrid[propNoneMask & ~initNoneMask] = -1

        # Calculate hazards that were added
        diffGrid[~propNoneMask & initNoneMask] = 1
        
        # Find areas that had some hazard and it changed to another hazard
        for initKey in initKeys:
            for propKey in propKeys:
                if initKey == "<None>" or propKey == "<None>":   # ignore any <None> cases
                    continue
                if initKey == propKey: # ignore cases where the keys are the same
                    continue
                
                # Now we know the keys are different and neither is <None>
                initIndex = self.getIndex(initKey, initKeys)
                propIndex = self.getIndex(propKey, propKeys)

                initMask = (initGrid == initIndex)
                propMask = (propGrid == propIndex)
                
                # The intersection is where they changed
                diffGrid[initMask & propMask] = 2

        #  Add this temporary grid to the grid manager so it can be seen
        self.createGrid("Fcst", diffName, "SCALAR", diffGrid, timeRange,
                descriptiveName="Diff Between NHC and WFO",
                precision=0, minAllowedValue=-1.0, maxAllowedValue=2.0)
        
        return 
    

    def filterHazardGrid(self, hazardGrid, filterKeys):
        
        filteredGrid = self.empty(np.int8)
        filteredKeys = ["<None>"]
        hazGrid, hazKeys = hazardGrid
        
        # Find the hazard keys that contain any filter key
        for filterKey in filterKeys:
            for hazKey in hazKeys:
                
                if filterKey not in hazKey:
                    continue
                hazIndex = self.getIndex(hazKey, hazKeys)
                mask = (hazGrid == hazIndex)  # get the points that are set to this mask
                # Cleanse the hazKey of all keys except filterKeys
                newKey = ""
                splitKeys = hazKey.split("^")
                for splitKey in splitKeys:
                    phenSig = splitKey.split(":")[0]
                    if phenSig not in filterKeys:
                        continue
                    
                    newKey = newKey + splitKey + "^"
                    
                if newKey[-1] == "^":
                    newKey = newKey[0:-1]   # trim the trailing "^"
                    
                    
                newIndex = self.getIndex(newKey, filteredKeys)
               
                filteredGrid[mask] = newIndex
            
        return filteredGrid, filteredKeys
    #  Calculates a difference grid (added versus removed) for WFOs

 #==============================================================================
 #  Methods for sending messages to WFOs
 #==============================================================================

    #  Define method to send a message to WFOs
    def sendMessageToWfos(self, wfos, message, testMode=True):
        SendMessageResult = collections.namedtuple('SendMessageResult',
                                                   ('success', 'wfo', 'output'))

        if len(wfos) == 0:
            msg = "sendMessageToWfos called with empty WFO list, nothing to do."
            self.statusBarMsg(msg, 'A')
            return

        #  Look at each WFO which needs a message
        results = []
        for wfo in wfos:

            #  Start constructing the final message
            final_message = "{} - {} have been sent by NHC.".format(wfo.strip(),
                                                                message.strip())

            #  If the ProposedSS grids are mentioned, send one message
            if "ProposedSS" in message:
                final_message += " Please join the stormsurgecollaboration"
                final_message += " chat room in NWSChat. You should run "
                final_message += "the Populate -> CopyNHCProposed procedure "
                final_message += "now, and start the collaboration process."
            
            #  Otherwise, let the WFO's know we're finished with this round 
            else:
                final_message += " The collaboration process is now done. "
                final_message += "You should run the Populate -> "
                final_message += "MergeProposedSS procedure now, and finish "
                final_message += "preparing the grids for the WFO TCV."

            #  If we are in test mode, just display the command which
            #  would be executed
            if testMode:
                msg = "Test message to WFO {}: '{}'".format(wfo, final_message)
                LogStream.logDebug(msg)

                result = ""          #  Simulate a successful transfer

            #  Otherwise, actually send this message
            else:
                msg = "Live message to WFO {}: '{}'".format(wfo, final_message)
                LogStream.logDebug(msg)

                result = self.sendWFOMessage(wfo, final_message)

            #  Keep track of which offices successfully got the message
            results.append(SendMessageResult(result == "", wfo, result))


        #  Comparison function to sort results, by status first, then by WFO
        def compare(x,y):
            result = cmp(x.success, y.success)

            if result == 0:
                result = cmp(x.wfo, y.wfo)

            return result

#         print "*"*80
#         print results

        total_count = 0
        fail_count = 0
        details = ""
        
        #  Construct a final status message of the message send status
        for result in sorted(results, cmp=compare):
            total_count += 1
            if result.success:
                details += "\nMessage successfully sent to site {}.".format(result.wfo)
            else:
                fail_count += 1
                details += "\nCould not send message to site {}. Command output:\n{}".format(result.wfo, result.output)

        if fail_count:
            msg = "{} of {} server(s) failed to receive WFO message. Site-by-site detail: \n{}".format(fail_count, total_count, details)
            self.statusBarMsg(msg, 'A')
        else:
            msg = "WFO message sent to all {} sites successfully. Site-by-site detail: \n{}".format(total_count, details)
            self.statusBarMsg(msg, 'R')


    #  Define method to determine WFOs which should get a message from NHC
    def getWfosAttention(self, WEname, anyChanges=None, percentThresh=3):
        #  anyChanges is a mask, where True means a change in hazards happened

        #  Make a list of WFOs NHC might communicate with
        searchWfos = set(self._surgeWfos + self._windWfos)

        #  Make sets to track WFOs with only surge hazards, those with only
        #  wind hazards, and those with both
        surgeWfos = set()
        windWfos = set()
        bothWfos = set()

        #  Make a dictionary of masks for all of these offices
        officeMasks = {}
        for wfo in searchWfos:
            try:
                officeMasks[wfo] = self.encodeEditArea("ISC_%s" % (wfo.upper()))
                
                #  If we are looking for any changes to the underlying field
                if anyChanges is not None:
                    
                    #  See if there are any changes in hazards for this WFO
                    overlay = (anyChanges & officeMasks[wfo])
                
                    if overlay.any():
                        msg = "Adding to surge - " + wfo + " for changes"
                        self.statusBarMsg(msg, 'R')
                        surgeWfos.add(wfo)
            except:
                msg = "No edit area found. Removing " + wfo + \
                      " from further processing."
                self.statusBarMsg(msg, 'U')

        #  Get the Hazards grid
        hazardGridList = self.getGrids(self._mutableID, WEname, "SFC",
                                       TimeRange.allTimes(), mode="List", 
                                       noDataError=0)

#         print "hazardGridList =", hazardGridList

        #  If there are no hazard grids
        if hazardGridList is None:
            hazardGridList = []

        #  Look at each WFO which needs a message
        for (hazardBytes, hazardKeys) in hazardGridList:

#             print "Starting to examine hazards"

            #  Look at each hazard key in this grid - except the first, <None>
            for (index, key) in enumerate(hazardKeys):

                #  Ignore the <None> and <Invalid> keys
                if key in ["<None>", "<Invalid>"]:
                    continue   #  do not bother looking further

#                 print "\n\nLooking at ", index, key

                #  Check this key for either storm surge (SS), or wind (HU, TR)
                #  hazards
                if re.search("(SS|HU|TR).[AW]", key) is not None:

#                     print "found a tropical hazard"
                    hazardType = "both"         #  assume both hazards are here

                    #-----------------------------------------------------------
                    #  See if which type of hazard this is

                    #  Wind hazard, no surge hazard
                    if re.search("(HU|TR).[AW]", key) is not None and \
                       re.search("SS.[AW]", key) is None:

                        hazardType = "wind-only"

                    #  Surge hazard, no wind hazard
                    elif re.search("SS.[AW]", key) is not None and \
                         re.search("(HU|TR).[AW]", key) is None:

                        hazardType = "surge-only"

                    #  See where this hazard is on the grid
                    hazardMask = hazardBytes == index

                    #  Now determine which offices we need to notify
                    for wfo, wfoMask in officeMasks.items():

                        #  See if this office has a current hazard
                        overlay = (officeMasks[wfo] & hazardMask)

                        #  If there are any points which overlap
                        if overlay.any():

#                            print "Getting zones for '%s'" % (wfo)

                            #  We need to look at all the zones associated
                            #  with this WFO, get them
                            zoneList = self.findWfoZones(wfo)
                            if len(zoneList) == 0:
                                msg = "\tCould not get zones for " + wfo
                                LogStream.logProblem(msg)
                                continue

                            #  Now, process each zone
                            for zone in zoneList:

#                                print zone,

                                #  Get the mask for this zone
                                try:
                                    zoneMask = self.encodeEditArea(zone)
                                except errno:
                                    msg = "\tCould not get zone mask for " + wfo
                                    LogStream.logProblem(msg, LogStream.exc())
                                    continue

#                                #  If we did not get this mask - move on
#                                if zoneMask is None:
#                                    continue

                                #  See if there is an overlap with current
                                #  hazard type
                                zoneOverlap = zoneMask & hazardMask

    #=======================================================================
    #  This code kept in case we need to enforce the 3% area of a zone
    #  requirement in the future. This would mimic the process of the text
    #  formatters.
                                #  Count all the points of the masks
#                                 countOverlap = np.count_nonzero(zoneOverlap)
#                                 countMask = np.count_nonzero(zoneMask)
#
#                                 #  See if there are enough points to justify
#                                 #  keeping this zone in the list
#                                 zonePercent = (
#                                     float(countOverlap) / float(countMask)
#                                 )

#                                 print "overlap = %d\tmask = %d\tpercent =%.2f" % \
#                                       (countOverlap, countMask, zonePercent)
#
                                #  If the percentage is high enough
#                                 if int((zonePercent*100.0) + 0.5) >= percentThresh:
    #
    #=======================================================================

                                #  For now, notify any zone which has a
                                #  possibility for a storm surge hazard
                                if zoneOverlap.any():

                                    #  We need to notify this WFO
                                    if hazardType == "wind-only":
                                        msg = "Adding to wind - " + wfo
                                        windWfos.add(wfo)
                                    elif hazardType == "surge-only":
                                        msg = "Adding to surge - " + wfo
                                        surgeWfos.add(wfo)
                                    else:
                                        msg = "Adding to both - " + wfo
                                        bothWfos.add(wfo)

                                    self.statusBarMsg(msg, 'R')
                                    print msg

                                    #  No point in looking at further zones
                                    break

        #=======================================================================
        #  Now ensure we do not duplicate WFOs with both hazards in the
        #  individual hazard sets.  Use this code when we are no longer using
        #  the text TCV to notify WFOs of tropical wind hazards.

#         for wfo in bothWfos:
#             if wfo in windWfos:
#                 windWfos.discard(wfo)
#             if wfo in surgeWfos:
#                 surgeWfos.discard(wfo)

        #=======================================================================
        #  Now ensure we do not duplicate WFOs with both hazards in the
        #  individual hazard sets - this is for the 2016 season

        for wfo in bothWfos:
            surgeWfos.add(wfo)

        #  Reset the sets for "both" and "wind-only" WFOs
        bothWfos = set()
        windWfos = set()

        #  Return the completed WFO notification list
        return (list(bothWfos), list(windWfos), list(surgeWfos))


    #  Define a method to find zones associated with a WFO
    def findWfoZones(self, wfo):

        #  Construct the SQL to get these attributes from the maps database
        reqParms = {'datatype' : 'maps',
                    'table' : 'mapdata.zone',
                    'locationField' : 'cwa',
                    'geomField' : 'the_geom',
                    'locationNames' : [wfo.strip()],
                    'parameters' : ['state', 'zone'],
                    }

        #  Create the Data Access request
        req = DataAccessLayer.newDataRequest(**reqParms)

        #  Process the response
        result = DataAccessLayer.getGeometryData(req)

        #  Check if we got a response
        if not result:
            # TODO need better error message
            # What should be done in this case?
            print "What!??!"

        #  Get ready to track matching zones
        zoneSet = set()

        #  Process the response contents
        for record in result:

            #  Retrieve state and zone
            state = record.getString('state')
            zone = record.getString('zone')

#             print "*"*(80)
#             print "state = {}   zone = {}".format(state, zone)

            #  If this is ALY - only keep CT and MA zones
            if wfo == "ALY" and state not in ["MA", "CT"]:
                continue

            #  Construct a UGC code and store it for later
            zoneSet.add(state + "Z" + zone)

#         print "zoneSet =", repr(zoneSet)

        #  Return the completed zone set
        return zoneSet


    #  Define a method to find zones associated with a WFO
    def notifyWFOs(self, field, anyChanges=None, testMode=None):
        #  anyChanges is a mask, where True means a change in hazards happened

        #  Ensure the test mode status is set - one way or the other
        if testMode is None:
            testMode = self._testMode

#         #  Get the status of each WFO's communications
#         wfoStatus = self.getWfoStatus()

        #  See which WFOs we need to notify
        (bothWfos, windWfos, surgeWfos) = self.getWfosAttention(field, 
                                                                anyChanges)

        #  Send a message to each office
#         message = "%s grids containing tropical, wind and storm surge hazards"%\
#                   (field)
#         self.sendMessageToWfos(bothWfos, message, self._testMode)

#         message = "%s grids containing tropical, wind hazards" % (field)
#         self.sendMessageToWfos(windWfos, message, self._testMode)

        message = "%s grids containing tropical, storm surge hazards" % (field)
        self.sendMessageToWfos(surgeWfos, message, testMode)


#===============================================================================
#  Code to process StormInfo files -
#===============================================================================

    def _synchronizeAdvisories(self):
        # Retrieving a directory causes synching to occur.
        # This code can throw an exception but don't catch it
        # so that forecasters can be made aware of the issue.
        file = LocalizationSupport.getLocalizationFile(
                                    LocalizationSupport.CAVE_STATIC,
                                    LocalizationSupport.SITE, self.getSiteID(),
                                    self._getAdvisoryPath()).getFile()
        return file

    #  Constructs the absolute path to the JSON files for this site
    def _getLocalAdvisoryDirectoryPath(self):
        file = self._synchronizeAdvisories()
        path = file.getPath()

        try:
             os.makedirs(path)
        except OSError as exception:
            if exception.errno != errno.EEXIST:
                raise

        return path

    #  Retrieves the names of the active storm JSON files for further processing  
    def _getStormAdvisoryNames(self):
        advisoryDirectoryPath = self._getLocalAdvisoryDirectoryPath()
        filenames = os.listdir(advisoryDirectoryPath)
        allAdvisories = filter(lambda filename: filename[-5:] == ".json",
                               filenames)

        print "allAdvisories = %s" % (pp.pformat(allAdvisories))

        stormAdvisories = filter(lambda filename: filename[:2] == "AT",
                                 allAdvisories)
#         stormAdvisories = map(lambda filename: filename[:-5], stormAdvisories)
        print "stormAdvisories = %s" % (pp.pformat(stormAdvisories))

        return stormAdvisories

    #  Loads a JSON storm record
    def _loadAdvisory(self, advisoryName):
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(advisoryName)

        try:
            pythonDict = JsonSupport.loadFromJson(LocalizationSupport.CAVE_STATIC,
                                             self.getSiteID(), fileName)

            statFileName = os.path.join(os.environ["HOME"], "caveData", "etc",
                                        "site", self.getSiteID(), fileName)
            lastModified = os.stat(statFileName).st_mtime
            pythonDict["lastModified"] = lastModified

            print "File contents for %s:" % (fileName)
            print pp.pformat(pythonDict)

            return pythonDict

        except Exception, e:
            print "Load Exception for %s : %s" % (fileName, e)
            return None


    #  Saves a JSON storm record
    def _saveAdvisory(self, advisoryName, advisoryDict):
        self._synchronizeAdvisories()
        fileName = self._getAdvisoryFilename(advisoryName)

        print "Saving %s to %s" % (advisoryName, fileName)
        print "advisoryDict: %s" % (pp.pformat(advisoryDict))

        try:
            JsonSupport.saveToJson(LocalizationSupport.CAVE_STATIC,
                                   self.getSiteID(), fileName, advisoryDict)
#             os.system('chmod 664 %s' % (fileName))
        except Exception as e:
            print "Save Exception for %s : %s" % (fileName, e)
        else: # No exceptions occurred
            print "Wrote file contents for: %s" % (fileName)

            # Purposely allow this to throw
            self._synchronizeAdvisories()

    #  Helper method which identifies where the JSON records go, based on GFE
    #  operating mode.  PRACTICE mode requires the files be placed in a
    #  different location in the Localization store
    def _getAdvisoryPath(self):
        gfeMode = self.gfeOperatingMode()

        if gfeMode == "PRACTICE":
            return os.path.join("gfe", "tcvAdvisories", "practice")
        else:
            return os.path.join("gfe", "tcvAdvisories")

    #  Helper method which constructs the absolute filename for a JSON record 
    def _getAdvisoryFilename(self, advisoryName):
        advisoryFilename = os.path.join(self._getAdvisoryPath(), advisoryName)

        if not advisoryFilename.endswith(".json"):
            advisoryFilename += ".json"
        
        return advisoryFilename

    #  Helper method which coordinates the actual extraction of JSON records 
    #  into our Python environment
    def extractStormInfo(self):

        #  Sync the CAVE localization store
        self._synchronizeAdvisories()

        #  Get the list of all available storm advisories
        fileList = self._getStormAdvisoryNames()

        #  Get the storm information from each advisory
        stormList = []

        for f in fileList:

            #  Load this storm info
            curStorm = self._loadAdvisory(f)

            for key in curStorm:

                #  Convert from unicode strings to a string Java will accept
                if type(curStorm[key]) is unicode:
                    curStorm[key] = str(curStorm[key])

            #  Create a dictionary for this storm
            stormList.append(curStorm)

        return stormList


    def determineStorm (self, stormList, bogusStormName):
        # Decide if this is a new storm or if we need to pre-populate info from existing storm
##        stormList = self.extractStormInfo()
        stormNames = []
        print "/"*100
        print stormList
        for sDict in stormList:
            stormNames.append(sDict["stormName"])
        stormNames.append("New")

        # Make the variableList dynamically based on the storm info
        variableList = []
        variableList.append(("Choose Storm", bogusStormName, "radio",
                             stormNames))

        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            "Choose Existing Storm or New Storm", variableList, varDict)
        status = processVarList.status()

        varDict = processVarList.varDict()

        if status.upper() != "OK":
            self.cancel()

        # Make sure they only choose one storm
        selectedName = varDict["Choose Storm"]

        return selectedName


#===============================================================================
#  Miscellaneous helper methods
#===============================================================================

    # Extract just the wind hazards from the specified hazard grid.
    def extractWindHazards(self, hazardGridList,
                           windHazards=["TR.W", "TR.A", "HU.W", "HU.A"]):

        #hazGrid, hazKeys = hazardGridList[hazWindIndex]
        # Make new empty wind hazard grid
        windHazGrid = self.empty(np.int8)
        windKeys = ["<None>"]

        # Find the hazardGrid that contains any windHazards.
        # Reverse the list first so we search backwards
        hazardGridList.reverse()
        hazardGrid = None
        for grid, keys in hazardGridList:
            if hazardGrid is not None:
                break
            for key in keys:
                for windHaz in windHazards:
                    # If we find a windHazard, save that grid
                    if key.find(windHaz):
                        hazardGrid = (grid, keys)

        # If we didn't find any wind hazards above, return the empty grid
        if hazardGrid is None:
            return (windHazGrid, windKeys)

        # Extract just the wind hazards from the grid we found
        hazGrid, hazKeys = hazardGrid
        for hazKey in hazKeys:
            phen = self.keyPhen(hazKey)
            sig = self.keySig(hazKey)
            phenSig = phen + "." + sig
            if phenSig in windHazards:
                hazIndex = self.getIndex(hazKey, hazKeys)
                windIndex = self.getIndex(hazKey, windKeys)
                windHazGrid[hazGrid == hazIndex] = windIndex

        return (windHazGrid, windKeys)


    # Merge the specified Discrete grid into the Hazard grid.
    def mergeDiscreteGrid(self, mergeHazGrid, timeRange):

        mergeGrid, mergeKeys = mergeHazGrid

        for mergeKey in mergeKeys:

            mergeIndex = self.getIndex(mergeKey, mergeKeys)
            mask = mergeGrid == mergeIndex

            self._hazUtils._addHazard("Hazards", timeRange, mergeKey, mask)

        return


    def variableExists(self, modelName, weName, weLevel):

        # it turns out the the modelName will not match the dbID().model()
        # directly, so it needs to be massaged.
        modelPos = modelName.find("_D2D_")
        if modelPos > -1:
            modelName = modelName[modelPos+5:]

        availParms = self.availableParms()
        for pName, level, dbID in availParms:
            if modelName in dbID.model():
                if weName in pName and weLevel in level:
                    return True

        return False


    def getAvgTopoGrid(self, topodb):

        siteID = self.getSiteID()
#         print "********************\n TOPO IS: ", topodb
        dbName = siteID + "_D2D_" + topodb

        weName = "avgTopo"
        trList = self.GM_getWEInventory(weName, dbName)

        #  Get the GFE topo
        topoGrid = self.getGrids(dbName, weName, "SFC",
                                 trList[0], mode="First")

        #  Convert from meters to feet
        topoGrid /= 0.3048

        topoGrid[topoGrid < -16000] = -80.0
        mask = topoGrid > 16000
        topoGrid[mask] = self.getTopo()[mask]

        return topoGrid


    def removeEarlierTRs(self, weName):

        #  Get an inventory of all the grids
        trList = self.GM_getWEInventory(weName, self._mutableID)

        #  Keep the latest grid
        del trList[-1]

        #  Remove all other grid we found
        for tr in trList:
            self.deleteCmd([weName], tr)

        return


    def getParmMinMaxLimits(self, modelName, weName):

        #  Get the info for this parameter
        parm = self.getParm(modelName, weName, "SFC")

        #  Return the valid min and max values
        return (parm.getGridInfo().getMinValue(),
                parm.getGridInfo().getMaxValue())


    #  Define a method to sort breakpoint record keys
    def sortBreakpoints(self, a, b):

        #  Make a list of valid string parts
        validTypes = [
            "LN",     #  mainland segments
            "KEY",    #  Florida Keys
            "ISL",    #  islands
            "CUBA",   #  Cuba
            "HISP",   #  Hispaniola
            "NAI",    #  North Atlantic islands
            "WTDE",   #  Deleware Bay
            "WTTP",   #  Tidal Potomac
            "WTCP",   #  Chesapeake Bay
            "WTPT",   #  Generic water points
            "GYC",    #  Guyana
            "VEC",    #  Venezuela
            "COC",    #  Colombia
            "PAC",    #  Panama
            "CRC",    #  Costa Rica
            "NIC",    #  Nicaragua
            "HNC",    #  Honduras
            "GTC",    #  Guatemala
            "BZC",    #  Belize
            "MXC",    #  Mexico
            "USC",    #  United States
            "CNC",    #  Canada
            "KEC",    #  Dry Tortugas
            "AWC",    #  Aruba
            "CWC",    #  Curacao
            "TTC",    #  Trinidad and Tobago
            "BBC",    #  Barbados
            "LCC",    #  St. Lucia
            "MQC",    #  France - Caribbean
            "AGC",    #  Antigua and Barbuda
            "BSC",    #  Bahamas
            "BMC",    #  Bermuda
            "JMC",    #  Jamaica
            "KYC",    #  Cayman Islands
            "CUC",    #  Cuba
            "DOC",    #  Dominican Republic
            "HTC",    #  Haiti
            "PMC",    #  France - North Atlantic
            "LOC",    #  Lake_Okeechobee
            "FBC",    #  Florida Bay
            "PSC",    #  Pamlico Sound
            "ASC",    #  Albemarle Sound
            "TXZ",    #  Texas
            "LAZ",    #  Louisiana
            "MSZ",    #  Mississippi
            "ALZ",    #  Alabama
            "FLZ",    #  Florida
            "GAZ",    #  Georgia
            "SCZ",    #  South Carolina
            "NCZ",    #  North Carolina
            "VAZ",    #  Virginia
            "MDZ",    #  Maryland
            "DCZ",    #  District of Columbia
            "DEZ",    #  Deleware
            "NJZ",    #  New Jersey
            "NYZ",    #  New York
            "CTZ",    #  Connecticut
            "RIZ",    #  Rhode Island
            "MAZ",    #  Massachusetts
            "NHZ",    #  New Hampshire
            "MEZ",    #  Maine
            "NMZ",    #  New Mexico
            "ARZ",    #  Arkansas
            "OKZ",    #  Oklahoma
            "MOZ",    #  Missouri
            "TNZ",    #  Tennessee
            "WVZ",    #  West Virginia
            "PAZ",    #  Pennsylvania
            "VTZ",    #  Vermont
            "PRZ",    #  Puerto Rico
            "VIZ",    #  U.S. Virgin Islands
            "RE",     #  General edit area collection
        ]

#         print "a = '%s'    b = '%s'" % (a, b)

        aSeg = a.split("_")[0]
        bSeg = b.split("_")[0]

        aSegType = ""
        bSegType = ""
        aSegNum = ""
        bSegNum = ""

        for c in aSeg:
            if c in string.letters:
                aSegType = aSegType + c

        for c in bSeg:
            if c in string.letters:
                bSegType = bSegType + c

        for c in aSeg:
            if c in string.digits:
                aSegNum = aSegNum + c
        for c in bSeg:
            if c in string.digits:
                bSegNum = bSegNum + c

        aTypeIndex = validTypes.index(aSegType)
        bTypeIndex = validTypes.index(bSegType)

        if aTypeIndex < bTypeIndex:
            return -1
        elif bTypeIndex < aTypeIndex:
            return 1

        if int(aSegNum) < int(bSegNum):
            return -1
        elif int(bSegNum) < int(aSegNum):
            return 1
        else:
            print "ERROR!!!!!!! Segment names are equal!!!!!!!"
            return 0


#===============================================================================
#  Hazard grid helper methods
#===============================================================================

    # Extracts the specified hazard from the hazardGrid. Returns a list of
    # keys, mask pairs where each hazard exists.
    def extractHazards(self, hazardGrid, hazard):
        hazGrid, hazKeys = hazardGrid

        keyMaskList = []
        for hazIndex, hazKey in enumerate(hazKeys):
            if hazard in hazKey:

                #  See if this key covers any portion of the domain
                mask = hazGrid == hazIndex
                if not mask.any():
                    continue

                #  Pair this key with its mask
                keyMaskList.append((hazKey, mask))

        return keyMaskList


    def purifyKey(self, hazKey, allowedKeys):

        #  Get ready to process some subkeys
        subKeyList = set()
        subKeys = hazKey.split("^")

        #  Process all the hazard subkeys
        for subKey in subKeys:

            #  Go over all the allowed Hazard keys
            for allowedKey in allowedKeys:

                #  If this is one of them
                if allowedKey in subKey:

                    #  Add it to the subkey list - if not already there
                    if allowedKey not in subKeyList:
                        subKeyList.add(subKey)

        #  Return the final key
        return "^".join(subKeyList)


    def mergeCertainHazards(self, initalGrid, gridToMerge, hazTR,
                            selectedHazards=["SS.W", "SS.A"]):

        #  Use the Proposed grid is now the one to use for GFE hazards, for now
        HazardUtils.ELEMENT = "ProposedSS"

        #  Split the initial grid into its components
        initialBytes, initialKeys = initalGrid

        #  Look for all the hazards we wish to keep
        for haz in selectedHazards:

            #  Find all the areas in the domain where this hazard exists
            keyMaskList = self.extractHazards(gridToMerge, haz)

            #  Process all areas identified to have current tropical hazard
            for hazKey, hazMask in keyMaskList:

                #  Filter out the hazards we do not want
                pureHazKey = self.purifyKey(hazKey, selectedHazards)

                #  If there is nothing left to do, move on to next hazard
                if pureHazKey == "":
                    continue

                #  Merge these hazards into the initial grid
                hazIndex = self.getIndex(pureHazKey, initialKeys)
                self._hazUtils._addHazard("ProposedSS", hazTR, pureHazKey,
                                          hazMask, combine=1)

        #  Make sure the Hazards grid is now the one to use for GFE hazards
        HazardUtils.ELEMENT = "Hazards"

        #  Return the merged grid
        return (initialBytes, initialKeys)


#===============================================================================
#  Generic method to display product text via a GFE procedure/smartTool
#===============================================================================

    def displayProduct(self, product):
        """
        Displays the product text. Returns true if forecaster clicked OK
        """
        from com.raytheon.viz.gfe.ui.runtimeui import ValuesDialog
        varList = []
        varList.append(("Product Text:", "", "label"))
        varList.append((product, "", "label"))
        varList.append(("Click OK to transmit the product", "", "label"))
        widgetList = self.getVariableListInputs(varList)
        dialog = ValuesDialog.openDialog("Text Product", widgetList, None)
        return dialog.getReturnCode() == 0 # 0 is OK, 1 is CANCEL
