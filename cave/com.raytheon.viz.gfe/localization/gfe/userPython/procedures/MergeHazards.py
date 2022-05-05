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
# MergeHazards
#
# Author: lefebvre
#
# This procedure reads all of the temporary hazard grids and selectively
# loads them in the the "Hazards" grid.
# ----------------------------------------------------------------------------
#
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Dec 23, 2013 16893      ryu         Check in njensen's change to removeTempHazards()
#                                     to call SmartScript.unloadWEs()
# Sep 19, 2016 19293      randerso    Changes for 2017 tropical season.
# Jun 23, 2017 6138       dgilling    Changes for Winter Weather VTEC
#                                     consolidation.
# Oct 12, 2017 DR20389    swhite      Remove HTI/TCV Restrictions on CFW Hazards.
#
########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

MenuItems = ["Hazards"]

import SmartScript
import HazardUtils
import LogStream
import numpy


from HazardUtils import MODEL
from HazardUtils import ELEMENT
from HazardUtils import LEVEL

######################### CONFIGURATION SECTION  ######################
#
#  This dictionary defines which hazards cannot be combined with other
#  Hazards.  The structure lists each hazard in the VTECTable followed
#  by a list of VTEC codes that may not be combined with it at the same
#  grid point.  For example  "DU.W" : ["DU.Y"] means that DU.W may not
#  be combined with a DU.Y hazard at the same grid point.

HazardsConflictDict = {
   "AF.W" : ["AF.Y"],
   "AF.Y" : ["AF.W"],
   "AQ.Y" : ["AS.O", "AS.Y"],
   "AS.O" : ["AQ.Y", "AS.Y"],
   "AS.Y" : ["AQ.Y", "AS.O"],
   "BH.S" : ["HU.A", "HU.W", "TR.A", "TR.W", "TY.A","TY.W", "SS.W", "SS.A"],
   "BW.Y" : ["GL.W", "SR.W", "HF.W", "TR.A", "TR.W", "HU.A", "HU.W", "HU.S",
             "SC.Y", "SW.Y", "SE.W", "RB.Y", "SI.Y"],
   "BZ.W" : ["WS.A", "IS.W", "LE.W", "WS.W", "WW.Y"],
   "CF.A" : ["CF.W", "CF.Y", "SS.A", "SS.W"],
   "CF.W" : ["CF.A", "CF.Y", "SS.A", "SS.W"],
   "CF.Y" : ["CF.W", "CF.A", "SS.A", "SS.W"],
   "CF.S" : ["CF.Y", "CF.W", "CF.A", "SS.A", "SS.W"],
   "DU.W" : ["DU.Y"],
   "DU.Y" : ["DU.W"],
   "EC.A" : ["WC.A", "EC.W", "WC.W"],
   "EC.W" : ["EC.A", "WC.A", "WC.W", "WC.Y"],
   "EH.A" : ["EH.W", "HT.Y"],
   "EH.W" : ["EH.A", "HT.Y"],
   "FA.A" : ["FF.A"],
   "FA.W" : [],
   "FA.Y" : [],
   "FF.A" : ["FA.A"],
   "FF.W" : [],
   "FG.Y" : [],
   "FL.A" : [],
   "FL.W" : [],
   "FL.Y" : [],
   "FR.Y" : ["FZ.A", "FZ.W", "HZ.W", "HZ.A"],
   "FW.A" : ["FW.W"],
   "FW.W" : ["FW.A"],
   "FZ.A" : ["FZ.W", "FR.Y", "HZ.W", "HZ.A"],
   "FZ.W" : ["FZ.A", "FR.Y", "HZ.W", "HZ.A"],
   "GL.A" : ["SR.W", "HF.W", "BW.Y", "TR.A", "TR.W", "HU.A", "HU.W",
             "SC.Y", "SW.Y", "SE.W", "RB.Y", "SI.Y", "GL.W", "SR.A",
             "HF.A", "SE.A", "TY.A", "TY.W"],
   "GL.W" : ["SR.W", "HF.W", "BW.Y", "TR.W", "HU.W",
             "SC.Y", "SW.Y", "SE.W", "RB.Y", "SI.Y", "GL.A", "SR.A",
             "SE.A","TY.W"],
   "HF.A" : ["BW.Y", "TR.A", "TR.W", "HU.A", "HU.W", "GL.A", "SR.A",
             "HF.W", "SE.A", "TY.A", "TY.W"],
   "HF.W" : ["GL.W", "SR.W", "BW.Y", "TR.A", "TR.W", "HU.A", "HU.W",
             "SC.Y", "SW.Y", "SE.W", "RB.Y", "SI.Y", "GL.A", "SR.A",
             "HF.A", "SE.A", "TY.W"],
   "HT.Y" : ["EH.A", "EH.W"],
   "HU.A" : ["HF.W", "BW.Y", "TR.A", "HU.W", "HU.S",
             "GL.A", "SR.A", "HF.A", "SE.A"],
   "HU.S" : ["TR.A", "TR.W", "HU.A", "HU.W", "TY.A", "TY.W"],
   "HU.W" : ["GL.W", "SR.W", "HF.W", "BW.Y", "TR.A", "TR.W", "HU.A", "SC.Y",
             "SW.Y", "SE.W", "RB.Y", "SI.Y", "GL.A", "SR.A", "HF.A", "SE.A",
             "HU.S"],
   "HW.A" : ["HW.W", "WI.Y"],
   "HW.W" : ["HW.A", "LW.Y", "WI.Y"],
   "HZ.A" : ["FZ.W", "FR.Y", "FZ.A", "HZ.W"],
   "HZ.W" : ["FZ.A", "FR.Y", "HZ.A", "FZ.W"],
   "IS.W" : ["WS.A", "BZ.W", "WS.W", "LE.W", "WW.Y"],
   "LE.W" : ["WS.A", "BZ.W", "IS.W", "WS.W", "WW.Y"],
   "LO.Y" : [],
   "LS.A" : ["LS.W", "LS.Y", "LS.S"],
   "LS.S" : ["LS.A", "LS.Y", "LS.W"],
   "LS.W" : ["LS.A", "LS.Y", "LS.S"],
   "LS.Y" : ["LS.A", "LS.W", "LS.S"],
   "LW.Y" : ["HW.W", "WI.Y"],
   "MA.S" : [],
   "MA.W" : [],
   "MF.Y" : [],
   "MH.W" : ["MH.Y"],
   "MH.Y" : ["MH.W"],
   "MS.Y" : [],
   "RB.Y" : ["GL.W", "SR.W", "HF.W", "BW.Y", "TR.W", "HU.W",
             "SE.W", "GL.A", "SR.A", "SE.A", "TY.W"],
   "RP.S" : [],
   "SC.Y" : ["GL.W", "SR.W", "SR.A", "BW.Y", "TR.W", "HU.W",
             "SE.W", "SE.A", "HF.W", "GL.A", "TY.W"],
   "SE.A" : ["SR.W", "HF.W", "HF.A", "BW.Y", "TR.A", "TR.W", "HU.A", "HU.W",
             "SC.Y", "SW.Y", "RB.Y", "SI.Y", "GL.A", "GL.W", "SE.W", "SR.A",
             "TY.A", "TY.W"],
   "SE.W" : ["SR.W", "HF.W", "BW.Y", "TR.W", "HU.W", "TY.W",
             "SC.Y", "SW.Y", "RB.Y", "SI.Y", "GL.A", "GL.W", "SE.A", "SR.A"],
   "SI.Y" : ["SR.W", "SR.A", "BW.Y", "TR.W", "HU.W", "TY.W",
             "GL.W", "GL.A", "HF.W", "SE.A", "SE.W"],
   "SM.Y" : [],
   "SR.A" : ["GL.A", "GL.W", "HF.A", "HF.W", "HU.W", "HU.A", "TR.W", "TR.A",
             "RB.Y", "SC.Y", "SE.A", "SE.W", "SI.Y", "SR.W", "SW.Y", "TY.W", "TY.A"],
   "SR.W" : ["GL.W", "HF.W", "BW.Y", "TR.W", "HU.W", "TY.W",
             "SC.Y", "SW.Y", "SE.W", "SE.A", "RB.Y", "SI.Y", "GL.A", "SR.A"],
   "SS.A" : ["CF.A", "CF.W", "CF.Y", "SS.W"],
   "SS.W" : ["CF.A", "CF.W", "CF.Y", "SS.A"],
   "SU.W" : ["SU.Y"],
   "SU.Y" : ["SU.W"],
   "SV.A" : ["TO.A"],
   "SV.W" : [],
   "SW.Y" : ["GL.W", "SR.W", "HF.W", "BW.Y", "TR.W", "HU.W", "TY.W",
             "SE.W", "GL.A", "SR.A", "SE.A"],
   "TO.A" : ["SV.A"],
   "TO.W" : [],
   "TR.A" : ["HF.W", "TR.W", "HU.A", "HU.S", "HU.W","TY.A", "TY.W",
             "GL.A", "SR.A", "HF.A", "SE.A"],
   "TR.W" : ["GL.W", "SR.W", "HF.W", "BW.Y", "TR.A", "HU.W", "HU.S",
             "SC.Y", "SW.Y", "SE.W", "RB.Y", "SI.Y", "TY.W",
             "GL.A", "SR.A", "HF.A", "SE.A"],
   "TS.A" : ["TS.W", "TS.Y"],
   "TS.W" : ["TS.A", "TS.Y"],
   "TS.Y" : ["TS.A", "TS.W"],
   "TY.A" : ["TR.A", "TY.W", "HU.S", "HF.W", "GL.A", "SR.A", "HF.A", "SE.A"],
   "TY.W" : ["TY.A", "HU.S", "TR.A", "TR.W", "GL.A", "SR.A", "HF.A", "SE.A"
             "GL.W", "SR.W", "HF.W", "BW.Y","SC.Y", "SW.Y", "SE.W", "RB.Y", "SI.Y"],
   "UP.W" : ["TR.A", "TR.W", "HU.A", "HU.S", "HU.W", "UP.Y"],
   "UP.Y" : ["TR.A", "TR.W", "HU.A", "HU.S", "HU.W", "UP.W"],
   "WC.A" : ["WC.Y", "WC.W"],
   "WC.W" : ["WC.A", "WC.Y"],
   "WC.Y" : ["WC.A","WC.W"],
   "WI.Y" : ["HW.A", "HW.W", "LW.Y"],
   "WS.A" : ["BZ.W", "IS.W", "WS.W", "LE.W", "WW.Y"],
   "WS.W" : ["WS.A", "BZ.W", "IS.W", "LE.W", "WW.Y"],
   "WW.Y" : ["WS.A", "BZ.W", "IS.W", "WS.W", "LE.W"],
   "ZF.Y" : [],
   }

########################## END OF CONFIGURATION SECTION ########################

class Procedure(SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss

    ##
    # Get the list of loaded temporary hazard parms
    # @return: Temporary hazard parm names, i.e., ["hazAFY"]
    # @rtype: List of Strings
    def getHazardParmNames(self):
        parms = self.loadedParms()
        hazParms = []
        for weName, level, dbID in parms:
            if weName.startswith("haz"):
                # TODO: Why is this back/forth xform needed?
                key = self._hazUtils._tempWENameToKey(weName)
                index = key.find(":")
                if index != -1:
                    mkey = key[0:index]
                    segNum = key[index+1:]
                else:
                    mkey = key
                    segNum = ""

                # append the hazard and a description
                parmName = "haz" + key
                parmName = parmName.replace(".", "")
                parmName = parmName.replace(":", "")
                hazParms.append(parmName)

        return hazParms

    ##
    # Unload (delete) all the temporary hazards
    def removeTempHazards(self):
        parms = self.loadedParms()
        
        toRemovePairs = []
        for weName, level, dbID in parms:
            if weName.startswith("haz"):
                toRemovePairs.append((weName, level))
        self.unloadWEs(MODEL, toRemovePairs)

        return

    ##
    # The action performed when the user opts to cancel a merge.
    # This was a callback under Tcl/tk; now displayDialog invokes
    # it directly.
    def cancelCommand(self):
        LogStream.logEvent("MergeHazards: cancel")
        return

    ##
    # The action performed when the user opts to continue a merge.
    # This was a callback under Tcl/tk; now displayDialog invokes
    # it directly.
    def continueCommand(self):
        LogStream.logEvent("MergeHazards: continue")
        parm = self.getParm(MODEL, ELEMENT, LEVEL)
        parm.setMutable(True)
        self.mergeHazardGrids()
        return

    ##
    # Displays a dialog box and queries the user to continue to merge or
    # abort the merge
    def displayDialog(self, message):
        from MessageBox import MessageBox
        messageBox = MessageBox(style=MessageBox.ICON_WARNING)
        messageBox.setText("MakeHazard")
        messageBox.setMessage(message)
        messageBox.setButtonLabels(["Continue Merge", "Cancel Merge"])
        messageBox.setDefaultIndex(1)
        if (messageBox.open() == 0):
            self.continueCommand()
        else:
            self.cancelCommand()

        return

    ##
    # Returns the set of hazParms grids that overlap with the specified
    # timeRange.
    # @param hazParms: Hazard parm names to check
    # @type hazParms: Sequence of string
    # @param timeRange: The time range to check for overlap with
    # @type timeRange: Python TimeRange
    # @return: Byte grids and keys of the overlapping parms
    # @rtype: 2-tuple: list of byte arrays, list of list of strings   
    def getOverlappingHazGrids(self, hazParms, timeRange):
        byteGridList = []
        keyList = []
        for hazParm in hazParms:
            trList = self._hazUtils._getWEInventory(hazParm)
            for tr in trList:
                if tr.overlaps(timeRange):
                    byteGrid, hazKey = self.getGrids(MODEL, hazParm, LEVEL,
                                                     tr, mode="First")
                    if isinstance(hazKey, str):
                        hazKey = eval(hazKey)
                    byteGridList.append(byteGrid)
                    keyList.append(hazKey)

        return byteGridList, keyList

    ##
    # Returns the first non-None key it finds in the keyList
    # @param keyList: Keys to search
    # @type keyList: Sequence of string
    # @return: First key that is not "<None>"
    # @rtype: string
    def getHazardKey(self, keyList):
        for k in keyList:
            if k != "<None>":
                return k

    ##
    # Checks the specified hazard grids to see if they are conflicting
    # Each grid is a tuple (byteGrid, key).  Uses the configurable
    # HazardConflictDict to determine whether two hazards can be combined
    # at the same grid point.  Returns an empty list if no conflict or
    # the list of hazards if they do.
    #
    # This method should really only be used internally; it assumes that
    # there is at most one key other than "<None>", and that it contains
    # a single subkey.
    #
    # @param hazGrid1: The first hazard grid
    # @type hazGrid1: 2-tuple: numpy array of int8, list of String
    # @param hazGrid2: The second hazard grid
    # @type hazGrid2: 2-tuple: numpy array of int8, list of String
    # @return: conflicting hazard names or empty list
    # @rtype: list
    def conflictingHazards(self, hazGrid1, hazGrid2):
        byteGrid1, hazKey1 = hazGrid1
        byteGrid2, hazKey2 = hazGrid2
        
        key1 = self.getHazardKey(hazKey1)
        key2 = self.getHazardKey(hazKey2)
        phenSig1 = key1[0:4]   # remove the etn
        phenSig2 = key2[0:4]
        
        keyConflict = False
        if phenSig1 == phenSig2 and key1 != key2:
            keyConflict = True
        elif phenSig1 in HazardsConflictDict:
            if phenSig2 in HazardsConflictDict[phenSig1]:
                keyConflict = True

        if keyConflict:
            # calculate the overlap, adding the grids together will tell us if
            # there is any overlap.  Any grid points > 1 are overlapped
            totalGrid = byteGrid1 + byteGrid2
            overlapMask = numpy.greater(totalGrid, 1)
            if numpy.any(overlapMask):
                return [key1, key2]
        
        return []
        
    ##
    # See if there are any temporary hazards for the same position and time
    # that conflict with one another.
    #
    # @param hazParms: Temporary hazard parm names to check.
    # @type hazParms: sequence of string
    # @return: The first conflict, or None if there are no conflicts
    # @rtype: 2-tuple(TimeRange, list of string) or NoneType
    def checkForHazardConflicts(self, hazParms):
        timeList = []
        for hazParm in hazParms:
            trList = self._hazUtils._getWEInventory(hazParm)
            for tr in trList:
                if tr.startTime().unixTime() not in timeList:
                    timeList.append(tr.startTime().unixTime())
                if tr.endTime().unixTime() not in timeList:
                    timeList.append(tr.endTime().unixTime())

        timeList.sort()   # sort the list

        for t in range(len(timeList) - 1):
            start = timeList[t]
            end = timeList[t+1]
            timeRange = self._hazUtils._makeTimeRange(start, end)
            byteGridList = []
            keyList = []
            byteGridList, keyList = self.getOverlappingHazGrids(hazParms, timeRange)
            # compare each grid to all other grids at this timeRange
            for firstIndex in range(len(byteGridList) - 1):
                for secondIndex in range(firstIndex + 1, len(byteGridList)):
                    grid1 = (byteGridList[firstIndex], keyList[firstIndex])
                    grid2 = (byteGridList[secondIndex], keyList[secondIndex])
                    conflictList = self.conflictingHazards(grid1, grid2)
                    if conflictList != []:
                        return (timeRange, conflictList)

        # if we made it to here, all is well
        return None
    
    ##
    # Perform checks to see if it's OK to merge hazards. If there are no conflicting
    # locks or incompatible hazards, do the merge. If there are conflicting locks,
    # generate a status bar message and quit. If there incompatible
    # hazards, show a warning and let the user decide whether to continue. 
    def checkForMerge(self):
        # get the hazards selected by the forecaster
        hazParms = self.getHazardParmNames()

        # check for empty list of hazards
        if hazParms == []:
            self.statusBarMsg("No temporary grids to merge.", "S")
            return

        # FIXME: Lock race condition
        # check for conflicting locks
        if self._hazUtils._conflictingLocks(hazParms):
            self.statusBarMsg("There are conflicting locks.  " +
                              "Please resolve these before merging any hazards", "S")
            return

        conflicts = self.checkForHazardConflicts(hazParms)
        if conflicts is None:
            # if no conflicts, merge the grids
            # We made the hazards parm immutable when we separated hazard grids.
            # It has to be made mutable to do the merge.
            parm = self.getParm(MODEL, ELEMENT, LEVEL)
            parm.setMutable(True)
            self.mergeHazardGrids()
        else:
            haz1 = conflicts[1][0].replace(".", "")
            haz2 = conflicts[1][1].replace(".", "")
            timeRange = str(conflicts[0])
            msg = "Hazard conflict detected!\n\n"
            msg += "Time: " + timeRange + " \n\n"
            msg += "with Hazard grids haz" + haz1 + " and haz" + haz2 + ".\n"

            LogStream.logEvent("Merge conflict: "+ msg)
            self.displayDialog(msg)

        return

    ##
    # Performs the actual merge of the temp hazards grids into the "Hazards" grid.
    def mergeHazardGrids(self):
        # get the hazards selected by the forecaster
        hazParms = self.getHazardParmNames()

        self._hazUtils._removeAllHazardsGrids()

        for hazParm in hazParms:
            trList = self._hazUtils._getWEInventory(hazParm)

            for tr in trList:
                byteGrid, hazKey = self.getGrids(MODEL, hazParm, LEVEL, tr,
                                                 mode="First")
                if isinstance(hazKey, str):
                    hazKey = eval(hazKey)

                uniqueKeys = self._hazUtils._getUniqueKeys(byteGrid, hazKey)
                for uKey in uniqueKeys:
                    if uKey == "<None>":
                        continue
                    subKeys = self._hazUtils._getSubKeys(uKey)
                    for subKey in subKeys:
                        # make the mask - find all areas that contain the subKey
                        mask = self.empty(bool)
                        for haz in hazKey:
                            if subKey in haz:
                                hazIndex = self.getIndex(haz, hazKey)
                                mask[numpy.equal(byteGrid, hazIndex)] = True

                        # make the grid
                        self._hazUtils._addHazard(ELEMENT, tr, subKey, mask)
                        LogStream.logEvent("merge: " + \
                          str(self._hazUtils._printTime(tr.startTime().unixTime())) + " " + \
                          str(self._hazUtils._printTime(tr.endTime().unixTime())) + " " + \
                          subKey + "\n")

        self.removeTempHazards()
        
        return

    ##
    # The main entry point of the procedure.
    def execute(self):
        self.setToolType("numeric")

        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)

        # see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in " +\
              "the GFE before running MergeHazards", "S")
            self.cancel()

        self.checkForMerge()
        return

