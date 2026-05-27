# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RestoreWindWWHazards.py
#
# This tool will plot Tropical Wind Hazards found in the VTEC activeTable and
# plot them in the Hazards grid.
#
# Author: lefebvre
#
# SOFTWARE HISTORY
#
# Date         Ticket#  Engineer    Description
# ------------ -------- ----------- ------------------------------------------
# Apr 24, 2020 21020    tlefebvr    Original Version.
# May  5, 2020 21020    tlefebvr    Prompts user to delete Hazards if empty
#                                   activeTable.
# May 20, 2020 22033                Addressed code review comments.
# Sep 16, 2020 22033                Removes all Hazards before restoring.
# Mar  9, 2020 22033                Filters for records with siteID == "KNHC" only
# Mar 10, 2020 22033    tlefebvr    Fixed bug with default parameter.
# Jul 29, 2021 22531    tlefebvr    Code clean up before check-in.
# Jan  6, 2022 22531    tlefebvr    Added a small dict to translate siteID
#                                   to activeTableID.
# Jan  7, 2022 22531    tlefebvr    Added call to ArchiveHazardGridsToJSON.
# Sep 15, 2022 22531    santos/composano/white/harrigan/belk Fixes after
#                                   21.4.1-13 testing. Found sig bug when testing
#                                   with multiple storms.
# Sep 29, 2023 2036298  santos/scomp/white Additional fixes after 21.4.1-13 testing.
# ----------------------------------------------------------------------------
MenuItems = ["None"]

import AbsTime, TimeRange
import SmartScript
import HazardUtils
import ProcessVariableList


class Procedure(SmartScript.SmartScript):

    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss

    def insertNewRecord(self, activeTable, dictKey, zoneID):
        """
        Checks to see if there is an identical record with a phen matching
        the combo ("HU", "TR"). If there is a match, a combo key (HU.A^TR.W)
        is inserted into activeTable its place.
        """

        def saveRecord(activeTable, dictKey, zoneID, etn):
            """
            Saves a record in the active table.
            """
            arecord = activeTable.get(dictKey, {})
            zoneList = arecord.get("zoneList", [])
            zoneList.append(zoneID)
            record = {"etn": etn, "zoneList": zoneList}
            if dictKey in activeTable:
                activeTable[dictKey]["zoneList"] = zoneList
            else:
                activeTable[dictKey] = record

            return

        def removeZone(activeTable, dictKey, zoneID):
            """
            Removes a record from the activeTable matching the specified parameters.
            """
            if dictKey not in activeTable:
                return
            zoneList = activeTable[dictKey].get("zoneList", None)
            if zoneID in zoneList:
                activeTable[dictKey]["zoneList"].remove(zoneID)
            return

        hazKey, start, end, etn = dictKey
        # Define the oppo phenSig
        if hazKey == "HU.A":
            oppoPhenSig = "TR.W"
        elif hazKey == "TR.W":
            oppoPhenSig = "HU.A"
        else:
            oppoPhenSig = None
        # Not part of the combo
        if oppoPhenSig is None:
            saveRecord(activeTable, dictKey, zoneID, etn)
            return

        # Look for the "other" key
        oppoKey = (oppoPhenSig, start, end, etn)
        if oppoKey not in activeTable:
            saveRecord(activeTable, dictKey, zoneID, etn)
            return
        # Found a possible match to the combo.
        zoneList = activeTable[oppoKey].get("zoneList", None)
        if zoneID in zoneList:
            # Found a match. Remove the old record and add the new
            removeZone(activeTable, oppoKey, zoneID)
            newDictKey = ("HU.A^TR.W", start, end, etn)
            saveRecord(activeTable, newDictKey, zoneID, etn)
        else:
            # No match, so just save the record.
            saveRecord(activeTable, dictKey, zoneID, etn)

        return

    def getActiveTableInfo(self, siteID, phenList=None):
        """
        Gets the active table, filtered by the specified phen list
        """
        activeTable = {}
        vtecTable = self.vtecActiveTable()
        vtecTable = self._hazUtils._filterVTECBasedOnGFEMode(vtecTable)

        actionList = ["NEW", "EXA", "CON"]
        for v in vtecTable:
            if v['officeid'] != siteID:
                continue
            # filter based phenList, if specified
            if phenList is not None and v['phen'] not in phenList:
                continue
            # Filter for actions
            if v['act'] not in actionList:
                continue

            startTm = v['startTime']
            # truncate the startTime to the top of the hour
            startTm = int(startTm / 3600) * 3600
            endTm = v['endTime']

            etn = v.get("etn", 0)
            zoneID = v['id']

            hazKey = v['phen'] + "." + v['sig']
            dictKey = (hazKey, startTm, endTm, etn)

            # Insert the record into the activeTable
            self.insertNewRecord(activeTable, dictKey, zoneID)

        return activeTable

    def activeTableID(self, siteID):
        """
        Translates the GFE domain's siteID into the ID used in the VTEC activeTable.
        """
        idDict = {
            "NHZ": "KNHC",
            "HPA": "PHFO",
            "GUM": "PGUM",
            "PQE": "PGUM",
            "PQW": "PGUM",
        }

        if siteID in idDict:
            return idDict[siteID]

        return None

    def makeComboHazardKey(self, etn):
        """
        Returns a new Combo key.
        """
        return "TR.W:" + str(etn) + "^HU.A:" + str(etn)

    def execute(self, editArea, varDict):

        # get the hazard utilities

        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)
        # Clean out any current Hazards grids
        self.deleteGrid(self.mutableID(), "Hazards", "SFC", TimeRange.allTimes())

        siteID = self.getSiteID()
        # Fetch the activeTableID to filter out all other messages
        activeTableID = self.activeTableID(siteID)
        if activeTableID is None:
            self.statusBarMsg("Invalid siteID for center: " + siteID + ".", "S")
            return

        phenList = ["HU", "TR", "TY"]
        activeTable = self.getActiveTableInfo(activeTableID, phenList)

        # Check for empty dict
        if not activeTable:
            self.statusBarMsg("Active table not found or empty.", "S")
            return

        twoDays = 48 * 3600
        for hazInfo in activeTable:
            key, start, end, etn = hazInfo
            # Clip timeRange two day duration
            if end - start > twoDays:
                end = start + twoDays
            timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                            AbsTime.AbsTime(end))
            zoneList = activeTable[hazInfo].get("zoneList", None)
            if not zoneList:
                print("ZoneList not found in activeTable. hazInfo:", hazInfo)
                continue
            for zone in zoneList:
                ea = self.getEditArea(zone)
                if ea is None:
                    print("Zone:", zone, "edit area not found.")
                    continue
                mask = self.encodeEditArea(ea)
                if "^" in key:
                    hazKey = self.makeComboHazardKey(activeTable[hazInfo]["etn"])
                else:
                    hazKey = key + ":" + str(activeTable[hazInfo]["etn"])

                self._hazUtils._addHazard("Hazards", timeRange, hazKey, mask, combine=1)

        self.saveElements(["Hazards"])
