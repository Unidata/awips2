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
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ----------------------------------------
# Apr 24, 2020 21020      tlefebvr    Original Verison.
# May  5, 2020 21020      tlefebvr    Prompts user to delete Hazards if empty
#                                     activeTable.
# May 20, 2020 22033                  Addressed code review comments.
#
# ----------------------------------------------------------------------------
MenuItems = ["Populate"]

import AbsTime, TimeRange
import SmartScript
import HazardUtils
import ProcessVariableList

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss
       
    def getActiveTableInfo(self, phenList=None):
        #gets the active table, filtered by the specified phen list
        activeTable = {}
        vtecTable = self.vtecActiveTable()        
        vtecTable = self._hazUtils._filterVTECBasedOnGFEMode(vtecTable)
        
        actionList = ["NEW", "EXA", "EXB", "EXT", "CON"]
        
        for v in vtecTable:
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
            
            hazKey = v['phen'] + "." + v['sig']

            dictKey = (hazKey, startTm, endTm, v['act'])

            if not dictKey in activeTable:
                activeTable[dictKey] = {}
                activeTable[dictKey]["etn"] = etn
            
            if not "zoneList" in activeTable[dictKey]:
                activeTable[dictKey]["zoneList"] = []
                
            activeTable[dictKey]["zoneList"].append(v['id']) 

        return activeTable

    # Make a temporary dialog to see if the user wants to continue.        
    def askToDeleteHazards(self):
        """ Pops a dialog and asks the user if they want to save before
        exiting.
        """
        variableList = []
        variableList.append(("Delete Expired Hazards?", "Yes", "radio", ["Yes", "No"]))

        # Display the GUI
        varDict = {}
        processVarList = ProcessVariableList.ProcessVariableList(
            "Hazards Have Expired for ", variableList, varDict)
        status = processVarList.status()
        if status.upper() == "OK":
            if varDict["Delete Expired Hazards?"] == "Yes":
                self.deleteGrid(self.mutableID(), "Hazards", "SFC", TimeRange.allTimes())
                self.saveElements(["Hazards"])
        
        return
    
    def execute(self, editArea, timeRange, varDict):

        # get the hazard utilities
        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)

        phenList = ["HU", "TR", "TY"]
        activeTable = self.getActiveTableInfo(phenList)

        # Check for empty dict
        if not activeTable:            
            self.askToDeleteHazards()
            return  
        
        for hazInfo in activeTable:
            key, start, end, vtecState = hazInfo
            
            for zone in activeTable[hazInfo]["zoneList"]:
                ea = self.getEditArea(zone)
                if ea is None:
                    continue
                mask = self.encodeEditArea(ea)
                timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                                AbsTime.AbsTime(end))
                hazKey = key + ":" + str(activeTable[hazInfo]["etn"])
                self._hazUtils._addHazard("Hazards", timeRange, hazKey, mask, combine=0)
