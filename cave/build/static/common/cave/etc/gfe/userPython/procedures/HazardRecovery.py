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
# HazardRecovery
#
# This procedure synchonizes the hazards grids with the contents of the
# VTEC active table.
#
#
# Author: lefebvre/mathewson
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Hazards"]

from numpy import *
import SmartScript
import time
import HazardUtils
import ProcessVariableList
import LogStream


MYSITEONLY = 0   #set to 1 to only have your sites be considered

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss

    def getActiveTable(self, site4ID=None, allZones=None):
        #gets the active table, filtered by site4ID, and list of zones
        activeTable = {}
        vtecTable = self.vtecActiveTable()        
        vtecTable = self._hazUtils._filterVTECBasedOnGFEMode(vtecTable)
        
        actionList = ["NEW", "EXA", "EXB", "EXT", "CON"]
        tropicalPhens = ["HU", "TY", "TR"]
        SPCPhens = ["TO", "SV"]
        skipPhenSig = [('FA','W'), ('FF','W'), ('FL','W'), ('FL','Y'), 
                       ('MA','W'), ('SV','W'), ('TO','W'), ('EW','W')]

        currentTime = int(time.time() / 3600) * 3600  #truncated
        
        for v in vtecTable:
            # filter based on zones
            if allZones is not None and v['id'] not in allZones:
                continue
       
            # filter based on actionCode
            action = v['act']
            if action not in actionList:
                continue

            # filter out phen/sigs to skip (short-fused)
            phen = v['phen']
            sig =  v['sig']
            if (phen,sig) in skipPhenSig:
                continue

            #set up checks
            if MYSITEONLY:
                spcsiteCheck = (v['officeid'] == site4ID)
                nonNatlCheck = (v['officeid'] == site4ID)
            else:
                spcsiteCheck = (v['officeid'] != 'KWNS')
                nonNatlCheck = (v['officeid'] not in ['KNHC','KWNS'])

            #filter
            hazKey = None
            if v['phen'] in SPCPhens and spcsiteCheck and \
               v['sig'] == "A":
                if v['pil'] in ['WCN']:
                    hazKey = v['phen'] + "." + v['sig'] + ":" + str(v['etn'])
            elif nonNatlCheck:
                hazKey = v['phen'] + "." + v['sig']
            elif v['phen'] in tropicalPhens and v['officeid'] == "KNHC":
                hazKey = v['phen'] + "." + v['sig']

            if hazKey is None:
                continue
            
            startTm = v['startTime']
            # turncate the startTime to the top of the hour
            startTm = int(startTm / 3600) * 3600

            # filter out past hazards
            if v['endTime'] < currentTime or \
              (v['officeid'] == "KNHC" and \
              (v['issueTime'] + 36*3600 < currentTime)):
                continue

            # end times can be insanely large. Arbitrarily trim them to 36hr
            # for UntilFurtherNotice
            endTm = v['endTime']
            if v.get('ufn', 0):
                endTm = currentTime + (36 * 3600)

            # now adjust starting times if event has already started to
            # be the current hour
            if startTm < currentTime:
                startTm = currentTime

            dictKey = (hazKey, startTm, endTm, v['act'])

            if not dictKey in activeTable:
                activeTable[dictKey] = []
                
            activeTable[dictKey].append(v['id'])

        return activeTable


    def execute(self, editArea, timeRange, varDict):
        # get the hazard utilities
        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)

        self.setToolType("numeric")

        # any temporary grids?
        if self._hazUtils._tempWELoaded():
            self.statusBarMsg("Unload temporary hazard weather elements before running HazardRecovery.",
                              "S")
            return

        # get the active table
        site4ID = self.getSite4ID(self.getSiteID())
        allZones = self.editAreaList()
        activeTable = self.getActiveTable(site4ID, allZones)

        # define this to define the dialog
        variableList = [
            ("Your entire Hazards inventory will be replaced with the " + \
             "contents of the active table.","", "label"),
            ]
        
        # call this to pop the dialog
        title = "Hazard Recovery"
        processVarList = ProcessVariableList.ProcessVariableList(title, \
                         variableList, varDict, parent = None)
        status = processVarList.status()
        if status.lower() != "ok":
            print "status:",  status
            LogStream.logDebug("HazardRecovery: cancel")
            return         
        LogStream.logDebug("HazardRecovery: OK")
        
        # see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in the GFE" + \
                              " before running HazardRecovery", "S")
            return

        # remove all of the current hazard grids
        self._hazUtils._removeAllHazardsGrids()
        self._hazUtils._unlockHazards()
        
        # any hazards at all?
        if len(activeTable) == 0:
            self.statusBarMsg("There are no hazards in the active table to recover. Hazard grids have been cleared.",
                              "S")
            # return

        
        keys = activeTable.keys()
        allzones = self.editAreaList()
        for key, start, end, action in keys:
            timeRange = self._hazUtils._makeTimeRange(start, end)
            zoneList = activeTable[(key, start, end, action)]
            filteredZoneList = []
            for z in zoneList:
                if z in allzones:
                    filteredZoneList.append(z)

            mask = self._hazUtils._makeMask(filteredZoneList)
            self._hazUtils._addHazard("Hazards", timeRange, key, mask)

            LogStream.logEvent(self._hazUtils._printTime(start),
              self._hazUtils._printTime(end), key, zoneList)



        return
