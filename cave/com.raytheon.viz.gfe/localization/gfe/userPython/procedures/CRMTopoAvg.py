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
# DisplayCRMMaxMin
#
# Author: LeFebvre/Santos
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

import SmartScript
import time
import AbsTime, DatabaseID, TimeRange
from numpy import *


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
    def getWEInventory(self, modelName, WEName, level):
        allTimes = TimeRange.allTimes()
        gridInfo = self.getGridInfo(modelName, WEName, level, allTimes)
        trList = []
        for g in gridInfo:
            start = g.gridTime().startTime().unixTime()
            end = g.gridTime().endTime().unixTime()
            tr = TimeRange.TimeRange(AbsTime.AbsTime(start),
                                AbsTime.AbsTime(end))
            trList.append(tr)

        return trList

    def getAvgTopoGrid(self):

        dbs = self.availableDatabases()
        for db in dbs:
            if db.modelIdentifier().find("CRMTopo") > -1:
                dbID = db
                break

        trList = self.getWEInventory(dbID, "avgTopo", "SFC")

        topoGrid = self.getGrids(dbID, "avgTopo", "SFC",
                                 trList[0], mode="First")

        # convert to feet
        topoGridC = topoGrid * 3.281
        topoGridC[less(topoGridC,0.0)] = 0.0
        #topoGrid = where(greater(topoGrid,200.0),200.0,topoGrid)
       
        return topoGridC

    def getAvgTopoGridNew(self):

        dbs = self.availableDatabases()
        for db in dbs:
            if db.modelIdentifier().find("NED") > -1:
                dbID = db
                break

        trList = self.getWEInventory(dbID, "avgTopo", "SFC")

        topoGrid = self.getGrids(dbID, "avgTopo", "SFC",
                                 trList[0], mode="First")

        # convert to feet
        topoGridC = topoGrid * 3.281
        topoGridC[less(topoGridC,-50.0)] = 0.0
        #topoGrid = where(greater(topoGrid,200.0),200.0,topoGrid)
       
        return topoGridC

    def getMaxTopoGrid(self):

        dbs = self.availableDatabases()
        for db in dbs:
            if db.modelIdentifier().find("CRMTopo") > -1:
                dbID = db
                break

        trList = self.getWEInventory(dbID, "maxTopo", "SFC")

        topoGrid = self.getGrids(dbID, "maxTopo", "SFC",
                                 trList[0], mode="First")

        # convert to feet
        topoGridC = topoGrid * 3.281
        topoGridC[less(topoGridC,-50.0)] = 0.0
        #topoGrid = where(greater(topoGrid,200.0),200.0,topoGrid)

        
        return topoGridC

    def getMaxTopoGridNew(self):

        dbs = self.availableDatabases()
        for db in dbs:
            if db.modelIdentifier().find("NED") > -1:
                dbID = db
                break

        trList = self.getWEInventory(dbID, "maxTopo", "SFC")

        topoGrid = self.getGrids(dbID, "maxTopo", "SFC",
                                 trList[0], mode="First")

        # convert to feet
        topoGridC = topoGrid * 3.281
        topoGridC[less(topoGridC,-50.0)] = 0.0
        #topoGrid = where(greater(topoGrid,200.0),200.0,topoGrid)
       
        return topoGridC    

    def getMinTopoGrid(self):

        dbs = self.availableDatabases()
        for db in dbs:
            if db.modelIdentifier().find("CRMTopo") > -1:
                dbID = db
                break

        trList = self.getWEInventory(dbID, "minTopo", "SFC")

        topoGrid = self.getGrids(dbID, "minTopo", "SFC",
                                 trList[0], mode="First")

        # convert to feet
        topoGridC = topoGrid * 3.281
        topoGridC[less(topoGridC,-50.0)] = 0.0
        #topoGrid = where(greater(topoGrid,200.0),200.0,topoGrid)
        
        return topoGridC

    def getMinTopoGridNew(self):

        dbs = self.availableDatabases()
        for db in dbs:
            if db.modelIdentifier().find("NED") > -1:
                dbID = db
                break

        trList = self.getWEInventory(dbID, "minTopo", "SFC")

        topoGrid = self.getGrids(dbID, "minTopo", "SFC",
                                 trList[0], mode="First")

        # convert to feet
        topoGridC = topoGrid * 3.281
        topoGridC[less(topoGridC,-50.0)] = 0.0
        #topoGrid = where(greater(topoGrid,200.0),200.0,topoGrid)
       
        return topoGridC 
    
    def execute(self):

        oldTopoGrid = self.getTopo().copy()
        oldTopoGrid[greater(oldTopoGrid,200.0)] = 200.0
        oldTopoGrid[less(oldTopoGrid,0.0)] = 0.0
        avgTopoGrid = self.getAvgTopoGrid()
        avgTopoGridNew = self.getAvgTopoGridNew()
        maxTopoGrid = self.getMaxTopoGrid()
        maxTopoGridNew = self.getMaxTopoGridNew()        
        minTopoGrid = self.getMinTopoGrid()
        minTopoGridNew = self.getMinTopoGridNew()        
        

        thisHour = int(time.time() / 3600) * 3600
        start = AbsTime.AbsTime(thisHour)
        end = AbsTime.AbsTime(thisHour + 3600)
        tr = TimeRange.TimeRange(start, end)

        self.createGrid("Fcst", "OLDTopo", "SCALAR", oldTopoGrid, tr,
                        minAllowedValue = 0.0, maxAllowedValue = 10000.0,
                        precision=2)

        self.createGrid("Fcst", "CRMTopoAvg", "SCALAR", avgTopoGrid, tr,
                        minAllowedValue = 0.0, maxAllowedValue = 10000.0,
                        precision=2)

        self.createGrid("Fcst", "CRMTopoAvgNew", "SCALAR", avgTopoGridNew, tr,
                        minAllowedValue = -50.0, maxAllowedValue = 10000.0,
                        precision=2)     

        self.createGrid("Fcst", "CRMTopoMax", "SCALAR", maxTopoGrid, tr,
                        minAllowedValue = -50.0, maxAllowedValue = 10000.0,
                        precision=2)
        
        self.createGrid("Fcst", "CRMTopoMaxNew", "SCALAR", maxTopoGridNew, tr,
                        minAllowedValue = -50.0, maxAllowedValue = 10000.0,
                        precision=2)
        
        
        self.createGrid("Fcst", "CRMTopoMin", "SCALAR", minTopoGrid, tr,
                        minAllowedValue = -50.0, maxAllowedValue = 10000.0,
                        precision=2)

        self.createGrid("Fcst", "CRMTopoMinNew", "SCALAR", minTopoGridNew, tr,
                        minAllowedValue = -50.0, maxAllowedValue = 10000.0,
                        precision=2)        
        
        return