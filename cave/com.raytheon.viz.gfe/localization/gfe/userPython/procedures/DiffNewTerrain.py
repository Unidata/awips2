# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# DiffNewTerrain.py
#
# Creates temporary elements newTerrainDiff and oldTerrainDiff by subtracting the 
# GFE Topo grid from the NewTerrain and OldTerrain elements respectively. 
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer     Description
# ------------ ---------- -----------  --------------------------
# Jan 13, 2015  #3955     randerso     Initial creation
#
# Author: randerso
# ----------------------------------------------------------------------------

MenuItems = ["Edit"]

VariableList = []

import SmartScript
import TimeRange
import numpy
## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        newTerrainDbId = self.findDatabase("EditTopo_NewTerrain")
            
        newTerrain = self.getGrids(newTerrainDbId, "NewTerrain", "SFC", timeRange, mode="First")
        oldTerrain = self.getGrids(newTerrainDbId, "OldTerrain", "SFC", timeRange, mode="First")

        topoDbId = self.findDatabase("EditTopo_Topo")
        topo = self.getGrids(topoDbId, "Topo", "SFC", timeRange, mode="First")

        self.unloadWEs(newTerrainDbId, [("newTerrainDiff", "SFC"),("oldTerrainDiff", "SFC")])

        delta = newTerrain - topo
        maxVal = numpy.nanmax(delta)
        minVal = numpy.nanmin(delta)
        maxDelta = max(abs(maxVal), abs(minVal))
        self.createGrid(newTerrainDbId, "newTerrainDiff", "SCALAR", delta, TimeRange.allTimes(), 
                        "NewTerrain - Topo", (0,60,60), 0, -maxDelta, maxDelta, "ft", defaultColorTable="GFE/Delta")

        delta = oldTerrain - topo
        maxVal = numpy.nanmax(delta)
        minVal = numpy.nanmin(delta)
        maxDelta = max(1.0, abs(maxVal), abs(minVal))
        self.createGrid(newTerrainDbId, "oldTerrainDiff", "SCALAR", delta, TimeRange.allTimes(), 
                        "OldTerrain - Topo", (0,60,60), 0, -maxDelta, maxDelta, "ft", defaultColorTable="GFE/Delta")
