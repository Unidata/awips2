# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# DiffNewTopo.py
#
# Creates the following temporary difference elements:
#   newTopoMinusTopo = NewTopo - Topo
#   GMTEDminusGTOPO  = GMTED - GTOPO
#   newEdits         = NewTopo - GMTED
#   currentEdits     = Topo - GTOPO
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer     Description
# ------------ ---------- -----------  --------------------------
# 10/13/2015    #4961     randerso     Initial creation
#
# Author: randerso
# ----------------------------------------------------------------------------

MenuItems = ["Edit"]

VariableList = []

import SmartScript
import TimeRange
import numpy

from com.raytheon.viz.gfe.ui.runtimeui import DisplayMessageDialog

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, timeRange):
        mutableID = self.mutableID()
        newTerrainDbId = self.findDatabase("EditTopo_NewTerrain")
        if mutableID != newTerrainDbId:
            DisplayMessageDialog.openError("Invalid Mutable Database", 
            "You must be using the NewTerrain GFE configuration to run this procedure.\n"
            "Please restart CAVE and select the NewTerrain GFE configuration.")
            return

        newTopo = self.getGrids(newTerrainDbId, "NewTopo", "SFC", timeRange, mode="First")
        
        baseTerrainDbId = self.findDatabase("EditTopo_BaseTerrain")
        gmted = self.getGrids(baseTerrainDbId, "GMTED", "SFC", timeRange, mode="First")
        gtopo = self.getGrids(baseTerrainDbId, "GTOPO", "SFC", timeRange, mode="First")

        topoDbId = self.findDatabase("EditTopo_Topo")
        topo = self.getGrids(topoDbId, "Topo", "SFC", timeRange, mode="First")

        self.unloadWEs(newTerrainDbId, 
                       [("newTopoMinusTopo", "SFC"),
                        ("GMTEDminusGTOPO", "SFC"),
                        ("newEdits", "SFC"),
                        ("currentEdits", "SFC")])

        delta = newTopo - topo
        maxVal = numpy.nanmax(delta)
        minVal = numpy.nanmin(delta)
        maxDelta = max(1.0, abs(maxVal), abs(minVal))
        self.createGrid(newTerrainDbId, "newTopoMinusTopo", "SCALAR", delta, TimeRange.allTimes(), 
                        "NewTopo - Topo", (0,60,60), 1, -maxDelta, maxDelta, "ft", defaultColorTable="GFE/Delta")

        delta = gmted - gtopo
        maxVal = numpy.nanmax(delta)
        minVal = numpy.nanmin(delta)
        maxDelta = max(1.0, abs(maxVal), abs(minVal))
        self.createGrid(newTerrainDbId, "GMTEDminusGTOPO", "SCALAR", delta, TimeRange.allTimes(), 
                        "GMTED - GTOPO", (0,60,60), 1, -maxDelta, maxDelta, "ft", defaultColorTable="GFE/Delta")

        delta = newTopo - gmted
        maxVal = numpy.nanmax(delta)
        minVal = numpy.nanmin(delta)
        maxDelta = max(1.0, abs(maxVal), abs(minVal))
        self.createGrid(newTerrainDbId, "newEdits", "SCALAR", delta, TimeRange.allTimes(), 
                        "NewTopo - GMTED", (0,60,60), 1, -maxDelta, maxDelta, "ft", defaultColorTable="GFE/Delta")

        delta = topo - gtopo
        maxVal = numpy.nanmax(delta)
        minVal = numpy.nanmin(delta)
        maxDelta = max(1.0, abs(maxVal), abs(minVal))
        self.createGrid(newTerrainDbId, "currentEdits", "SCALAR", delta, TimeRange.allTimes(), 
                        "Topo - GTOPO", (0,60,60), 1, -maxDelta, maxDelta, "ft", defaultColorTable="GFE/Delta")
