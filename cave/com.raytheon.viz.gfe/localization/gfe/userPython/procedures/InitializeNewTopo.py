# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# InitializeNewTopo.py
#
# Copies current GFE Topo grid to CurrentTopo element in NewTerrain database.
# Populates NewTopo element from GMTED2010 topo dataset.
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

    def execute(self):
        mutableID = self.mutableID()
        newTerrainDbId = self.findDatabase("EditTopo_NewTerrain")
        if mutableID != newTerrainDbId:
            DisplayMessageDialog.openError("Invalid Mutable Database", 
            "You must be using the NewTerrain GFE configuration to run this procedure.\n"
            "Please restart CAVE and select the NewTerrain GFE configuration.")
            return

        allTimes = TimeRange.allTimes()
        baseTerrainDbId = self.findDatabase("EditTopo_BaseTerrain")
        gtopoParm = self.getParm(baseTerrainDbId, "GTOPO", "SFC")
        gtopoGrid = self.getTerrainGrid("gtopo30")
        gtopoParm.setMutable(True)
        self.createGrid(baseTerrainDbId, "GTOPO", "SCALAR", gtopoGrid, allTimes)
        gtopoParm.saveParameter(True)
        gtopoParm.setMutable(False)
            
        gmtedParm = self.getParm(baseTerrainDbId, "GMTED", "SFC")
        gmtedGrid = self.getTerrainGrid("gmted2010")
        gmtedParm.setMutable(True)
        self.createGrid(baseTerrainDbId, "GMTED", "SCALAR", gmtedGrid, allTimes)
        gmtedParm.saveParameter(True)
        gmtedParm.setMutable(False)
            
        newTopoParm = self.getParm(newTerrainDbId, "NewTopo", "SFC")
        inventory = newTopoParm.getGridInventory()
        doIt = True
        if len(inventory) > 0:
            doIt = DisplayMessageDialog.openQuestion("NewTopo Grid exists!", 
                   "Do you wish to re-initialize the NewTopo Grid discarding any edits?")

        if doIt:
            self.createGrid(newTerrainDbId, "NewTopo", "SCALAR", gmtedGrid, allTimes)
            newTopoParm.saveParameter(True)

        self.setActiveElement(newTerrainDbId, "NewTopo", "SFC", allTimes)

    def getTerrainGrid(self, topoFile):
        gloc = self.getGridLoc()
        
        from com.raytheon.uf.common.geospatial import MapUtil
        gridGeometry = MapUtil.getGridGeometry(gloc)
        
        from com.raytheon.uf.common.topo import TopoQuery
        from java.io import File
        fileName = "/topo/" + topoFile + ".h5"
        terrainData = TopoQuery.getInstance(File(fileName), 0).getHeight(gridGeometry)
        grid = numpy.array(terrainData).reshape(self.getGridShape())
        grid /= 0.3048 # convert from meters to feet
        
        return grid
        