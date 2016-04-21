# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# InitializeNewTerrain.py
#
# Copies current GFE Topo grid to OldTerrain element in NewTerrain database.
# Populates NewTerrain element from GMTED2010 topo dataset.
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

from com.raytheon.viz.gfe.ui.runtimeui import DisplayMessageDialog

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        mutableID = self.mutableID()
        topoDbId = self.findDatabase("EditTopo_Topo")
        if mutableID != topoDbId:
            DisplayMessageDialog.openError("Invalid Mutable Database", "You must be using the NewTerrain GFE configuration to run this procedure.\nPlease restart CAVE and select the NewTerrain GFE configuraion")
            return
            
        newTerrainDbId = self.findDatabase("EditTopo_NewTerrain")

        newTerrainParm = self.getParm(newTerrainDbId, "NewTerrain", "SFC")
        inventory = newTerrainParm.getGridInventory()
        doIt = True
        if len(inventory) > 0:
            doIt = DisplayMessageDialog.openQuestion("NewTerrain Grid exists!", "Do you wish to re-initialize the NewTerrain Grid discarding all your edits?")

        if doIt:
            newTerrainParm.setMutable(True)
            newTerrainGrid = self.getTerrainGrid()
            self.createGrid(newTerrainDbId, "NewTerrain", "SCALAR", newTerrainGrid, TimeRange.allTimes())
            newTerrainParm.saveParameter(True)
            newTerrainParm.setMutable(False)

        oldTerrainParm = self.getParm(newTerrainDbId, "OldTerrain", "SFC")
        inventory = oldTerrainParm.getGridInventory()
        doIt = True
        if len(inventory) > 0:
            doIt = DisplayMessageDialog.openQuestion("OldTerrain Grid exists!", "Do you wish to update it?")

        if doIt:
            oldTerrainParm.setMutable(True)
            oldTerrainGrid = self.getTopo()
            self.createGrid(newTerrainDbId, "OldTerrain", "SCALAR", oldTerrainGrid, TimeRange.allTimes())
            oldTerrainParm.saveParameter(True)
            oldTerrainParm.setMutable(False)

        self.setActiveElement(topoDbId, "Topo", "SFC", TimeRange.allTimes())

    def getTerrainGrid(self):
        gloc = self.getGridLoc()
        
        from com.raytheon.uf.common.geospatial import MapUtil
        gridGeometry = MapUtil.getGridGeometry(gloc)
        
        from com.raytheon.uf.common.topo import TopoQuery
        from java.io import File
        terrainData = TopoQuery.getInstance(File("/topo/gmted2010.h5"), 0).getHeight(gridGeometry)
        
        from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DFloat
        grid = Grid2DFloat.createGrid(gloc.getNx().intValue(), gloc.getNy().intValue(), terrainData).getNDArray()
        grid /= 0.3048 # convert from meters to feet
        
        return grid
        