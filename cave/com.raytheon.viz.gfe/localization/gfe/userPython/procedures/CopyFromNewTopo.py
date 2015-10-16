# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CopyFromNewTopo.py
#
# Creates a backup copy of the current GFE Topo in PrevTopo
# and then replaces the GFE Topo with the NewTopo
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

from com.raytheon.viz.gfe.ui.runtimeui import DisplayMessageDialog

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, timeRange):
        mutableID = self.mutableID()
        topoDbId = self.findDatabase("EditTopo_Topo")
        if mutableID != topoDbId:
            DisplayMessageDialog.openError("Invalid Mutable Database", 
            "You must be using the EditTopo GFE configuration to run this procedure.\n"
            "Please restart CAVE and select the EditTopo GFE configuration.")
            return
        
        # check for existence of NewTopo grid
        newTerrainDbId = self.findDatabase("EditTopo_NewTerrain")
        newTopoParm = self.getParm(newTerrainDbId, "NewTopo", "SFC")
        inventory = newTopoParm.getGridInventory()
        if len(inventory) == 0:
            DisplayMessage.openError("No NewTopo grid exists!",
            "You must initialize and edit the NewTopo grid as desired.\n" 
            "You should run this procedure only when your edits are final.")
            return

        doIt = DisplayMessageDialog.openQuestion("WARNING!", 
           "You are about to replace your GFE Topo data with the NewTopo data.\n"
           "You should only do this after you have completed all edits and have\n"
           "resolved any border differences with your neighboring sites.\n\n"
           "Do you wish to continue?")
            
        if not doIt:
            return;
        
        baseTerrainDbId = self.findDatabase("EditTopo_BaseTerrain")
        prevTopoParm = self.getParm(baseTerrainDbId, "PrevTopo", "SFC")
        inventory = prevTopoParm.getGridInventory()
        doIt = True
        if len(inventory) > 0:
            doIt = DisplayMessageDialog.openQuestion("WARNING PrevTopo exists!", 
                   "It appears that you have previously run CopyFromNewTopo and a\n"
                   "backup copy of your original GFE Topo grid already exists.\n\n"
                   "Do you want to overwrite the backup copy with your current GFE Topo grid?")
        if doIt:
            # Save the current Topo grid to PrevTopo
            topo = self.getGrids(topoDbId, "Topo", "SFC", timeRange, mode="First")
            prevTopoParm.setMutable(True)
            self.createGrid(baseTerrainDbId, "PrevTopo", "SCALAR", topo, TimeRange.allTimes())
            prevTopoParm.saveParameter(True)
            prevTopoParm.setMutable(False)

        # Copy the NewTopo data to Topo
        newTopo = self.getGrids(newTerrainDbId, "NewTopo", "SFC", timeRange, mode="First")

        topoParm = self.getParm(topoDbId, "Topo", "SFC")
        self.createGrid(topoDbId, "Topo", "SCALAR", newTopo, TimeRange.allTimes())
        topoParm.saveParameter(True)

        DisplayMessageDialog.openInformation("Topo Successfully Updated!", 
        "To revert to the previous version, run the RevertTopo procedure.")
            