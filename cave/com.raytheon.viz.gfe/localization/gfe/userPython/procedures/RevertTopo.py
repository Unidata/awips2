# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RevertTopo.py
#
# Restores the GFE Topo from the most recent backup in PrevTopo
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
        topoDbId = self.findDatabase("EditTopo_Topo")
        if mutableID != topoDbId:
            DisplayMessageDialog.openError("Invalid Mutable Database", 
            "You must be using the EditTopo GFE configuration to run this procedure.\n"
            "Please restart CAVE and select the EditTopo GFE configuration.")
            return
        
        # check for existence of PrevTopo grid
        baseTerrainDbId = self.findDatabase("EditTopo_BaseTerrain")
        prevTopoParm = self.getParm(baseTerrainDbId, "PrevTopo", "SFC")
        inventory = prevTopoParm.getGridInventory()
        if len(inventory) == 0:
            DisplayMessageDialog.openError("No PrevTopo grid exists!", 
                "It appears you have not yet run CopyFromNewTopo so you have\n"
                "no backup copy to restore.")

        doIt = DisplayMessageDialog.openQuestion("RevertTopo",
               "You are about to revert your GFE Topo data to the last saved state.\n\n"
               "Do you wish to continue?")
        
        if not doIt:
            return
        
        # Replace the current Topo grid with PrevTopo
        prevTopo = self.getGrids(baseTerrainDbId, "PrevTopo", "SFC", timeRange, mode="First")
        topoParm = self.getParm(topoDbId, "Topo", "SFC")
        self.createGrid(topoDbId, "Topo", "SCALAR", prevTopo, TimeRange.allTimes())
        topoParm.saveParameter(True)

        DisplayMessageDialog.openInformation("Success!", "Topo successfully restored!")
