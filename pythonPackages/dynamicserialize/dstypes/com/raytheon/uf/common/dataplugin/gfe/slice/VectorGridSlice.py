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

# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.slice import ScalarGridSlice


class VectorGridSlice(ScalarGridSlice):

    def __init__(self):
        super(VectorGridSlice, self).__init__()
        self.dirGrid = None
        
    def getNumPyGrid(self):
        return (self.scalarGrid.getNumPyGrid(), self.dirGrid.getNumPyGrid())

    def getDirGrid(self):
        return self.dirGrid

    def setDirGrid(self, dirGrid):
        self.dirGrid = dirGrid
        
    def getMagGrid(self):
        return self.scalarGrid

    def setMagGrid(self, magGrid):
        self.scalarGrid = magGrid
