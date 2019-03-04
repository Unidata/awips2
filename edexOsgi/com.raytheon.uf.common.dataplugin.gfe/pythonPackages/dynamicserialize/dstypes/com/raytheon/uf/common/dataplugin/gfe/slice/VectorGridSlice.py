##
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
