##
##

# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.slice import AbstractGridSlice

class ScalarGridSlice(AbstractGridSlice):

    def __init__(self):
        super(ScalarGridSlice, self).__init__()
        self.scalarGrid = None
        
    def getNumPyGrid(self):
        return self.scalarGrid.getNumPyGrid()

    def getScalarGrid(self):
        return self.scalarGrid

    def setScalarGrid(self, scalarGrid):
        self.scalarGrid = scalarGrid
