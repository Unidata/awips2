##
##

# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.slice import AbstractGridSlice


class DiscreteGridSlice(AbstractGridSlice):

    def __init__(self):
        super(DiscreteGridSlice, self).__init__()
        self.discreteGrid = None
        self.key = []

    def getDiscreteGrid(self):
        return self.discreteGrid

    def setDiscreteGrid(self, discreteGrid):
        self.discreteGrid = discreteGrid
        
    def getNumPyGrid(self):
        return (self.discreteGrid.getNumPyGrid(), self.key)

    def getKey(self):
        return self.key

    def setKey(self, key):
        self.key = key
