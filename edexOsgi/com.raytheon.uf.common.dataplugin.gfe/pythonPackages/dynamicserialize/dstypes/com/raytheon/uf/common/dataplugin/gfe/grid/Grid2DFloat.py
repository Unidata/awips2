##
##

# File auto-generated against equivalent DynamicSerialize Java class

import numpy


class Grid2DFloat(object):

    def __init__(self):
        self.buffer = None
        self.xdim = None
        self.ydim = None

    def getBuffer(self):
        return self.buffer

    def setBuffer(self, buffer):
        self.buffer = buffer

    def getXdim(self):
        return self.xdim

    def setXdim(self, xdim):
        self.xdim = xdim

    def getYdim(self):
        return self.ydim

    def setYdim(self, ydim):
        self.ydim = ydim
        
    def getNumPyGrid(self):
        return numpy.resize(self.buffer, (self.xdim, self.ydim))
