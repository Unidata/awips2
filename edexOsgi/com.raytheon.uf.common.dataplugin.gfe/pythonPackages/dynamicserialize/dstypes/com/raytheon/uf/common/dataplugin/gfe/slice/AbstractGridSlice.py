##
##

import abc


class AbstractGridSlice(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __init__(self):
        self.validTime = None
        self.gridParmInfo = None
        self.gridDataHistory = None
        
    @abc.abstractmethod
    def getNumPyGrid(self):
        raise NotImplementedError
        
    def getValidTime(self):
        return self.validTime

    def setValidTime(self, validTime):
        self.validTime = validTime

    def getGridParmInfo(self):
        return self.gridParmInfo

    def setGridParmInfo(self, gridParmInfo):
        self.gridParmInfo = gridParmInfo

    def getGridDataHistory(self):
        return self.gridDataHistory

    def setGridDataHistory(self, gridDataHistory):
        self.gridDataHistory = gridDataHistory