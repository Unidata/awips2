##
##


#
# TODO
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/20/10                      njensen       Initial Creation.
#    
# 
#

import TimingInterface, tables, numpy

class TablesTiming(TimingInterface.TimingInterface):
    
    def __init__(self, filename, mode="r"):
        self.file = tables.openFile(filename, mode) 
    
    def createGroup(self, name):
        return self.file.createGroup('/', name)
        
    def createDataset(self, group, name, value, nDimensions=1):
        if nDimensions == 1:
            ds = self.file.createVLArray(group, name, tables.Float32Atom(shape=()))
            if type(value) is numpy.ndarray:
                for x in value:
                    ds.append([x])
            else:
                ds.append(value)
        elif nDimensions == 2:
            ds = self.file.createArray(group, name, value)
        return ds
    
    def close(self):
        self.file.flush()
        self.file.close()
    
    def appendValue(self, dataset, value):        
        dataset.append([value])
    
    def getDataset(self, name):
        return self.file.getNode(name)
    
    def sampleValue(self, dataset, index):
        return dataset[index]