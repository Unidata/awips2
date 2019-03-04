##
##


#
# Timing tests for various storage plugins for the
# Python Process Isolated Enhanced Storage
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

import PiesExceptions

class TimingInterface:
    
    def __init__(self):
        pass
    
    def createGroup(self, name):
        raise PiesExceptions.NotImplementedException("createGroup")
        
    def createDataset(self, group, name, value, nDimensions=1):
        raise PiesExceptions.NotImplementedException("createDataset")
    
    def close(self):
        raise PiesExceptions.NotImplementedException("close")
    
    def appendValue(self, value):
        raise PiesExceptions.NotImplementedException("appendValue")
    
    def getDataset(self, name):
        raise PiesExceptions.NotImplementedException("getDataset")
    
    def sampleValue(self, dataset, index):
        raise PiesExceptions.NotImplementedException("sampleValue")
    
    def splitGroupData(self, name):
        x = name.split('/')
        return x[1:-1], x[-1]
    
    
    
    