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

import TimingInterface, netCDF4

class Netcdf4Timing(TimingInterface.TimingInterface):
    
    def __init__(self, filename, mode="r"):
        self.file = netCDF4.Dataset(filename, mode, format='NETCDF4') 
    
    def createGroup(self, name):
        return self.file.createGroup(name)
        
    def createDataset(self, group, name, value, nDimensions=1):
        dims = []
        for i in range(nDimensions):
            n = str(i)
            dims.append(n)
            group.createDimension(n, None)                
        v = group.createVariable(name, value.dtype, tuple(dims))
        v[:] = value
        return v  
        
    
    def close(self):
        self.file.close()
    
    def appendValue(self, dataset, value):
        dataset[dataset.size] = value
    
    def getDataset(self, name):
        groups, v = self.splitGroupData(name)
        g = self.file.groups[groups[0]]
        for i in range(1, len(groups)):
            g = g.groups[groups[i]]
        return g.variables[v]        
    
    def sampleValue(self, dataset, index):
        return dataset[index]
            