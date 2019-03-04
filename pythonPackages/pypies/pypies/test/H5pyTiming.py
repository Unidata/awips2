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

import TimingInterface, h5py

class H5pyTiming(TimingInterface.TimingInterface):
    
    def __init__(self, filename, mode="r"):
        self.file = h5py.File(filename, mode) 
    
    def createGroup(self, name):
        return self.file.create_group(name)
        
    def createDataset(self, group, name, value, nDimensions=1):
        ds = group.create_dataset(name, value.shape, 'f', chunks=True)
        ds[:] = value
        return ds
    
    def close(self):
        self.file.flush()
        self.file.close()
    
    def appendValue(self, dataset, value):
        resize = (dataset.shape[0] + 1,)                        
        dataset.resize(resize)
        dataset[dataset.shape[0] - 1] = value
    
    def getDataset(self, name):
        return self.file[name]
    
    def sampleValue(self, dataset, index):
        return dataset[index]