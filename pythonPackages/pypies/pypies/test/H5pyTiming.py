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