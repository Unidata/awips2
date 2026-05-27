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
            