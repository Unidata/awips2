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