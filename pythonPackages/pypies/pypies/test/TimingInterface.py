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
    
    
    
    