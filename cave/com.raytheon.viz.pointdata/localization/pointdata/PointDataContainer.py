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


import PointDataView, NoDataException

#
# Python wrapper for point data
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/20/09                      njensen       Initial Creation.
#    05/11/11                      njensen       Split out data request to PointDataRetrieve.py
#    
# 
#
    
class PointDataContainer:

    def __init__(self, pdvDict, javaPdc, refTime):        
        self.__pdvDict = pdvDict        
        self.__javaPdc = javaPdc
        self.refTime = refTime
        
    def __getitem__(self, key):
        return self.__pdvDict[key]
    
    def has_key(self, key):
        return self.__pdvDict.has_key(key)
    
    def keys(self):
        return self.__pdvDict.keys()
    
    def __contains__(self, key):
        return self.has_key(key)
    
    def hasParam(self, param):
        return self.__javaPdc.getParameters().contains(param)
    
    def __repr__(self):
        return "PointDataContainer: " + str(self.__pdvDict)

    
    