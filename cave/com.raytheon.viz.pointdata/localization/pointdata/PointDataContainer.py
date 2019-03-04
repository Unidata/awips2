##
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

##
# This is a base file that is not intended to be overridden.
##

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

    
    