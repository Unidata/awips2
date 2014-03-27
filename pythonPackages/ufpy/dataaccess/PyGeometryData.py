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
# Implements IGeometryData for use by native Python clients to the Data Access
# Framework.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/03/13                      dgilling      Initial Creation.
#    01/06/14         #2537        bsteffen       Share geometry WKT.
#    
#

from ufpy.dataaccess import IGeometryData
from ufpy.dataaccess import PyData

class PyGeometryData(IGeometryData, PyData.PyData):
    
    def __init__(self, geoDataRecord, geometry):
        PyData.PyData.__init__(self, geoDataRecord)
        self.__geometry = geometry
        self.__dataMap = {}
        tempDataMap = geoDataRecord.getDataMap()
        for key, value in tempDataMap.items():
            self.__dataMap[key] = (value[0], value[1], value[2])

    def getGeometry(self):
        return self.__geometry
    
    def getParameters(self):        
        return self.__dataMap.keys()
    
    def getString(self, param):
        value = self.__dataMap[param][0]
        return str(value)
    
    def getNumber(self, param):         
        value = self.__dataMap[param][0]
        t = self.getType(param)        
        if t == 'INT':            
            return int(value)
        elif t == 'LONG':
            return long(value)
        elif t == 'FLOAT':
            return float(value)
        elif t == 'DOUBLE':
            return float(value)
        else:
            return value
    
    def getUnit(self, param):
        return self.__dataMap[param][2]
    
    def getType(self, param):
        return self.__dataMap[param][1]
