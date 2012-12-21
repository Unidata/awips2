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
# Implements IGeometryData and wraps around a Java IGeometryData.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen       Initial Creation.
#    
# 
#

from ufpy.dataaccess import IGeometryData
import JData
import shapely.wkt

class JGeometryData(IGeometryData, JData.JData):
    
    def __init__(self, wrappedObject):
        JData.JData.__init__(self, wrappedObject)
        
    def __getitem__(self, key):
        if key == 'geometry':
            return self.getGeometry()
        elif key == 'parameters':
            return self.getParameters()
        elif key == 'unit':
            return self.getUnit()
        elif key =='locationName':
            return self.getLocationName()
        elif key == 'time':
            return self.getDataTime()
        elif key == 'level':
            return self.getLevel()
        else:
            t = str(self.getType(key))
            if t == 'STRING':
                return self.getString(key)
            else:
                return self.getNumber(key)

    def getGeometry(self):
        return shapely.wkt.loads(self.jobj.getGeometry().toText())
    
    def getParameters(self):        
        params = []
        jparams = self.jobj.getParameters()
        itr = jparams.iterator()
        while itr.hasNext():
            params.append(itr.next())                
    
    def getString(self, param):
        return str(self.jobj.getString(param))
    
    def getNumber(self, param):         
        jval = self.jobj.getNumber(param)
        t = self.getType(param)        
        if t == 'INT':            
            return jval.intValue()
        elif t == 'LONG':
            return jval.longValue()
        elif t == 'FLOAT':
            return jval.floatValue()
        elif t == 'DOUBLE':
            return jval.doubleValue()
        else:
            return jval
    
    def getUnit(self, param):
        return str(self.jobj.getUnit(param))
    
    def getType(self, param):
        return str(self.jobj.getType(param))
    
    def getLocationName(self):
        return self.jobj.getLocationName()
