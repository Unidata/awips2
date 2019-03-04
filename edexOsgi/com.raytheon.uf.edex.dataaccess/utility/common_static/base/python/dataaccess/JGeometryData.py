##
##

#
# Implements IGeometryData and wraps around a Java IGeometryData.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen        Initial Creation.
#    06/03/13         #2023        dgilling       Remove "unit" support from
#                                                 __getitem__ as it only threw errors.
#    08/06/14          3185        njensen        Only import shapely when necessary
#    01/30/18          7183        mapeters       Remove number unboxing calls in
#                                                 getNumber() for Jep 3.6 upgrade
#
#

##
# This is a base file that is not intended to be overridden.
##



from awips.dataaccess import IGeometryData
import JData

class JGeometryData(IGeometryData, JData.JData):
    
    def __init__(self, wrappedObject):
        JData.JData.__init__(self, wrappedObject)
        
    def __getitem__(self, key):
        if key == 'geometry':
            return self.getGeometry()
        elif key == 'parameters':
            return self.getParameters()
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
        import shapely.wkt
        return shapely.wkt.loads(self.jobj.getGeometry().toText())
    
    def getParameters(self):        
        params = []
        jparams = self.jobj.getParameters()
        itr = jparams.iterator()
        while itr.hasNext():
            params.append(str(itr.next()))
        return params          
    
    def getString(self, param):
        return str(self.jobj.getString(param))
    
    def getNumber(self, param):         
        jval = self.jobj.getNumber(param)
        t = self.getType(param)        
        if t == 'INT' or t == 'LONG':            
            return int(jval)
        elif t == 'FLOAT' or t == 'DOUBLE':
            return float(jval)
        else:
            return jval
    
    def getUnit(self, param):
        return str(self.jobj.getUnit(param))
    
    def getType(self, param):
        return str(self.jobj.getType(param))
