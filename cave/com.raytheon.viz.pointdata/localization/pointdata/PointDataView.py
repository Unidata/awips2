##
##


#
# Python wrapper for PointDataView
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/20/09                      njensen       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

class PointDataView:

    def __init__(self, javaPointDataView):        
        self.__javaPdv = javaPointDataView   
        self.__keys = []
        keyset = self.__javaPdv.getContainer().getParameters()
        itr = keyset.iterator()
        while itr.hasNext():
            self.__keys.append(str(itr.next()))
            
    def __getitem__(self, key):
        result = None        
        strValType = self.getType(key)
        if strValType == 'FLOAT':
            result = self.__javaPdv.getFloat(key)
        elif strValType == 'STRING':
            result = self.__javaPdv.getString(key)
        elif strValType == 'INT':
            result = self.__javaPdv.getInt(key)
        elif strValType == 'LONG':
            result = self.__javaPdv.getLong(key)
            
        return result

    def getType(self, key):
        val = self.__javaPdv.getType(key)
        if val:
            val = str(val)
        return val
    
    def has_key(self, key):
        return self.__keys.__contains__(key)
    
    def keys(self):
        return self.__keys
    
    def __contains__(self, key):
        return self.has_key(key)
    
    def getFillValue(self, key):
        # TODO if we get fill value support in pointdata, hook that up
        return -9999.0
    
    def getNumberAllLevels(self, key):
        strValType = self.getType(key)
        jlevels = self.__javaPdv.getNumberAllLevels(key)
        levels = []
        for level in jlevels:
            level = str(level)
            if strValType == 'FLOAT':
                levels.append(float(level))
            elif strValType == 'STRING':
                levels.append(str(level))
            elif strValType == 'INT':
                levels.append(int(level))
            elif strValType == 'LONG':
                levels.append(long(level))
        return levels
        

    
        
        
            
    