##
##

#
# Implements IData and wraps around a Java IData
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen       Initial Creation.
#    06/03/13          #2023       dgilling      Implement getAttributes().
# 
#

##
# This is a base file that is not intended to be overridden.
##



from awips.dataaccess import IData
import JUtil, DataTime

class JData(IData, JUtil.JavaWrapperClass):
    
    def __init__(self, wrappedObject):
        self.jobj = wrappedObject
    
    def getAttribute(self, key):
        return self.jobj.getAttribute(key)
    
    def getAttributes(self):
        attributes = []
        jattribs = self.jobj.getAttributes()
        itr = jattribs.iterator()
        while itr.hasNext():
            attributes.append(str(itr.next()))
        return attributes
    
    def getDataTime(self):
        return DataTime.DataTime(self.jobj.getDataTime())
    
    def getLevel(self):
        return str(self.jobj.getLevel())
    
    def getLocationName(self):
        return self.jobj.getLocationName()
    
    def toJavaObj(self):
        return self.jobj
    

