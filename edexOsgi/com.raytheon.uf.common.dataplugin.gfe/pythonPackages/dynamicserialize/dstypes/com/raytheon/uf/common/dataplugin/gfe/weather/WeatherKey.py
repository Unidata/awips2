##
##

# File auto-generated against equivalent DynamicSerialize Java class


SUBKEY_SEPARATOR = '^'

class WeatherKey(object):
## FIXME: Implement WeatherSubKey and use it in this class when needed. ##

    def __init__(self, siteId="", subKeys=[]):
        self.siteId = siteId
        if type(subKeys) is str:
            self.__parseString(str(subKeys))
        else:
            self.subKeys = subKeys
            
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return SUBKEY_SEPARATOR.join(self.subKeys)
            
    def __eq__(self, other):
        if not isinstance(other, WeatherKey):
            return False
        return self.subKeys == self.subKeys
    
    def __ne__(self, other):
        return (not self.__eq__(other))
    
    def __hash__(self):
        prime = 31
        result = 1
        result = prime * result + (0 if self.subKeys is None else hash(self.subKeys))
        return result

    def getSiteId(self):
        return self.siteId

    def setSiteId(self, siteId):
        self.siteId = siteId

    def getSubKeys(self):
        return self.subKeys

    def setSubKeys(self, subKeys):
        self.subKeys = subKeys
        
    def __parseString(self, subKeys):
        self.subKeys = subKeys.split(SUBKEY_SEPARATOR)
