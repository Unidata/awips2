##
##

class GetVtecAttributeRequest(object):

    def __init__(self):
        self.siteId = None
        self.attribute = None
        self.defaultValue = None
        
    def getSiteId(self):
        return self.siteId
    
    def setSiteId(self, site):
        self.siteId = site

    def getAttribute(self):
        return self.attribute
    
    def setAttribute(self, attribute):
        self.attribute = attribute
        
    def getDefaultValue(self):
        return self.defaultValue
    
    def setDefaultValue(self, default):
        self.defaultValue = default
