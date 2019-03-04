##
##

#
# Provides a python compatible wrapper to WxDefinition
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/10/08                      njensen       Initial Creation.
#    
# 
#

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

class WxDefinition:
    def __init__(self, javaDef):
        self.__def = javaDef
    
    def javaDefinition(self):
        return self.__def

    def typeDesc(self, wxType):
        return self.__def.typeDesc(wxType)
    
    def intensityDesc(self, wxType, inten):
        return self.__def.intensityDesc(wxType, inten)
    
    def attributeDesc(self, wxType, attr):
        return self.__def.attributeDesc(wxType, attr)
    
    def coverageDesc(self, wxType, cov):
        return self.__def.coverageDesc(wxType, cov)
    
    def visibilityDesc(self, wxType, vis):
        return self.__def.visibilityDesc(wxType, vis)
    
    