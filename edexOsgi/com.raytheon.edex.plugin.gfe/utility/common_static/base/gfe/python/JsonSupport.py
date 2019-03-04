##
##
#
# Provides support for loading/saving python objects from/to localization files
# using JSON serialization
# 
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/10/2014                    randerso       Initial Creation.
#    12/08/2014       #4953        randerso       Updated for changes to LocalizationSupport
##

##
# This is a base file that is not intended to be overridden.
##



import json
import LocalizationSupport
import JUtil

def loadFromJson(localizationType, siteID, fileName):
    jsonString = LocalizationSupport.readFile(localizationType, LocalizationSupport.SITE, siteID, fileName)
    object = json.loads(jsonString)
    return object
    
def saveToJson(localizationType, siteID, fileName, object):
    jsonString = json.dumps(object, sort_keys=True, 
                            indent=4, separators=(',', ': '))
    LocalizationSupport.writeFile(localizationType, LocalizationSupport.SITE, siteID, fileName, jsonString)

def loadJsonFromJava(localizationType, siteID, fileName):
    object = loadFromJson(localizationType, siteID, fileName)
    javaObject = JUtil.pyValToJavaObj(object)
    return javaObject

def saveJsonFromJava(localizationType, siteID, fileName, javaObject):
    object = JUtil.javaObjToPyVal(javaObject)
    saveToJson(localizationType, siteID, fileName, object)
    