##
##
#
# Provides support for reading/writing/deleting files using localization
# 
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/10/2014                    randerso       Initial Creation.
#    12/08/2014      #4953         randerso       Made for generic allowing specification of LocalizationLevel
#                                                 Re-exported Java enums for easier use.
#    Apr 25, 2015     4952         njensen        Updated for new JEP API
##

##
# This is a base file that is not intended to be overridden.
##



from LockingFile import File
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext
LocalizationType = LocalizationContext.LocalizationType
LocalizationLevel = LocalizationContext.LocalizationLevel

EDEX_STATIC = LocalizationType.EDEX_STATIC
CAVE_STATIC = LocalizationType.CAVE_STATIC
CAVE_CONFIG = LocalizationType.CAVE_CONFIG
COMMON_STATIC = LocalizationType.COMMON_STATIC

BASE = LocalizationLevel.BASE
CONFIGURED = LocalizationLevel.CONFIGURED
SITE = LocalizationLevel.SITE
USER = LocalizationLevel.USER

def getLocalizationFile(loctype, loclevel, contextname, filename):
    pathManager = PathManagerFactory.getPathManager()
    context = pathManager.getContext(loctype, loclevel)
    context.setContextName(contextname)
    localizationFile = pathManager.getLocalizationFile(context, filename)
    return localizationFile

def readFile(loctype, loclevel, contextname, filename):
    localizationFile = getLocalizationFile(loctype, loclevel, contextname, filename)

    with File(localizationFile.getFile(), filename, 'r') as pythonFile:
        fileContents = pythonFile.read()
    
    return fileContents
    
def writeFile(loctype, loclevel, contextname, filename, contents):
    localizationFile = getLocalizationFile(loctype, loclevel, contextname, filename)
    
    with File(localizationFile.getFile(), filename, 'w') as pythonFile:
        pythonFile.write(contents)
    
    localizationFile.save()

def deleteFile(loctype, loclevel, contextname, filename):
    localizationFile = getLocalizationFile(loctype, loclevel, contextname, filename)
    localizationFile.delete()
