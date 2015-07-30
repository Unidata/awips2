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
REGION = LocalizationLevel.REGION
CONFIGURED = LocalizationLevel.CONFIGURED
SITE = LocalizationLevel.SITE
WORKSTATION = LocalizationLevel.WORKSTATION
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