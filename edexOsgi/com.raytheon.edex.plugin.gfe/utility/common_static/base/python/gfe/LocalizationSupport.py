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
#    11/10/14                      randerso       Initial Creation.
##
from LockingFile import File
from com.raytheon.uf.common.localization import PathManagerFactory

def __getLocalizationFile(loctype, site, filename):
    pathManager = PathManagerFactory.getPathManager()
    context = pathManager.getContextForSite(loctype, site)
    localizationFile = pathManager.getLocalizationFile(context, filename)
    return localizationFile

def readFile(loctype, site, filename):
    localizationFile = __getLocalizationFile(loctype, site, filename)

    with File(localizationFile.getFile(), filename, 'r') as pythonFile:
        fileContents = pythonFile.read()
    
    return fileContents
    
def writeFile(loctype, site, filename, contents):
    localizationFile = __getLocalizationFile(loctype, site, filename)
    
    with File(localizationFile.getFile(), filename, 'w') as pythonFile:
        pythonFile.write(contents)
    
    localizationFile.save()

def deleteFile(loctype, site, filename):
    localizationFile = __getLocalizationFile(loctype, site, filename)
    localizationFile.delete()