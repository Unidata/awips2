# #
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
# #

#
# Overrides python modules with each other and returns a new module
#
#
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash        Initial Creation.
#    11/04/13        2086          bkowal       Updated to merge classes - both legacy and non-legacy.
#                                               Minimum to Maximum level of retrieval can now be specified.
#    11/12/13        2540          bkowal       Relocated common methods to PythonOverriderCore.py.
#    02/13/14        2712          bkowal       The main PythonOverrider module can now determine whether
#                                               it is running in Jep or not and make the necessary
#                                               adjustments.
#
#
#

import subprocess

THRIFT_HOST = subprocess.check_output(
                    'source /awips2/fxa/bin/setup.env; echo $DEFAULT_HOST', 
                    shell=True).strip()
THRIFT_PORT = subprocess.check_output(
                    'source /awips2/fxa/bin/setup.env; echo $DEFAULT_PORT', 
                    shell=True).strip()

import PythonOverriderCore

JEP_AVAILABLE = True
try:
    from PathManager import PathManager
except ImportError:
    import PythonOverriderPure
    JEP_AVAILABLE = False

def importModule(name, loctype='COMMON_STATIC', level=None, localizationHost=None, 
                localizationPort=None, localizedSite=None, localizationUser=None):
    """
    Takes a name (filename and localization path) and the localization type and finds the 
    file and overrides it, and returns the module
    
    Args:
            name : the name and path of the file in localization
            loctype : a string representation of the localization type
            level : a string representation of the localization level (BASE, SITE, etc.)
    
    Returns:
            a module that has all the correct methods after being overridden
    """
    if not JEP_AVAILABLE:
        if localizationHost is None:
            localizationHost = THRIFT_HOST
        
        if localizationPort is None:
            localizationPort = THRIFT_PORT
        
        return PythonOverriderPure.importModule(name, localizationHost, localizationPort,
                localizedSite, localizationUser, loctype, level)
    
    pathManager = PathManager()
    tieredFiles = pathManager.getTieredLocalizationFile(loctype, name)
    availableLevels = pathManager.getAvailableLevels()
    levels = PythonOverriderCore._buildLocalizationLevelsList(availableLevels, level)
        
    lfiles = []
    for _level in levels :
        if _level in tieredFiles:
            lfiles.append(tieredFiles[_level].getPath())
    themodule = PythonOverriderCore._internalOverride(lfiles)
    return themodule