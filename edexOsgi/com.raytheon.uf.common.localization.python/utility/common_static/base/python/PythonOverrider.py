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
#    06/01/15        4433          bkowal       Specified a default thrift host and port.
#
#
#

import os.path
import sys

JEP_AVAILABLE = sys.modules.has_key('jep')

if JEP_AVAILABLE:
    from PathManager import PathManager
else:
    SETUP_FILE = '/awips2/fxa/bin/setup.env'
    THRIFT_HOST = 'localhost'
    THRIFT_PORT = '9581'    
    
    import PythonOverriderPure
    
    if os.path.isfile(SETUP_FILE):
        import subprocess
        
        format = 'source {0}; echo ${1}'
        test_host = subprocess.check_output(format.format(SETUP_FILE, "DEFAULT_HOST"), shell=True).strip()
        if test_host:
            THRIFT_HOST = test_host
        test_port = subprocess.check_output(format.format(SETUP_FILE, "DEFAULT_PORT"), shell=True).strip()
        if test_port:
            THRIFT_PORT = test_port

import PythonOverriderCore

def importModule(name, loctype='COMMON_STATIC', level=None, localizedSite=None, 
                localizationUser=None):
    """
    Takes a name (filename and localization path) and the localization type and finds the 
    file and overrides it, and returns the module
    
    Args:
            name : the name and path of the file in localization
            loctype : a string representation of the localization type
            level : a string representation of the localization level (BASE, SITE, etc.)
            localizedSite: the site that localization information should be
                retrieved for (if applicable)
            localizationUser: the user that localization information should
                be retrieved for (if applicable)
    
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