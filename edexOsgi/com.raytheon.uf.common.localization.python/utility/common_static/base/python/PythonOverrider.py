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
#
#
#

import os
import imp
import inspect
from PathManager import PathManager
import LocalizationUtil

def override(name, loctype):
    """
    Takes a name (filename and localization path) and the localization type and finds the 
    file and overrides it, and returns the module
    
    Args:
            name : the name and path of the file in localization
            loctype : a string representation of the localization type
    
    Returns:
            a module that has all the correct methods after being overridden
    """
    pathManager = PathManager()
    tieredFiles = pathManager.getTieredLocalizationFile(loctype, name)
    levels = pathManager.getAvailableLevels()
    lfiles = list()
    for level in levels :
        if tieredFiles.has_key(level) :
            lfiles.append(tieredFiles[level].getPath())
    themodule = _internalOverride(lfiles)
    return themodule

def _internalOverride(files):
    """
    Takes the files and overrides them
    
    Args:
            files : the files that are to be overridden
    
    Returns:
            a new module that contains all the necessary elements
    """
    themodule = imp.new_module('tmpmodule')
    # modules = list of all the modules
    for module in files :
        # load each module, temporarily
        tmpmodule = LocalizationUtil.loadModule(module)
        the_module = _combineMembers(tmpmodule, themodule)
    return themodule

def _combineMembers(tocombine, finalmodule):
    """
    Loops over the necessary parts of each module and decides how to combine them
    
    Args:
            tocombine : the module to combine in
            finalmodule : the module that is being combined into
    
    Returns:
            a new python module that was created above
    """
    # get the functions
    members = inspect.getmembers(tocombine, inspect.isfunction)
    for member in members :
        finalmodule.__setattr__(member[0], member[1])

    # get the classes
    classes = inspect.getmembers(tocombine, inspect.isclass)
    for clazz in classes:
        finalmodule.__setattr__(clazz[0], clazz[1])

    for attr in dir(tocombine):
        if attr.startswith('__') == False or attr.startswith('_') == False:
            if hasattr(finalmodule, attr):
                if isinstance(attr, dict):
                    # simply update dicts with the new keys
                    getattr(finalmodule, attr).update(getattr(tocombine, attr))
                if isinstance(attr, list):
                    listattr = getattr(tocombine, attr)
                    for i in listattr:
                        # override each element in the list if it exists
                        getattr(finalmodule, attr)[i] = listattr[i]
                else :
                    finalmodule.__setattr__(attr, getattr(tocombine, attr))
            else :
                if inspect.ismethod(attr) == False and inspect.isbuiltin(attr) == False and inspect.isclass(attr) == False:
                    finalmodule.__setattr__(attr, getattr(tocombine, attr))
    return finalmodule
