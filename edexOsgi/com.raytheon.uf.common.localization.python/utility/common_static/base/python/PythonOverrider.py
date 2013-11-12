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
#
#
#

import os
import imp
import types
from PathManager import PathManager
import LocalizationUtil

def importModule(name, loctype='COMMON_STATIC', level=None):
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
    pathManager = PathManager()
    tieredFiles = pathManager.getTieredLocalizationFile(loctype, name)
    availableLevels = pathManager.getAvailableLevels()
    if level == None:
        levels = availableLevels
    else:
        # ensure that the specified level is actually a legitimate level
        if level not in availableLevels:
            raise LookupError('An invalid level has been specified!')
        
        levels = []
        try:
            levelIdx = availableLevels.index(level)
            levels = availableLevels[:levelIdx + 1]
        except ValueError:
            # ignore; the exception should never be thrown, we verify that the specified level
            # is valid in the previous if statement
            pass
        
    lfiles = []
    for _level in levels :
        if _level in tieredFiles:
            lfiles.append(tieredFiles[_level].getPath())
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
        themodule = _combineMembers(tmpmodule, themodule)
    return themodule

def _combineMembers(tocombine, combinationresult):
    for attr in dir(tocombine):
        if attr.startswith('__') or attr.startswith('_') or isType(attr, types.BuiltinFunctionType):
            # skip
            continue
        
        # is the element a class?
        if isType(getattr(tocombine, attr), types.ClassType):
            combinationresult = _mergeClasses(tocombine, combinationresult, attr)
        else:
            # absolute override
            combinationresult = _mergeAttributes(tocombine, combinationresult, attr)
        
    return combinationresult

def _mergeClasses(source, target, className):
    sourceClass = getattr(source, className)
    targetClass = getattr(target, className, None)
    
    if (targetClass == None):
        return _mergeAttributes(source, target, className)
    
    legacyMode = (hasattr(sourceClass, '__class__') == False)
    
    # verify that both classes are either legacy for current style.
    if ((hasattr(targetClass, '__class__') == False) != legacyMode):
        raise Exception("A legacy python class cannot be merged with a non-legacy python class!")
    
    # ensure that the classes are not exactly the same (breaks the legacy merge).
    if compareClasses(sourceClass, targetClass):
        # nothing to merge
        return target
    
    for attr in dir(sourceClass):
         # include private attributes because this is a class? 
         # methods cannot just be merged into a class, so skip them.
         if isType(attr, types.BuiltinFunctionType) or isType(attr, types.MethodType) or \
         attr.startswith('__') or attr.startswith('_'):
             continue
         
         # do we need to worry about nested classes?
         if isType(getattr(sourceClass, attr), types.ClassType):
             target = _mergeClasses(source, target, attr)
         
         attributeName = className + '.' + attr
         target = _mergeAttributes(source, target, attributeName)
         
    # complete the merge / override of methods.
    exec(_buildMergeDirective(className, legacyMode))
    return _mergeAttributes(source, target, className)

def _buildMergeDirective(className, legacyMode):
    if (legacyMode):
        return 'source.' + className + '.__bases__ = (target.' + className + ',)'
    else:
        return 'source.' + className + ' = type("' + className + \
            '", (target.' + className + ',), dict(source.' + className + '.__dict__))'

def isType(object, type):
    return type(object) == type
            
def compareClasses(clazz1, clazz2):
    clazz1Attr = dir(clazz1)
    clazz2Attr = dir(clazz2)

    if (len(clazz1Attr) != len(clazz2Attr)):
        return False

    i = 0
    while i < len(clazz1Attr):
        # compare the names
        if (clazz1Attr[i] != clazz2Attr[i]):
            return False

        # compare the attributes directly
        attr1 = getattr(clazz1, clazz1Attr[i])
        attr2 = getattr(clazz2, clazz2Attr[i])
        if (attr1 != attr2):
            return False

        i += 1

    return True

def _mergeAttributes(source, target, attributeName):
    
    mergeDirective = 'target.' + attributeName + ' = source.' + attributeName
    exec(mergeDirective)
    
    return target