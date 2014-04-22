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
# Used to merge two ore more python modules and returns a new module. This set of methods
# are compatible with and used by both the Jep Python Overrider solution and the Pure
# (outside of Jep, no Java dependencies) Python Overrider solution. 
#
#
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/12/13                      bkowal         Initial Creation.
#    01/20/14        2712          bkowal         Improve python class merging. Classes
#                                                 will now truly override each other
#                                                 instead of extending each other.
#
#
#

import os, imp, types, inspect
import ModuleUtil

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
        tmpmodule = ModuleUtil.loadModule(module)
        themodule = _combineMembers(tmpmodule, themodule)
    return themodule

def _buildLocalizationLevelsList(availableLevels, desiredLevel):
    if desiredLevel == None:
        return availableLevels
    else:        
        # ensure that the specified level is actually a legitimate level
        if desiredLevel not in availableLevels:
            raise LookupError('An invalid level has been specified!')
        
        levels = []
        try:
            levelIdx = availableLevels.index(desiredLevel)
            levels = availableLevels[:levelIdx + 1]
        except ValueError:
            # ignore; the exception should never be thrown, we verify that the specified level
            # is valid in the previous if statement
            pass
        
        return levels         
        
def _combineMembers(tocombine, combinationresult):    
    for attr in dir(tocombine):
        if attr.startswith('__') or attr.startswith('_') or isinstance(attr, types.BuiltinFunctionType):
            # skip
            continue
        
        # is the element a class?
        if isinstance(getattr(tocombine, attr), types.ClassType) \
        or isinstance(getattr(tocombine, attr), types.TypeType):
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
    if _compareClasses(sourceClass, targetClass):
        # nothing to merge
        return target
    
    for attr in dir(sourceClass):
         # include private attributes because this is a class? 
         # methods cannot just be merged into a class, so skip them.
         if isinstance(getattr(sourceClass, attr), types.BuiltinFunctionType) \
            or isinstance(getattr(sourceClass, attr), types.MethodType) \
            or attr.startswith('__'):
            continue
         
         # do we need to worry about nested classes?
         if isinstance(getattr(sourceClass, attr), types.ClassType) \
            or isinstance(getattr(sourceClass, attr), types.TypeType):
             target = _mergeClasses(source, target, attr)
         
         attributeName = className + '.' + attr
         target = _mergeAttributes(source, target, attributeName)
    
    # make new class "extend" the original class
    for attr in dir(targetClass):
        if attr != '__init__' \
            and isinstance(getattr(targetClass, attr), types.MethodType) \
            and not attr in dir(sourceClass):
            # complete the merge / override of methods for any method that
            # the new class does not implement
            
            # retrieve the implementation of the method (this is different from
            # retrieving the method, itself)
            exec('method = target.' + className + '.' + attr + '.im_func')
            # copy the method implementation to the other class and give it
            # the same name as the original
            classMethodDirective = _buildReplaceClassMethodDirective(className, attr)
            exec(classMethodDirective)    
         
    return _mergeAttributes(source, target, className)

def _buildReplaceClassMethodDirective(className, methodName):
    replaceDirective = 'setattr(source.' + className + ', "' + methodName + '", method)'
    
    return replaceDirective
            
def _compareClasses(clazz1, clazz2):
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