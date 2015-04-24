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
#    02/09/15        4120          reblum         Fixed bugs in the class merging.
#
#
#

import os, imp, types, inspect
import ModuleUtil

def _internalOverride(files):
    """
    Combines the different localization levels of the same class/module into a single
    class/module with the lower localization level methods taking precedence over the 
    higher localization level methods.
    
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

    # Get the set of unimplemented abstractmethods from the sourceClass
    abstractMethods = set()
    if sourceClass.__dict__.has_key('__abstractmethods__'):
        for method in sourceClass.__abstractmethods__:
            abstractMethods.add(method)

    # make new class "extend" the original class
    for attr in dir(targetClass):
        if isinstance(getattr(targetClass, attr), types.MethodType) \
            and not attr in sourceClass.__dict__:
            # complete the merge / override of methods for any method that
            # the new class does not implement
            
            # retrieve the implementation of the method (this is different from
            # retrieving the method, itself)
            exec('method = target.' + className + '.' + attr + '.im_func')
            # copy the method implementation to the other class and give it
            # the same name as the original
            classMethodDirective = _buildReplaceClassMethodDirective(className, attr)
            exec(classMethodDirective)
            # If the method we just merged was an abstractmethod remove it from 
            # abstractMethods since it now has been implemented.
            if attr in abstractMethods:
                abstractMethods.remove(attr)

    # Update __abstractmethods__ so that it correctly reflects if there are any 
    # unimplemented abstactmethods for the merged class.
    if sourceClass.__dict__.has_key('__abstractmethods__'):
        directive = 'sourceClass.__abstractmethods__ =  frozenset(abstractMethods)'
        exec(directive)
         
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
        attr1 = None
        attr2 = None
        # see http://bugs.python.org/issue10006
        if clazz1Attr[i] != '__abstractmethods__':
            attr1 = getattr(clazz1, clazz1Attr[i])
        if clazz2Attr[i] != '__abstractmethods__':
            attr2 = getattr(clazz2, clazz2Attr[i])
        if (attr1 != attr2):
            return False

        i += 1

    return True

def _mergeAttributes(source, target, attributeName):
    
    mergeDirective = 'target.' + attributeName + ' = source.' + attributeName
    exec(mergeDirective)
    
    return target