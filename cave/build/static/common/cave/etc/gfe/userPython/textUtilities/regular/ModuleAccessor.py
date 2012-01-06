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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ModuleAccessor.py
#  Access to the internals of Modules
#
# Author: hansen
# ----------------------------------------------------------------------------
########################################################################

import types, sys
import traceback

class ModuleAccessor:
    # Used to access objects within Modules
    def __init__(self, errorCB=None):
        # Used for error messages
        self.__errorCB = errorCB

    def module(self, moduleName, showError=1):
        # Return the module with the given name
        try:
            if sys.modules.has_key(moduleName):
                #del sys.modules[moduleName] # this is bad for the automated tests code replacement
                return sys.modules[moduleName]
            module = __import__(moduleName)
        except:
            if showError and self.__errorCB is not None:
                self.__errorCB("Problem finding or importing module: "
                               + moduleName, tracebackFlag=1)
            return None
        return module

    def variables(self, moduleName, variableList, showError=1):
        # Return the global variables in the given module
        module = self.module(moduleName, showError)
        if module is None:
            return None
        variables = []
        for variableName in variableList:
            if variableName in module.__dict__.keys():
                variables.append(module.__dict__[variableName])
            else:
                variables.append(None)
        return tuple(variables)

    def variable(self, moduleName, variableName, showError=1):
        # Return the global variable in the given module
        module = self.module(moduleName, showError)
        if module is None:
            return None
        if variableName in module.__dict__.keys():
            return module.__dict__[variableName]
        else:
           return None

    def classDefinition(self, moduleName, className):
        # Returns the class in the given module
        if className is None:
            return None, None
        module = self.module(moduleName, 1)
        if module is None:
            return None, None
        # Look for Class
        classDefinition = self.getClassDefinition(module, className)
        return module, classDefinition

    def getClassDefinition(self, module, className):
        # Check for the given class in the module
        if className in module.__dict__.keys() and \
           type(module.__dict__[className]) is types.ClassType:
            return module.__dict__[className]
        else:
            return None

    def getFunctions(self, moduleName, functionNames, className=None,
                     classArgs=None, classOnly=0):
        # Returns a dictionary containing the executable functions
        # Looks first for functions in a class
        # If not found, looks for functions in the module itself
        # These functions are definitions, not executable instances
        module, classDefinition = self.classDefinition(moduleName, className)
        if module is None:
            return None, None, None

        classInstance = None
        if classDefinition is not None:
            # Create the callable class instance and set up the
            # functions
            classInstance = classDefinition(classArgs)
            functionDict = self.getClassFunctions(
                classInstance, functionNames)
        elif not classOnly == 1:
            # Look for a Function with same name as module
            functionDict = self.getModuleFunctions(
                module, functionNames)
        else:
            return None, None, None

        return module, classInstance, functionDict

    def getClassFunctions(self,  classInstance,  functionNames):
        # Returns a dictionary containing the functions specified
        # in the given classInstance
        functionDict = {}
        for functionName in functionNames:
            functionDict[functionName] = getattr(classInstance,
                                                 functionName, None)
        return functionDict

    def getModuleFunctions(self, module, functionNames):
        # Returns a dictionary containing the functions specified
        # for the given module
        functionDict = {}
        for functionName in functionNames:
            if functionName in module.__dict__.keys():
                result = module.__dict__[functionName]
            else:
                result = None
            functionDict[functionName] = result
        return functionDict

    def callMethod(self, method, argCallback, classInstance=None):
        # Get arguments and call the method
        if method is None:
            return None
        elif hasattr(method, 'im_func'):   # It is a user defined method
            co = method.im_func.func_code
        elif hasattr(method, 'func_code'): # It is a user defined function
            co = method.func_code
        else:                              # Don't know what it is
            return None

        # Set up variables and values for arguments in args
        argValueList = argCallback(co.co_varnames[:co.co_argcount], [])
        if type(argValueList) is not types.ListType:
            error = argValueList
            return error

        # Format the arguments and call the method
        return method(*argValueList)
