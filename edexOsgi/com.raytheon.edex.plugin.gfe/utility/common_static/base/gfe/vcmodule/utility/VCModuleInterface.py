#!/usr/bin/env python
#
# Globally import and sets up instances of the smart tool scripts.
# Designed to be used as a master controller for inspecting and running
# smart tools from Java.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/21/08                      njensen        Initial Creation.
#    
# 
#

import numpy
import sys

import JUtil
import MasterInterface


class VCModuleInterface(MasterInterface.MasterInterface):
    
    def __init__(self, scriptPath):
        MasterInterface.MasterInterface.__init__(self)
        self.importModules(scriptPath)
        
    def getMethodArgNames(self, moduleName, className, methodName):
        from java.util import ArrayList        
        args = self.getMethodArgs(moduleName, className, methodName)
        argList = ArrayList()
        for a in args:
            argList.add(a)
        return argList
        
    def getMethodArgs(self, moduleName, className, methodName):
        return MasterInterface.MasterInterface.getMethodArgs(self, moduleName, className, methodName)[1:]
