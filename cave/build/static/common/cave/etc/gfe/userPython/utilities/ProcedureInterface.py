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
# Globally import and sets up instances of the procedure scripts.
# Designed to be used as a master controller for inspecting and running
# procedures from Java.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/05/08                      njensen        Initial Creation.
#    
# 
#


import sys
import Exceptions
import MasterInterface
from com.raytheon.uf.common.localization import PathManagerFactory

class ProcedureInterface(MasterInterface.MasterInterface):
    
    def __init__(self, scriptPath):
        MasterInterface.MasterInterface.__init__(self)
        self.importModules(scriptPath)
        self.pathMgr = PathManagerFactory.getPathManager()
        
        self.menuToProcMap = {}
        for script in self.scripts:
            self.__mapMenuList(script)
                    
    def __mapMenuList(self, script):
        if hasattr(sys.modules[script], "MenuItems"):
            menus = sys.modules[script].MenuItems
            if menus is not None:
                for item in menus:
                    if item is not None and len(item) > 0:
                        if self.menuToProcMap.has_key(item):
                            self.menuToProcMap[item].add(script)
                        else:
                            self.menuToProcMap[item] = set([script])
    
    def getScripts(self, menu):
        from java.util import HashSet
        scriptList = HashSet()
        if self.menuToProcMap.has_key(menu):
            menuItems = self.menuToProcMap[menu]
            for item in menuItems:
                scriptList.add(str(item))             
        return scriptList
    
    def addModule(self, moduleName):
        MasterInterface.MasterInterface.addModule(self, moduleName)
        self.__mapMenuList(moduleName)
        
    def removeModule(self, moduleName):
        MasterInterface.MasterInterface.removeModule(self, moduleName)
        for key in self.menuToProcMap:
            procList = self.menuToProcMap.get(key)
            if moduleName in procList:
                procList.remove(moduleName)
                self.menuToProcMap[key] = procList
        if self.pathMgr.getStaticLocalizationFile("gfe/userPython/procedures/" + moduleName + ".py") is not None:
            self.addModule(moduleName)
            
    
    def getStartupErrors(self):
        from java.util import ArrayList
        errorList = ArrayList()
        for err in self.getImportErrors():
            errorList.add(str(err))
        return errorList
    
    def getMethodArgNames(self, moduleName, className, methodName):
        from java.util import ArrayList        
        args = self.getMethodArgs(moduleName, className, methodName)
        argList = ArrayList()
        for a in args:
            argList.add(a)
        return argList
    
    def runProcedure(self, moduleName, className, methodName, **kwargs):
        try:
             return self.runMethod(moduleName, className, methodName, **kwargs)
        except Exceptions.EditActionError, e:
            if "Cancel" == e.errorType() and "Cancel" == e.errorInfo():
                return None
            msg = moduleName + ":" + e.errorType() + ": " + e.errorInfo()
            raise RuntimeError(msg)
        
    def getVariableList(self, name):
        result = None
        if hasattr(sys.modules[name], "VariableList"):
            result = sys.modules[name].VariableList
        return result
    
    def getVariableListInputs(self, name):
        varList = self.getVariableList(name)
        return self.runMethod(name, "Procedure", "getVariableListInputs", VariableList=varList)
    
    def reloadModule(self, moduleName):
        if sys.modules.has_key(moduleName):
            mod = sys.modules[moduleName]
            if hasattr(mod, "MenuItems"):
                menus = mod.MenuItems
                if menus is not None:
                    for item in menus:
                        try:
                            self.menuToProcMap[item].remove(moduleName)
                        except ValueError:
                            # wasn't in list
                            pass
                        
        MasterInterface.MasterInterface.reloadModule(self, moduleName)
        self.__mapMenuList(moduleName)
            
            

    
    