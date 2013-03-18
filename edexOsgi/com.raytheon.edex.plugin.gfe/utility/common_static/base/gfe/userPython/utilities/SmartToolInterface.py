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
#    01/17/13         1486         dgilling       Re-factor based on 
#                                                 RollbackMasterInterface.
#    
# 
#


import sys
import Exceptions
import RollbackMasterInterface


class SmartToolInterface(RollbackMasterInterface.RollbackMasterInterface):
    
    def __init__(self, scriptPath):
        super(SmartToolInterface, self).__init__(scriptPath)
        self.importModules()
        self.parmToModuleMap = {'variableElement' : set()}
        for script in self.scripts:
            self.__mapDisplayList(script)
                    
    def __mapDisplayList(self, script):
        if hasattr(sys.modules[script], "WeatherElementEdited"):
            parm = sys.modules[script].WeatherElementEdited
            if parm in ['variableElement', 'None']:
                if hasattr(sys.modules[script], "ScreenList"):
                    screen = sys.modules[script].ScreenList
                    for scr in screen:
                        if self.parmToModuleMap.has_key(scr):
                            self.parmToModuleMap[scr].add(script)
                        else:
                            self.parmToModuleMap[scr] = set([script])
                else:
                    if self.parmToModuleMap.has_key(parm):
                        self.parmToModuleMap[parm].add(script)
                    else:
                        self.parmToModuleMap[parm] = set([script])
            else:
                if self.parmToModuleMap.has_key(parm):
                    self.parmToModuleMap[parm].add(script)
                else:
                    self.parmToModuleMap[parm] = set([script])

    def getWeatherElementEdited(self, name):
        return sys.modules[name].WeatherElementEdited

    def getScreenList(self, name):
        return sys.modules[name].ScreenList

    def getVariableList(self, name):
        result = None
        if hasattr(sys.modules[name], "VariableList"):
            result = sys.modules[name].VariableList
        return result
    
    def getHideTool(self, name):
        "Determine whether a tool is hidden."
        result = getattr(sys.modules[name], "HideTool", False)
        result = bool(result)
        return result
    
    def getVariableListInputs(self, name):
        varList = self.getVariableList(name)
        return self.runMethod(name, "Tool", "getVariableListInputs", VariableList=varList)
    
    def getScripts(self, weatherElement=None, gridType=None):
        from java.util import HashSet
        scriptList = HashSet()
        if weatherElement is None and gridType is None:
            # Add all the tools
            for s in self.parmToModuleMap:
                val = self.parmToModuleMap[s]
                for v in val:                
                    scriptList.add(str(v))
        else:
            # Add the tools specific to the weather element
            if weatherElement is not None:
                if self.parmToModuleMap.has_key(weatherElement):
                    specificParms = self.parmToModuleMap[weatherElement]
                    for s in specificParms:
                        if not self.getHideTool(s):
                            scriptList.add(str(s))

            # Add the tools specific to the grid type
            if gridType is not None:
                if self.parmToModuleMap.has_key(gridType):
                    specificParms = self.parmToModuleMap[gridType]
                    for s in specificParms:
                        if not self.getHideTool(s):
                            scriptList.add(str(s))

            # Add the general-use tools
            allParms = self.parmToModuleMap['variableElement']
            for s in allParms:
                scriptList.add(str(s))
                
        return scriptList
    
    def getMethodArgNames(self, moduleName, className, methodName):
        from java.util import ArrayList        
        args = self.getMethodArgs(moduleName, className, methodName)
        argList = ArrayList()
        for a in args:
            argList.add(a)
        return argList
    
    def addModule(self, moduleName):        
        super(SmartToolInterface, self).addModule(moduleName)
        self.__mapDisplayList(moduleName)
        
    def reloadModule(self, moduleName):
        self.__removeModuleFromMap(moduleName)
        super(SmartToolInterface, self).reloadModule(moduleName)
        self.__mapDisplayList(moduleName)
        
    def removeModule(self, moduleName):
        super(SmartToolInterface, self).removeModule(moduleName)
        self.__removeModuleFromMap(moduleName)
        # in-case we removed just an override, let's
        # check to see if another script with the same name exists
        if sys.modules.has_key(moduleName):
            self.__mapDisplayList(moduleName)
    
    def __removeModuleFromMap(self, moduleName):
        for parm in self.parmToModuleMap:
            toolList = self.parmToModuleMap.get(parm)
            if moduleName in toolList:
                toolList.remove(moduleName)
                self.parmToModuleMap[parm] = toolList
    
    def runTool(self, moduleName, className, methodName, **kwargs):
        try:
             return self.runMethod(moduleName, className, methodName, **kwargs)
        except Exceptions.EditActionError, e:
            msg = e.errorType() + ": " + e.errorInfo()
            raise RuntimeError(msg)
            
        
            

    
    