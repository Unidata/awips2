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
#    07/23/15         4263         dgilling       Support refactored Java
#                                                 SmartToolControllers.
#    04/13/16         5568         dgilling       More lenient handling of 
#                                                 ScreenList.
#    
# 
#


import logging
import sys
import Exceptions

import JUtil
import ProcessVariableList
import RollbackMasterInterface
import UFStatusHandler


PLUGIN_NAME = 'com.raytheon.viz.gfe'
CATEGORY = 'GFE'


class SmartToolInterface(RollbackMasterInterface.RollbackMasterInterface):
    
    def __init__(self, scriptPath):
        super(SmartToolInterface, self).__init__(scriptPath)
        
        logging.basicConfig(level=logging.INFO)
        self.log = logging.getLogger("SmartToolInterface")
        self.log.addHandler(UFStatusHandler.UFStatusHandler(PLUGIN_NAME, CATEGORY))
        
        self.importModules()
            
    def __getToolInfo(self, script, dataMgr):
        elementToEdit = self.getWeatherElementEdited(script)
        screenList = self.getScreenList(script)
        hideTool = self.getHideTool(script)
        docString = self.getMethodInfo(script, "Tool", "execute")
        varDict = self.getVariableListInputs(script)
        return elementToEdit, screenList, hideTool, docString, varDict
                    
    def getWeatherElementEdited(self, name):
        return getattr(sys.modules[name], "WeatherElementEdited", "None")

    def getScreenList(self, name):
        screenList = getattr(sys.modules[name], "ScreenList", None)
        if screenList is not None:
            try:
                iter(screenList)
            except TypeError:
                screenList = [str(screenList)]
            else:
                if isinstance(screenList, basestring):
                    screenList = [str(screenList)]
                else:
                    screenList = [str(i) for i in screenList]
        return screenList

    def getVariableList(self, name):
        return getattr(sys.modules[name], "VariableList", [])
    
    def getHideTool(self, name):
        "Determine whether a tool is hidden."
        result = getattr(sys.modules[name], "HideTool", False)
        result = bool(result)
        return result
    
    def getVariableListInputs(self, name):
        varList = self.getVariableList(name)
        return ProcessVariableList.buildWidgetList(varList)
    
    def getScripts(self, dataMgr):
        from java.util import HashMap
        from com.raytheon.viz.gfe.smarttool.script import SmartToolMetadata
        
        scriptList = HashMap()
        
        for script in self.scripts:
            try:
                (element, screenList, hideTool, docString, varDict) = self.__getToolInfo(script, dataMgr)
                name = str(script)
                if screenList is not None:
                    screenList = JUtil.pyValToJavaObj(screenList)            
                hideTool = bool(hideTool)
                docString = str(docString)
                metadata = SmartToolMetadata(name, element, screenList, hideTool, docString, varDict)
                scriptList.put(name, metadata)
            except:
                self.log.exception("Unable to load metadata for smart tool " + script)
                
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
        
    def reloadModule(self, moduleName):
        super(SmartToolInterface, self).reloadModule(moduleName)
        
    def removeModule(self, moduleName):
        super(SmartToolInterface, self).removeModule(moduleName)
    
    def runTool(self, moduleName, className, methodName, **kwargs):
        try:
             return self.runMethod(moduleName, className, methodName, **kwargs)
        except Exceptions.EditActionError, e:
            msg = e.errorType() + ": " + e.errorInfo()
            raise RuntimeError(msg)
            
        
            

    
    