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
#    01/17/13         1486         dgilling       Re-factor based on 
#                                                 RollbackMasterInterface.
#    07/27/15         4263         dgilling       Support refactored Java
#                                                 ProcedureControllers.
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


class ProcedureInterface(RollbackMasterInterface.RollbackMasterInterface):
    
    def __init__(self, scriptPath):
        super(ProcedureInterface, self).__init__(scriptPath)
        
        logging.basicConfig(level=logging.INFO)
        self.log = logging.getLogger("ProcedureInterface")
        self.log.addHandler(UFStatusHandler.UFStatusHandler(PLUGIN_NAME, CATEGORY))
        
        self.importModules()
                    
    def __getProcedureInfo(self, script, dataMgr):
        menus = self.getMenuName(script)
        argNames = self.getMethodArgNames(script, "Procedure", "execute")
        varDict = self.getVariableListInputs(script)
        return menus, argNames, varDict
    
    def getScripts(self, dataMgr):
        from java.util import HashMap
        from com.raytheon.viz.gfe.procedures import ProcedureMetadata
        
        scriptList = HashMap()
        for script in self.scripts:
            try:
                (menus, argNames, varDict) = self.__getProcedureInfo(script, dataMgr)
                name = str(script)
                if not menus:
                    menus = [] 
                menus = JUtil.pyValToJavaObj(menus)
                argNames = JUtil.pyValToJavaObj(argNames)
                metadata = ProcedureMetadata(name, menus, argNames, varDict)
                scriptList.put(name, metadata)
            except:
                self.log.exception("Unable to load metadata for procedure " + script)
                
        return scriptList
    
    def addModule(self, moduleName):
        super(ProcedureInterface, self).addModule(moduleName)
        
    def removeModule(self, moduleName):
        super(ProcedureInterface, self).removeModule(moduleName)
    
    def getMethodArgNames(self, moduleName, className, methodName):  
        args = self.getMethodArgs(moduleName, className, methodName)
        return JUtil.pyValToJavaObj(args)
    
    def runProcedure(self, moduleName, className, methodName, **kwargs):
        try:
             return self.runMethod(moduleName, className, methodName, **kwargs)
        except Exceptions.EditActionError, e:
            if "Cancel" == e.errorType() and "Cancel" == e.errorInfo():
                return None
            msg = moduleName + ":" + e.errorType() + ": " + e.errorInfo()
            raise RuntimeError(msg)

    def getMenuName(self, name):
        return getattr(sys.modules[name], "MenuItems", [])

    def getVariableList(self, name):
        return getattr(sys.modules[name], "VariableList", [])
    
    def getVariableListInputs(self, name):
        varList = self.getVariableList(name)
        return ProcessVariableList.buildWidgetList(varList)
    
    def reloadModule(self, moduleName):
        super(ProcedureInterface, self).reloadModule(moduleName)
