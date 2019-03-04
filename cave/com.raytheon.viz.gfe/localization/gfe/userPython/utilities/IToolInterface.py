##
##

#
# Globally import and sets up instances of the itool scripts.
# Designed to be used as a master controller for inspecting and running
# itools from Java.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/21/09                      njensen        Initial Creation.
#    
# 
#

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import sys
import MasterInterface
import Exceptions

class IToolInterface(MasterInterface.MasterInterface):
    
    def __init__(self, scriptPath):
        MasterInterface.MasterInterface.__init__(self)
        self.importModules(scriptPath)
            
                                        
    def getScripts(self, menu):
        from java.util import HashSet
        scriptList = HashSet()
        for script in self.scripts:
            scriptList.add(str(script))                    
        return scriptList            
    
    def getStartupErrors(self):
        from java.util import ArrayList
        errorList = ArrayList()
        for err in self.getImportErrors():
            errorList.add(str(err))
        return errorList
    
    def runITool(self, moduleName, className, methodName, **kwargs):
        try:
             print kwargs
             return self.runMethod(moduleName, className, methodName, **kwargs)
        except Exceptions.EditActionError, e:
            msg = e.errorType() + ": " + e.errorInfo()
            raise RuntimeError(msg)
        
    def getVariableList(self, name):
        result = None
        if hasattr(sys.modules[name], "VariableList"):
            result = sys.modules[name].VariableList
        return result
    
    def getVariableListInputs(self, name):
        varList = self.getVariableList(name)
        return self.runMethod(name, "ITool", "getVariableListInputs", VariableList=varList)            

    