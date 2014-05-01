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

    