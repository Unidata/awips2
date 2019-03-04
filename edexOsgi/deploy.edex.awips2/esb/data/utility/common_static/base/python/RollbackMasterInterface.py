##
##

#
# Globally import and sets up instances of the scripts.
# Designed to be used as a master controller for inspecting and running
# python scripts from Java. Differs from base MasterInterface class because
# it utilizes the rollback importer.
#
# This class should remain purely python.  For Java interactions, extend this class.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/17/13                      dgilling       Initial Creation.
#    10/09/13         16614   njensen      Fixed reloadModules()
#    
# 
#

import MasterInterface

class RollbackMasterInterface(MasterInterface.MasterInterface):
    
    def __init__(self, scriptPath):
        super(RollbackMasterInterface, self).__init__()
        self._scriptPath = scriptPath
        
    def importModules(self):
        super(RollbackMasterInterface, self).importModules(self._scriptPath)
                
    def getStartupErrors(self):
        from java.util import ArrayList
        errorList = ArrayList()
        for err in self.getImportErrors():
            errorList.add(str(err))
        return errorList
    
    def addModule(self, moduleName):        
        super(RollbackMasterInterface, self).addModule(moduleName)
        self.reloadModules()
        
    def reloadModule(self, moduleName):
        super(RollbackMasterInterface, self).reloadModule(moduleName)
        self.reloadModules()
        
    def removeModule(self, moduleName):
        super(RollbackMasterInterface, self).removeModule(moduleName)
        self.reloadModules()
    
    def reloadModules(self):
        for script in self.scripts:
            super(RollbackMasterInterface, self).reloadModule(script)        
        
        