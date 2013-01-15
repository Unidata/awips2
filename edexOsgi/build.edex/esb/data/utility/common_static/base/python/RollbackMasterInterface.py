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
#    
# 
#

import os

import MasterInterface
import RollBackImporter

rollbackImporter = RollBackImporter.RollBackImporter()


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
            super(RollbackMasterInterface, self).removeModule(script)       
        rollbackImporter.rollback()
        self.importModules()
        
        