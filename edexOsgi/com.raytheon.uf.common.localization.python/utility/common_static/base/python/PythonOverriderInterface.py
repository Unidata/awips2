# #
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
# #

#
# A proxy to Python Overrider that utilizes the capabilities of the
# Python RollbackMasterInterface. The objective of this class is
# to prevent the MasterInterface from completing imports because the
# MasterInterface will overwrite modules instead of merging them.
#
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/14/2014      #2766         bkowal         Initial Creation.
#
#
#

import os, sys, string, traceback
import RollbackMasterInterface
import PythonOverrider

class PythonOverriderInterface(RollbackMasterInterface.RollbackMasterInterface):
    def __init__(self, scriptPath, localizationPath=None):
        super(PythonOverriderInterface, self).__init__(scriptPath)
        self._localizationPath = localizationPath
        
    def importModules(self):
        modulesToImport = []
        
        for s in self._scriptPath.split(os.path.pathsep):
            if os.path.exists(s):
                scriptfiles = os.listdir(s)
        
                for filename in scriptfiles:
                    split = string.split(filename, ".")
                    if len(split) == 2 and len(split[0]) > 0 and split[1] == "py" and not filename.endswith("Interface.py"):
                        if not split[0] in modulesToImport:
                            modulesToImport.append(split[0])        
        
        for moduleName in modulesToImport:
            self._importModule(moduleName)
        
    def addModule(self, moduleName):
        if not moduleName in self.scripts:
            self.scripts.append(moduleName)
        self.reloadModules()
    
    def reloadModules(self):
        for script in self.scripts:
            # first remove all references to the existing module
            if sys.modules.has_key(script):
                self.clearModuleAttributes(script)
                sys.modules.pop(script)
            
            # now use PythonOverrider to re-import the module
            self._importModule(script)
            
    def _importModule(self, moduleName):
        scriptName = moduleName + '.py'
        if self._localizationPath:
            scriptName = os.path.join(self._localizationPath, scriptName)
        try:
            importedModule = PythonOverrider.importModule(scriptName)
        except Exception, e:
            msg = moduleName + "\n" + traceback.format_exc()
            self.addImportError(msg)
            return
            
        if not moduleName in self.scripts:
            self.scripts.append(moduleName)                    