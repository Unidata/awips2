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
#    10/13/09                      njensen        Initial Creation.
#    
# 
#


import sys
import MasterInterface
import Avn

#sys.argv = ['TafToolInterface']

class TafToolInterface(MasterInterface.MasterInterface):
    
    def __init__(self, scriptPath):
        MasterInterface.MasterInterface.__init__(self)
        self.importModules(scriptPath)
            
    def getScripts(self):
        from java.util import ArrayList
        scriptList = ArrayList()
        for s in self.scripts:
            scriptList.add(s)
        return scriptList
    
    def getStartupErrors(self):
        from java.util import ArrayList
        errorList = ArrayList()
        for err in self.getImportErrors():
            errorList.add(str(err))
        return errorList
    
    def runTool(self, moduleName, bbb, fcsts):
        import JUtil        
        fcsts = JUtil.javaStringListToPylist(fcsts)
        ids = [Avn._getIds(f) for f in fcsts]
        #bbb = self.sp.bbb.get().strip().upper()
        mod = __import__(moduleName)
        fcsts = mod.updateTafs(bbb, dict(zip(ids, fcsts)))
        if fcsts:
            return '\n'.join([fcsts[i] for i in ids])
        else:
            return None

    
    