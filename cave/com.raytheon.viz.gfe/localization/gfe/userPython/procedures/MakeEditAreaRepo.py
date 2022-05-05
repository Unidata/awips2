# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# ----------------------------------------------------------------------------
##
#
# MakeEditAreaRepo.py
#
# This procedure uses EditAreaUtilities to make an edit area repository for 
# saving and retrieving edit areas outside of CAVE.
#
# Author: lefebvre
#
# SOFTWARE HISTORY
#  Date        Ticket#    Engineer    Description
#  ----------- ---------- ----------- --------------------------
#  11/21/2016   -         tlefebvre   Added siteID to Edit area repo path so
#                                     domains will stay separated.
#  12/02/2016   -         tlefebvre   Final clean-up
#  12/20/2017  DCS17686   tlefebvre   Initial baseline version.
#  05/22/2018  DR20724    tlefebvre   Remove from menus by default since most 
#                                     sites will not use these tools
#
##
# ----------------------------------------------------------------------------
MenuItems = []

VariableList = []


import time
import SmartScript
import EditAreaUtilities
import os, stat


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
    # Creates the directory tree with appropriate permissions
    def makeFullDirPath(self, dirPath, permissions):
        
        if not os.path.exists(dirPath):
            os.makedirs(dirPath, permissions)
        return

    def execute(self):
        
        t1 = time.time()
        
        siteID = self.getSiteID()
        
        path = ""
        
        # Define the path for the repository here

#        For High Seas tools
#        path = "/data/local/HighSeas/" + siteID + "/EditAreas/"

        if path == "":
            self.statusBarMsg("You must define a path in this procedure before creating a repository", "U")
            return
    
        self._eaUtils = EditAreaUtilities.EditAreaUtilities(path)
        
        permissions = stat.S_IRWXU + stat.S_IRWXG + stat.S_IROTH  # 775 permissions on dirs and files
        
        self.makeFullDirPath(path, permissions)
        
        eaList = self.editAreaList()
        for ea in eaList:
            
            try:
                gfeEA = self.encodeEditArea(ea)
            except:
                print("error getting", ea, "from GFE.")
                continue
            
            self._eaUtils.saveEditArea(ea, gfeEA)
            
            print("Saved edit area", path + ea)
            
        print("Finished saving all edit areas in", time.time() - t1, "seconds.")
        
        return
  



