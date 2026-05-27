# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# ----------------------------------------------------------------------------
##
# EditAreaUtilities
#
# An interface to create and maintain a set of edit areas outside of CAVE.
# The Data Access Framework does not offer edit areas as a data type, so
# this code is necessary to store and retrieve data outside of CAVE.
#
# Author: lefebvre
#
# SOFTWARE HISTORY
#  Date        Ticket#    Engineer    Description
#  ----------- ---------- ----------- --------------------------
#  08/05/2016    -        tlefebvre   Better error checking when mask file not found.
#  12/02/2016    -        tlefebvre   Documented code.
#  12/20/2017  DCS17686   tlefebvre   Initial baseline version.
#
##
# ----------------------------------------------------------------------------

import numpy as np
import os

class EditAreaUtilities():
    def __init__(self, repositoryPath):
        self._repositoryPath = repositoryPath
    
    # Saves the specified mask under the name eaName into the edit area repository
    def saveEditArea(self, eaName, mask):
        
        # Use the numpy method to save the mask
        np.save(self._repositoryPath + eaName, mask)
        
        return
        
    # Retreives an edit area in the form of a mask from the repository
    def fetchEditArea(self, name):
        
        fullPath = os.path.join(self._repositoryPath, name + ".npy")
        if not os.path.exists(fullPath):

            print("Error!", fullPath, "not found in EditArea repository")
            return None
        
        try:
            mask = np.load(fullPath)
        except:
            print("Error reading edit area:", name, "from EditArea repository.")
            return None
        
        # Make very sure we're returning a good mask or None
        if mask is None:
            return None
        
        return mask # return the contents of the file rather than the file itself
    
    # Returns the names of all the edit areas in the repository 
    def allEditAreaNames(self):
        
        fileList = os.listdir(self._repositoryPath)
        nameList = []
        for fileName in fileList:
            if ".npy" in fileName:
                nameList.append(fileName[:-4]) 
        
        return nameList
        
