# #
# #

#
# Python can have these and be able to make modifications and save
# files back to the server through this class.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash          Initial Creation.
#    Apr 27, 2015     4259         njensen        Updated for new JEP API
#    Aug 08, 2017     6379         njensen        Removed isProtected()
#    
# 
#

import JUtil
from datetime import datetime
from LockingFile import File

from java.io import File as JavaFile
from com.raytheon.uf.common.localization import LocalizationContext
JavaLocalizationLevel = LocalizationContext.LocalizationLevel 


class LocalizationFile(JUtil.JavaWrapperClass):
    
    def __init__(self, wrappedObject):
        self.jobj = wrappedObject
    
    def save(self):
        '''
        @summary: Used to save the localization file back to localization
        '''
        return self.jobj.save()
        
    def delete(self):
        '''
        @summary: Used to delete the localization file from localization
        '''
        return self.jobj.delete()
    
    def exists(self):
        '''
        @summary: Check if the file actually exists
        '''
        return self.jobj.exists()
    
    def getName(self):
        '''
        @summary: Get the name of the file.
        '''
        return self.jobj.getName()
    
    def getPath(self):
        '''
        @summary: Get the path of the file
        '''
        return self.jobj.getFile().getAbsolutePath()
    
    def getFile(self, mode='r'):
        '''
        @param mode: The mode.
        @summary: Return a file handle to the file
        '''
        retrieveFile = True
        if mode == 'w':
            retrieveFile = False
        return File(self.jobj.getFile(retrieveFile), self.getName(), mode)

    def getTimeStamp(self):
        '''
        @summary: Get the timestamp of the file as a python datetime.
        '''
        return datetime.fromtimestamp(self.jobj.getTimeStamp().getTime() / 1000.0)
    
    def isAvailableOnServer(self):
        '''
        @summary: Is this file available on the server
        '''
        return self.jobj.isAvailableOnServer()
    
    def isDirectory(self):
        '''
        @summary: Is this a directory.
        '''
        return self.jobj.isDirectory()
    
    def __eq__(self, other):
        return self.jobj.equals(other.jobj)
    
    def toJavaObj(self):
        '''
        @summary: Returns the behind the scenes Java object
        '''
        return self.jobj
    
    
