# #
# #

#
# Python needs to have access to a context, not to create though
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash          Initial Creation.
#    09/25/13         2250         mnash          Fix for FileLocker.unlock
#                                                 not being correctly called
#    Apr 27, 2015     4259         njensen        Updated for new JEP API
#    
# 
#

from com.raytheon.uf.common.localization import FileLocker
Type = FileLocker.Type 
from java.io import File as JavaFile
from java.lang import Object
import jep

class File(file):
    
    def __init__(self, file, name, mode):
        self.lockerObject = Object()
        
        type = Type.WRITE
        if mode == 'r':
            type = Type.READ
        
        self.file = file
        locked = FileLocker.lock(self.lockerObject, file, type)
        # log if not locked, because that's bad
        super(File, self).__init__(file.getAbsolutePath(), mode)
    
    def close(self):
        array = jep.jarray(1, JavaFile)
        array[0] = self.file
        FileLocker.unlock(self.lockerObject, array)
        return super(File, self).close()
    
    
