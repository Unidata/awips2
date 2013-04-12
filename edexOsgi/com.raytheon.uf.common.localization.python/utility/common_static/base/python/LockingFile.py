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
# Python needs to have access to a context, not to create though
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash        Initial Creation.
#    
# 
#

from com.raytheon.uf.common.localization import FileLocker, FileLocker_Type as Type
from java.io import File as JavaFile
from java.lang import Object


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
        FileLocker.unlock(self.lockerObject, self.file)
        return super(File,self).close()
    
    