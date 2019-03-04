##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
#    Exceptions  -- library of exceptions for Smart Tools and Scripts
#
# Author: hansen
# ----------------------------------------------------------------------------
########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import exceptions

class EditActionError(exceptions.Exception):
    def __init__(self, errorType=None, errorInfo=None):
        exceptions.Exception.__init__(self)
        self.__errorType = errorType
        self.__errorInfo = errorInfo

    def errorType(self):
        return self.__errorType
    def errorInfo(self):
        return self.__errorInfo
    def setErrorInfo(self, info):
        self.__errorInfo = info
