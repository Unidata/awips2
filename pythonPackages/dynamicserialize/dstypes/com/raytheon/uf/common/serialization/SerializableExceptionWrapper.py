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

# File auto-generated against equivalent DynamicSerialize Java class

class SerializableExceptionWrapper(object):

    def __init__(self):
        self.stackTrace = None
        self.message = None
        self.exceptionClass = None
        self.wrapper = None

    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        if not self.message:
            self.message = ''
        retVal = "" + self.exceptionClass + " exception thrown: " + self.message + "\n"
        for element in self.stackTrace:
            retVal += "\tat " + str(element) + "\n"
        return retVal
    
    def getStackTrace(self):
        return self.stackTrace

    def setStackTrace(self, stackTrace):
        self.stackTrace = stackTrace

    def getMessage(self):
        return self.message

    def setMessage(self, message):
        self.message = message

    def getExceptionClass(self):
        return self.exceptionClass

    def setExceptionClass(self, exceptionClass):
        self.exceptionClass = exceptionClass

    def getWrapper(self):
        return self.wrapper

    def setWrapper(self, wrapper):
        self.wrapper = wrapper

