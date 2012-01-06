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

# File auto-generated against equivalent DynamicSerialize Java class

class StackTraceElement(object):

    def __init__(self):
        self.declaringClass = None
        self.methodName = None
        self.fileName = None
        self.lineNumber = 0
        
    def getDeclaringClass(self):
        return self.declaringClass
    
    def setDeclaringClass(self, clz):
        self.declaringClass = clz
        
    def getMethodName(self):
        return self.methodName
    
    def setMethodName(self, methodName):
        self.methodName = methodName
        
    def getFileName(self):
        return self.fileName
    
    def setFileName(self, filename):
        self.fileName = filename
        
    def getLineNumber(self):
        return self.lineNumber
    
    def setLineNumber(self, lineNumber):
        self.lineNumber = int(lineNumber)
        
    def isNativeMethod(self):
        return (self.lineNumber == -2)
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        msg = self.declaringClass + "." + self.methodName
        if self.isNativeMethod():
            msg += "(Native Method)"
        elif self.fileName is not None and self.lineNumber >= 0:
            msg += "(" + self.fileName + ":" + str(self.lineNumber) + ")"
        elif self.fileName is not None:
            msg += "(" + self.fileName + ")"
        else:
            msg += "(Unknown Source)"
        return msg
        
        
