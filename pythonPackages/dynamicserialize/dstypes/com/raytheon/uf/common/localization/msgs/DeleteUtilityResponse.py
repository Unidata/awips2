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

class DeleteUtilityResponse(object):

    def __init__(self):
        self.context = None
        self.pathName = None
        self.errorText = None
        self.timeStamp = None

    def getContext(self):
        return self.context

    def setContext(self, context):
        self.context = context

    def getPathName(self):
        return self.pathName

    def setPathName(self, pathName):
        self.pathName = pathName

    def getErrorText(self):
        return self.errorText

    def setErrorText(self, errorText):
        self.errorText = errorText

    def getTimeStamp(self):
        return self.timeStamp

    def setTimeStamp(self, timeStamp):
        self.timeStamp = timeStamp

    def getFormattedErrorMessage(self):
        return "Error deleting " + self.getContextRelativePath() + ": " + self.getErrorText()

    def getContextRelativePath(self):
        return str(self.getContext()) + "/" + self.getPathName()
