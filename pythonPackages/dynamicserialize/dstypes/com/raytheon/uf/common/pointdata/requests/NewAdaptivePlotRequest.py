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

class NewAdaptivePlotRequest(object):

    def __init__(self):
        self.fileContents = None
        self.fileName = None
        self.bundleName = None
        self.description = None

    def getFileContents(self):
        return self.fileContents

    def setFileContents(self, fileContents):
        self.fileContents = fileContents

    def getFileName(self):
        return self.fileName

    def setFileName(self, fileName):
        self.fileName = fileName

    def getBundleName(self):
        return self.bundleName

    def setBundleName(self, bundleName):
        self.bundleName = bundleName

    def getDescription(self):
        return self.description

    def setDescription(self, description):
        self.description = description

