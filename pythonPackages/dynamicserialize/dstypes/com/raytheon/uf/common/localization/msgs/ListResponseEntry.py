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

class ListResponseEntry(object):

    def __init__(self):
        self.fileName = None
        self.context = None
        self.date = None
        self.checksum = None
        self.directory = None
        self.protectedLevel = None
        self.existsOnServer = None

    def getFileName(self):
        return self.fileName

    def setFileName(self, fileName):
        self.fileName = fileName

    def getContext(self):
        return self.context

    def setContext(self, context):
        self.context = context

    def getDate(self):
        return self.date

    def setDate(self, date):
        self.date = date

    def getChecksum(self):
        return self.checksum

    def setChecksum(self, checksum):
        self.checksum = checksum

    def getDirectory(self):
        return self.directory

    def setDirectory(self, directory):
        self.directory = directory

    def getProtectedFile(self):
        return self.protectedLevel is not None

    def getProtectedLevel(self):
        return self.protectedLevel

    def setProtectedLevel(self, protectedLevel):
        self.protectedLevel = protectedLevel

    def getExistsOnServer(self):
        return self.existsOnServer

    def setExistsOnServer(self, existsOnServer):
        self.existsOnServer = existsOnServer
        
    def __str__(self):
        return self.fileName

