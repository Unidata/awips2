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

class ListUtilityCommand(object):

    def __init__(self):
        self.subDirectory = None
        self.recursive = None
        self.filesOnly = None
        self.localizedSite = None
        self.context = None

    def getSubDirectory(self):
        return self.subDirectory

    def setSubDirectory(self, subDirectory):
        self.subDirectory = subDirectory

    def getRecursive(self):
        return self.recursive

    def setRecursive(self, recursive):
        self.recursive = recursive

    def getFilesOnly(self):
        return self.filesOnly

    def setFilesOnly(self, filesOnly):
        self.filesOnly = filesOnly

    def getLocalizedSite(self):
        return self.localizedSite

    def setLocalizedSite(self, localizedSite):
        self.localizedSite = localizedSite

    def getContext(self):
        return self.context

    def setContext(self, context):
        self.context = context

