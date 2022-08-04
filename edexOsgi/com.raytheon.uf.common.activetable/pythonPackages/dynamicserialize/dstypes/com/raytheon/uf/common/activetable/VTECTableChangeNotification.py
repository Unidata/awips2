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

class VTECTableChangeNotification(object):

    def __init__(self):
        self.mode = None
        self.modTime = None
        self.modSource = None
        self.changes = None

    def getMode(self):
        return self.mode

    def setMode(self, mode):
        self.mode = mode

    def getModTime(self):
        return self.modTime

    def setModTime(self, modTime):
        self.modTime = modTime

    def getModSource(self):
        return self.modSource

    def setModSource(self, modSource):
        self.modSource = modSource

    def getChanges(self):
        return self.changes

    def setChanges(self, changes):
        self.changes = changes

    def __repr__(self):
        msg = 'Table Name: ' + str(self.mode) + '\n'
        msg += 'ModTime: ' + str(self.modTime) + '\n'
        msg += 'ModSource: ' + str(self.modSource)
        return msg
