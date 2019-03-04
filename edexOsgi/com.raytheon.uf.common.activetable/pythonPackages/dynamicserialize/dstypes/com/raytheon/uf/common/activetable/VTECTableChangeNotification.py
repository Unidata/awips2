##
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
