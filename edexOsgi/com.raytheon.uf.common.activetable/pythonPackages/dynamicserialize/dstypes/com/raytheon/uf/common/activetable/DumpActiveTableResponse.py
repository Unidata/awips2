##
##

class DumpActiveTableResponse(object):
    def __init__(self):
        self.dump = None
        self.unfilteredCount = 0
        self.filteredCount = 0
        self.message = None

    def getUnfilteredCount(self):
        return self.unfilteredCount
    
    def getFilteredCount(self):
        return self.filteredCount
            
    def getDump(self):
        return self.dump
    
    def getMessage(self):
        return self.message
    
    def setUnfilteredCount(self, unfilteredCount):
        self.unfilteredCount = unfilteredCount
        
    def setFilteredCount(self, filteredCount):
        self.filteredCount = filteredCount
        
    def setDump(self, dump):
        self.dump = dump
        
    def setMessage(self, message):
        self.message = message
        