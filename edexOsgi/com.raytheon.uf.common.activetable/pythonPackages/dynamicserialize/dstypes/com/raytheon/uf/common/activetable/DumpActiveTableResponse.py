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
        