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
# Jul 27, 2015 4654     skorolev     Added filters

class AlertVizRequest(object):

    def __init__(self):
        self.message = None
        self.machine = None
        self.priority = None
        self.sourceKey = None
        self.category = None
        self.audioFile = None
        self.filters = None
        
    def getMessage(self):
        return self.message

    def setMessage(self, message):
        self.message = message

    def getMachine(self):
        return self.machine

    def setMachine(self, machine):
        self.machine = machine

    def getPriority(self):
        return self.priority

    def setPriority(self, priority):
        self.priority = priority

    def getSourceKey(self):
        return self.sourceKey

    def setSourceKey(self, sourceKey):
        self.sourceKey = sourceKey

    def getCategory(self):
        return self.category

    def setCategory(self, category):
        self.category = category

    def getAudioFile(self):
        return self.audioFile

    def setAudioFile(self, audioFile):
        self.audioFile = audioFile
        
    def getFilters(self):
        return self.filters
    
    def setFilters(self, filters):
        if filters is None:
           self.filters = {}
        elif not(filters.has_key(None) or filters.values().count(None)>0 or filters.has_key('') or filters.values().count('')>0):
           self.filters = filters
        else:
           raise ValueError('Filters must not contain None or empty keys or values: %s' % filters)
