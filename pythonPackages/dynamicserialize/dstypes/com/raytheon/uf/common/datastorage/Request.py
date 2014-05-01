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

# File auto-generated against equivalent DynamicSerialize Java class

class Request(object):

    def __init__(self):
        self.points = None
        self.indices = None
        self.minIndexForSlab = None
        self.maxIndexForSlab = None
        self.type = None

    def getPoints(self):
        return self.points

    def setPoints(self, points):
        self.points = points

    def getIndices(self):
        return self.indices

    def setIndices(self, indices):
        self.indices = indices

    def getMinIndexForSlab(self):
        return self.minIndexForSlab

    def setMinIndexForSlab(self, minIndexForSlab):
        self.minIndexForSlab = minIndexForSlab

    def getMaxIndexForSlab(self):
        return self.maxIndexForSlab

    def setMaxIndexForSlab(self, maxIndexForSlab):
        self.maxIndexForSlab = maxIndexForSlab

    def getType(self):
        return self.type

    def setType(self, type):
        self.type = type

