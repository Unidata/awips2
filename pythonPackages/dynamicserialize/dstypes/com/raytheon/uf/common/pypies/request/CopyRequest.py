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

class CopyRequest(object):

    def __init__(self):
        self.repack = None
        self.repackCompression = None
        self.outputDir = None
        self.timestampCheck = None
        self.minMillisSinceLastChange = None
        self.maxMillisSinceLastChange = None
        self.filename = None

    def getRepack(self):
        return self.repack

    def setRepack(self, repack):
        self.repack = repack

    def getRepackCompression(self):
        return self.repackCompression

    def setRepackCompression(self, repackCompression):
        self.repackCompression = repackCompression

    def getOutputDir(self):
        return self.outputDir

    def setOutputDir(self, outputDir):
        self.outputDir = outputDir

    def getTimestampCheck(self):
        return self.timestampCheck

    def setTimestampCheck(self, timestampCheck):
        self.timestampCheck = timestampCheck

    def getMinMillisSinceLastChange(self):
        return self.minMillisSinceLastChange

    def setMinMillisSinceLastChange(self, minMillisSinceLastChange):
        self.minMillisSinceLastChange = minMillisSinceLastChange

    def getMaxMillisSinceLastChange(self):
        return self.maxMillisSinceLastChange

    def setMaxMillisSinceLastChange(self, maxMillisSinceLastChange):
        self.maxMillisSinceLastChange = maxMillisSinceLastChange

    def getFilename(self):
        return self.filename

    def setFilename(self, filename):
        self.filename = filename
