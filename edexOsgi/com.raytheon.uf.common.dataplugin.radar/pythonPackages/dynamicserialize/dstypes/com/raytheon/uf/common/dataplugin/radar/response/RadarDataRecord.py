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
# 
#      SOFTWARE HISTORY
# 
#     Date            Ticket#       Engineer       Description
#     ------------    ----------    -----------    --------------------------
#     Aug 19, 2014                  nabowle        Generated

class RadarDataRecord(object):

    def __init__(self):
        self.hdf5Data = None
        self.trueElevationAngle = None
        self.elevationNumber = None
        self.elevation = None
        self.longitude = None
        self.latitude = None
        self.dataTime = None
        self.volumeCoveragePattern = None

    def getHdf5Data(self):
        return self.hdf5Data

    def setHdf5Data(self, hdf5Data):
        self.hdf5Data = hdf5Data

    def getTrueElevationAngle(self):
        return self.trueElevationAngle

    def setTrueElevationAngle(self, trueElevationAngle):
        self.trueElevationAngle = trueElevationAngle

    def getElevationNumber(self):
        return self.elevationNumber

    def setElevationNumber(self, elevationNumber):
        self.elevationNumber = elevationNumber

    def getElevation(self):
        return self.elevation

    def setElevation(self, elevation):
        self.elevation = elevation

    def getLongitude(self):
        return self.longitude

    def setLongitude(self, longitude):
        self.longitude = longitude

    def getLatitude(self):
        return self.latitude

    def setLatitude(self, latitude):
        self.latitude = latitude

    def getDataTime(self):
        return self.dataTime

    def setDataTime(self, dataTime):
        self.dataTime = dataTime

    def getVolumeCoveragePattern(self):
        return self.volumeCoveragePattern

    def setVolumeCoveragePattern(self, volumeCoveragePattern):
        self.volumeCoveragePattern = volumeCoveragePattern

