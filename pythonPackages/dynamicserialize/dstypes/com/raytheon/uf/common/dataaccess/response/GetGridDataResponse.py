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

class GetGridDataResponse(object):

    def __init__(self):
        self.gridData = None
        self.siteNxValues = None
        self.siteNyValues = None
        self.siteLatGrids = None
        self.siteLonGrids = None

    def getGridData(self):
        return self.gridData

    def setGridData(self, gridData):
        self.gridData = gridData
        
    def getSiteNxValues(self):
        return self.siteNxValues

    def setSiteNxValues(self, siteNxValues):
        self.siteNxValues = siteNxValues
        
    def getSiteNyValues(self):
        return self.siteNyValues

    def setSiteNyValues(self, siteNyValues):
        self.siteNyValues = siteNyValues
        
    def getSiteLatGrids(self):
        return self.siteLatGrids

    def setSiteLatGrids(self, siteLatGrids):
        self.siteLatGrids = siteLatGrids
        
    def getSiteLonGrids(self):
        return self.siteLonGrids

    def setSiteLonGrids(self, siteLonGrids):
        self.siteLonGrids = siteLonGrids

