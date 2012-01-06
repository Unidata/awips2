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

class ProjectionData(object):

    def __init__(self):
        self.projectionID = None
        self.projectionType = None
        self.latLonLL = None
        self.latLonUR = None
        self.latLonOrigin = None
        self.stdParallelOne = None
        self.stdParallelTwo = None
        self.gridPointLL = None
        self.gridPointUR = None
        self.latIntersect = None
        self.lonCenter = None
        self.lonOrigin = None

    def getProjectionID(self):
        return self.projectionID

    def setProjectionID(self, projectionID):
        self.projectionID = projectionID

    def getProjectionType(self):
        return self.projectionType

    def setProjectionType(self, projectionType):
        self.projectionType = projectionType

    def getLatLonLL(self):
        return self.latLonLL

    def setLatLonLL(self, latLonLL):
        self.latLonLL = latLonLL

    def getLatLonUR(self):
        return self.latLonUR

    def setLatLonUR(self, latLonUR):
        self.latLonUR = latLonUR

    def getLatLonOrigin(self):
        return self.latLonOrigin

    def setLatLonOrigin(self, latLonOrigin):
        self.latLonOrigin = latLonOrigin

    def getStdParallelOne(self):
        return self.stdParallelOne

    def setStdParallelOne(self, stdParallelOne):
        self.stdParallelOne = stdParallelOne

    def getStdParallelTwo(self):
        return self.stdParallelTwo

    def setStdParallelTwo(self, stdParallelTwo):
        self.stdParallelTwo = stdParallelTwo

    def getGridPointLL(self):
        return self.gridPointLL

    def setGridPointLL(self, gridPointLL):
        self.gridPointLL = gridPointLL

    def getGridPointUR(self):
        return self.gridPointUR

    def setGridPointUR(self, gridPointUR):
        self.gridPointUR = gridPointUR

    def getLatIntersect(self):
        return self.latIntersect

    def setLatIntersect(self, latIntersect):
        self.latIntersect = latIntersect

    def getLonCenter(self):
        return self.lonCenter

    def setLonCenter(self, lonCenter):
        self.lonCenter = lonCenter

    def getLonOrigin(self):
        return self.lonOrigin

    def setLonOrigin(self, lonOrigin):
        self.lonOrigin = lonOrigin
        
    def keys(self):
        return ['projectionID', 'projectionType', 'latLonLL', 'latLonUR', 
                'latLonOrigin', 'stdParallelOne', 'stdParallelTwo', 
                'gridPointLL', 'gridPointUR', 'latIntersect', 'lonCenter', 
                'lonOrigin']

