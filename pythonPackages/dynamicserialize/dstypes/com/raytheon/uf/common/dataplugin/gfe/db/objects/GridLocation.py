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

class GridLocation(object):

    def __init__(self):
        self.siteId = None
        self.nx = None
        self.ny = None
        self.timeZone = None
        self.projection = None
        self.origin = None
        self.extent = None
        self.geometry = None
        self.crsWKT = None
        self.identifier = None
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        s = "[SiteID =" + self.siteId + ",ProjID=" + self.projection.getProjectionID() +\
            ",gridSize=(" + str(self.nx) + ',' + str(self.ny) + ")"
            # TODO: Handle geometry in dynamicserialize
            # ,loc=" + this.geometry.getGeometryType();
        s += ']'
        return s
    
    def __eq__(self, other):
        if not isinstance(other, GridLocation):
            return False
        if self.siteId != other.siteId:
            return False
        if self.crsWKT != other.crsWKT:
            return False
        # FIXME: Geometry/Polygon objects don't really work in dynamicserialize
        # commenting out this check unless it causes problems
#        if self.geometry != other.geometry:
#            return False
        if self.nx != other.nx:
            return False
        if self.ny != other.ny:
            return False
        return True
    
    def __ne__(self, other):
        return (not self.__eq__(other))

    def getSiteId(self):
        return self.siteId

    def setSiteId(self, siteId):
        self.siteId = siteId

    def getNx(self):
        return self.nx

    def setNx(self, nx):
        self.nx = nx

    def getNy(self):
        return self.ny

    def setNy(self, ny):
        self.ny = ny

    def getTimeZone(self):
        return self.timeZone

    def setTimeZone(self, timeZone):
        self.timeZone = timeZone

    def getProjection(self):
        return self.projection

    def setProjection(self, projection):
        self.projection = projection

    def getOrigin(self):
        return self.origin

    def setOrigin(self, origin):
        self.origin = origin

    def getExtent(self):
        return self.extent

    def setExtent(self, extent):
        self.extent = extent

    def getGeometry(self):
        return self.geometry

    def setGeometry(self, geometry):
        self.geometry = geometry

    def getCrsWKT(self):
        return self.crsWKT

    def setCrsWKT(self, crsWKT):
        self.crsWKT = crsWKT

    def getIdentifier(self):
        return self.identifier

    def setIdentifier(self, identifier):
        self.identifier = identifier
        
    def isValid(self):
        if self.projection is None:
            return False
        if self.nx < 2 or self.ny < 2:
            return False
        if self.origin is None or self.extent is None:
            return False 
        return True

