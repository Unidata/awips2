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


#
# Implements IGeometryRequest and wraps around a Java IGeometryRequest.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/18/12                      njensen       Initial Creation.
#    
# 
#

from ufpy.dataaccess import IGeometryRequest
import JUtil, JDataRequest
import jep
import shapely.wkt

class JGeometryRequest(IGeometryRequest, JDataRequest.JDataRequest):
    
    def __init__(self, wrappedObject):
        JDataRequest.JDataRequest.__init__(self, wrappedObject)
        
    def getEnvelope(self):
        env = None
        jenv = self.jobj.getEnvelope()        
        if jenv:
            from com.vividsolutions.jts.geom import GeometryFactory
            env = shapely.wkt.loads(GeometryFactory().toGeometry(jenv).toText())
        return env
    
    def setEnvelope(self, env):
        from com.vividsolutions.jts.geom import Envelope        
        bounds = env.bounds        
        jenv = Envelope(bounds[0], bounds[2], bounds[1], bounds[3])
        self.jobj.setEnvelope(bounds)        
    
    def getLocationNames(self):        
        return self.jobj.getLocationNames()
    
    def setLocationNames(self, *args):
        from java.lang import String as JavaString
        locs = jep.jarray(len(args), JavaString)
        for i in xrange(len(args)):
            locs[i] = str(args[i])
        self.jobj.setLocationNames(locs)    

