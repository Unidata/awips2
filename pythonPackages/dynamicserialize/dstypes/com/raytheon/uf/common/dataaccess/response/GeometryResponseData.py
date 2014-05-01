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
# and then modified post-generation to use AbstractResponseData.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/04/13         #2023        dgilling       Initial Creation.
#    01/06/14         #2537        bsteffen       Store geometry index instead of WKT.
#
#


from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.response import AbstractResponseData

class GeometryResponseData(AbstractResponseData):

    def __init__(self):
        super(GeometryResponseData, self).__init__()
        self.dataMap = None
        self.geometryWKTindex = None

    def getDataMap(self):
        return self.dataMap

    def setDataMap(self, dataMap):
        self.dataMap = dataMap

    def getGeometryWKTindex(self):
        return self.geometryWKTindex

    def setGeometryWKTindex(self, geometryWKTindex):
        self.geometryWKTindex = geometryWKTindex
