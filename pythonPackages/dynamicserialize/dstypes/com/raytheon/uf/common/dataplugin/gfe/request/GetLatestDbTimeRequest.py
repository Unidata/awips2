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
# and then modified post-generation to use AbstractGfeRequest and
# implement str(), repr()
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/22/13         2025         dgilling       Initial Creation.
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import AbstractGfeRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID


class GetLatestDbTimeRequest(AbstractGfeRequest):

    def __init__(self, dbId=None):
        super(GetLatestDbTimeRequest, self).__init__()
        if dbId is not None and isinstance(dbId, DatabaseID):
            self.dbId = dbId
            self.siteID = dbId.getSiteId()
        elif dbId is not None and not isinstance(dbId, DatabaseID):
            raise TypeError(
                "Attempt to construct GetLatestDbTimeRequest without providing a valid DatabaseID.")
        
    def __str__(self):
        retVal = "GetLatestDbTimeRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "dbId: " + str(self.dbId) + "]"
        return retVal
    
    def __repr__(self):
        retVal = "ExecuteIfpNetCDFGridRequest(" 
        retVal += "wokstationID=" + repr(self.workstationID) + ", "
        retVal += "siteID=" + repr(self.siteID) + ", "
        retVal += "dbId=" + repr(self.dbId) + ")"
        return retVal

    def getDbId(self):
        return self.dbId

    def setDbId(self, dbId):
        if isinstance(dbId, DatabaseID):
            self.dbId = dbId
        else:
            raise TypeError(
                "Attempt to call GetLatestDbTimeRequest.setDbId() without providing a valid DatabaseID.")
