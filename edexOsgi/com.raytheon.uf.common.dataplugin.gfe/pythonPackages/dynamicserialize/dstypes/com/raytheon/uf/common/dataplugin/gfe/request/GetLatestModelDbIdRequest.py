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


class GetLatestModelDbIdRequest(AbstractGfeRequest):

    def __init__(self, siteId=None, modelName=None):
        super(GetLatestModelDbIdRequest, self).__init__()
        if siteId is not None:
            self.siteID = str(siteId)
        if modelName is not None:
            self.modelName = str(modelName)
        
    def __str__(self):
        retVal = "GetLatestModelDbIdRequest[" 
        retVal += "wokstationID: " + str(self.workstationID) + ", "
        retVal += "siteID: " + str(self.siteID) + ", "
        retVal += "modelName: " + str(self.modelName) + "]"
        return retVal
    
    def __repr__(self):
        retVal = "ExecuteIfpNetCDFGridRequest(" 
        retVal += "wokstationID=" + repr(self.workstationID) + ", "
        retVal += "siteID=" + repr(self.siteID) + ", "
        retVal += "modelName=" + repr(self.modelName) + ")"
        return retVal

    def getModelName(self):
        return self.modelName

    def setModelName(self, modelName):
        self.modelName = str(modelName)
