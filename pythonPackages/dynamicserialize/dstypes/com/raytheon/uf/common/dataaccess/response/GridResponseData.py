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
#
#


from dynamicserialize.dstypes.com.raytheon.uf.common.dataaccess.response import AbstractResponseData

class GridResponseData(AbstractResponseData):

    def __init__(self):
        super(GridResponseData, self).__init__()
        self.parameter = None
        self.unit = None
        self.gridData = None

    def getParameter(self):
        return self.parameter

    def setParameter(self, parameter):
        self.parameter = parameter

    def getUnit(self):
        return self.unit

    def setUnit(self, unit):
        self.unit = unit

    def getGridData(self):
        return self.gridData

    def setGridData(self, gridData):
        self.gridData = gridData
