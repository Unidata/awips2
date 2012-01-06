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

import TimeRange
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GridParmInfo

class GridInfo(object):
    ##
    # Constructor. gridTime is required, supply gridParmInfo OR the rest of
    # the parameters (not both).
    #
    def __init__(self, parmID=None, gridLoc=None, maxLimit=None, minLimit=None, 
                 units=None, gridTime=None, type=None, timeIndependentParm=None, 
                 timeConstraints=None, precision=None, rateParm=None, 
                 descriptiveName=None, gridParmInfo=None):
        if (gridParmInfo==None):
            gridParmInfo = GridParmInfo(parmID, gridLoc, type, units, 
                                        descriptiveName, minLimit, maxLimit,
                                        precision, timeIndependentParm,
                                        timeConstraints, rateParm)
        elif parmID is not None or \
             gridLoc is not None or \
             maxLimit is not None or \
             minLimit is not None or \
             units is not None or \
             type is not None or \
             timeIndependentParm is not None or \
             timeConstraints is not None or \
             precision is not None or \
             rateParm is not None or \
             descriptiveName is not None:
            raise IllegalArgumentException("Only gridTime can be specified with gridParmInfo")
        
        self.gridParmInfo = gridParmInfo
        if isinstance(gridTime, TimeRange.TimeRange):
            self._gridTime = gridTime;
        else :
            self._gridTime = TimeRange.TimeRange(gridTime)

    ##
    # Get the parm ID of the Parm this grid belongs to.
    # @return: the Parm ID
    # @rtype: com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID
    def getParmID(self):
        return self.gridParmInfo.getParmID()
    
    ##
    # Return the grid location.
    # @return: the grid location.
    # @rtype com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation
    def gridLocation(self):
        return self.gridParmInfo.getGridLoc()
    
    ##
    # Return the maximum value allowed for this grid.
    # @return: Maximum value
    # @rtype: float
    def maxLimit(self):
        return self.gridParmInfo.getMaxValue()
    
    ##
    # Return the minimum value allowed for this grid.
    # @return Minimum value
    # @rtype: float
    def minLimit(self):
        return self.gridParmInfo.getMinValue()
    
    ##
    # Return the time range of this grid.
    # @return: The valid time range of the grid.
    # @rtype: TimeRange.TimeRange
    def gridTime(self):
        return self._gridTime
    
    ##
    # Return the grid type.
    # @return: the grid type 
    # @rtype: com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType
    def type(self):
        return self.gridParmInfo.getGridType()
    
    ##
    # The parm units, as a String.
    # @return: The units 
    # @rtype: String
    def units(self):
        return self.gridParmInfo.getUnitString()
    
    ##
    # @return: Whether this is a time independent parm.
    # @rtype: boolean
    def timeIndependentParm(self):
        return self.gridParmInfo.getTimeIndependentParm()

    ##
    # @return: The time constraints of this grid
    # @rtype: com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints
    def tc(self):
        return self.gridParmInfo.getTimeConstraints()
    
    ##
    # @rtype: int
    def precision(self):
        return self.gridParmInfo.getPrecision()
    
    ##
    # Return whether this grid's parm is a rate parm.
    # @rtype: boolean
    def rateParm(self):
        return self.gridParmInfo.isRateParm()

