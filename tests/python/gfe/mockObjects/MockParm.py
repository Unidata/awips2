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
import sys
import numpy as np
import MockGridData
import MockSlice
import TimeRange
from com.raytheon.uf.common.time import TimeRange as TR 

##
# A Python stand-in for a Java Parm, for unit testing.
class MockParm(object):
    
    ##
    # Constructor.
    # @param id: ID of this Parm, so we can tell if the right one is being used.
    # @type id: string
    # @param outObj: place to log events
    # @type outObj: file-like
    def __init__(self, id, outObj=sys.stdout):
        self.id = id
        self.outObj = outObj
        self.gridInventory = []
        self.lockedTRs = []
        
    ##
    # Stub substitute for the deleteTR() method of Java Parm interface.
    # This method writes a string containing timeRange and self.id to
    # self.outObj. 
    # @param timeRange: The time range of grids to delete from this parm. 
    # @type timeRange: com.raytheon.uf.common.time.TimeRange
    def deleteTR(self, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            raise TypeError, "timeRange is python wrapper object, not proper Java object"
        if not timeRange.isValid():
            raise RuntimeError, "Java time range is invalid"
        
        self.outObj.write("parm(" + self.id + "):deleteTR(" + repr(timeRange) + ")\n")
        self.outObj.flush()
        
    def getGridInventory(self, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            raise TypeError, "timeRange is python wrapper object, not proper Java object"
        if not timeRange.isValid():
            raise RuntimeError, "Java time range is invalid"
        
        return self.gridInventory
    
    def startParmEdit(self, dateList):
        rtnGrids = []
        for idx in range(len(dateList)):
            date = dateList[idx]
            tr = TR(date, 1000L)
            for lockedTR in self.lockedTRs:
                if lockedTR.overlaps(tr):
                    rte = RuntimeError("com.raytheon.viz.gfe.GFEOperationFailedException: locked timerange: " + str(tr));
                    raise rte
            self.lockedTRs.append(tr)
            slice = MockSlice.MockSlice(tr)
            data = MockGridData.MockGridData(slice)
            rtnGrids.append(data)
        return rtnGrids

    def endParmEdit(self):
        self.lockedTRs = []
