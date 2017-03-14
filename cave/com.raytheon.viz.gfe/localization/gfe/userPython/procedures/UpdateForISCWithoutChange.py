# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# UpdateForISCWithoutChange.py
# 
# Searches the mutable database for grids that have not been sent to ISC for
# greater than THRESHOLD number of seconds and locks them so they will be saved and 
# resent without changing their contents.
#
# Author: randerso
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/05/15         #4012        randerso       Initial creation
#
##

THRESHOLD = 6 * 60 * 60 # 6 hours in seconds

MenuItems = ["Edit"]

VariableList = []

import AbsTime
import SmartScript
import TimeRange

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        current = AbsTime.current()
        mutableDbId = self.mutableID()
        iscDbId = self.findDatabase("_ISC")
        
        # get the list of ISC parms
        iscParms = self.availableParms(iscDbId)
        for iscParm in iscParms:
            
            # find the associated mutable parm, if any
            mutableParm = self.getParm(mutableDbId, iscParm[0], iscParm[1])
            if mutableParm is not None:
                
                # loop over every grid in the parm's inventory
                inv = mutableParm.getGridInventory()
                for i in inv:
                    for history in i.getHistory():
                        # if the grid was last sent more THRESHOLD seconds ago lock it
                        if history.getLastSentTime() is not None:
                            lastSentTime = AbsTime.AbsTime(history.getLastSentTime())
                            delta = current - lastSentTime
                            if delta > THRESHOLD:
                                i.lockGrid()
                        