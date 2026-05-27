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
# VCModule for PoP ISC Parm, ported from AWIPS1.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/18/2011      10789         dgilling       Initial Creation.
#    
# 
#

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##


from numpy import *


SCALAR  = 'Scalar'
VECTOR  = 'Vector'
WEATHER = 'Weather'
DISCRETE = 'Discrete'
YES = True
NO = False

class VCParm:
    def splitTR(self, tr, inv):
        for btr in inv:
            if self._overlaps(btr, tr):
                if self._containsTR(btr, tr):
                    return []
                else:
                    splitlst = []
                    if not self._contains(btr, tr[0]):
                        splitlst = splitlst + self.splitTR((tr[0], btr[0]),
                                                           inv)
                    if not self._contains(btr, tr[1]):
                        splitlst = splitlst + self.splitTR((btr[1], tr[1]),
                                                           inv)
                    return splitlst
        return [tr]

    def tcmp(self, t1, t2):
        return cmp(t1[0], t2[0])

    def getInventory(self, PoP, PoP_ISC):
        # all Fcst grids get included if they overlap an ISC grid
        # list is sorted in ascending time.
        rval = []
        for tr in PoP:
            lst = []
            for itr in PoP_ISC:
                if self._overlaps(tr, itr):
                    lst.append(itr)
            if len(lst):
                rval.append((tr, [tr], lst))

        # Any ISC grid which does not overlap
        # or any portion that does not overlap
        # goes in as well
        for tr in PoP_ISC:
            for ntr in self.splitTR(tr, PoP):
                for ftr in PoP:
                    if self._overlaps(ntr, ftr):
                        break
                else:
                    rval.append((ntr, [], [tr]))

        #sort the times by starting times
        rval.sort(key=lambda x: x[0][0])
        return rval


    # returns the percentage of tr1 that overlaps tr2
    def poverlap(self, tr1, tr2):
        itr = self._intersect(tr1, tr2)
        return (itr[1] - itr[0]) / float(tr1[1] - tr1[0])

    def calcGrid(self, PoP, PoP_ISC):
        # If getInventory() works right
        # we either get 0 or 1 PoP grids.
        # And if 0 then only 1 PoP_ISC
        if len(PoP) == 0:
            return PoP_ISC[0][1]
        rval = PoP_ISC[0][1]
        for i in PoP_ISC:
            rval = maximum(i[1], rval)
        return rval

    def calcHistory(self, PoP_ISC):
        rval = []
        for t in PoP_ISC:
            for i in t[1]:
                rval.append(i)
        return rval

    def getWEInfo(self):
        # For some reason, setting time dependent to YES
        # makes the GFE loose this ???

        return (("PoP", SCALAR, "%", "Prob of Precip", 100.0, 0.0, 0, NO),
                ("ISC", "V"), (3600, 3600, 0))

    def _containsTR(self, tr1, tr2):
        if tr1 == tr2:
            return True
        return self._contains(tr1, tr2[0]) and self._contains(tr1, tr2[1])

    def _contains(self, timerange, time):
        if timerange[1] - timerange[0]:
            return ((time >= timerange[0]) and (time < timerange[1]))
        return time == timerange[0]

    def _overlaps(self, tr1, tr2):
        if self._contains(tr2, tr1[0]) or self._contains(tr1, tr2[0]):
            return True
        return False

    def _intersect(self, t1, t2):
        if self._overlaps(t1, t2):
            return (max(t1[0], t2[0]), min(t1[1], t2[1]))
        return None
