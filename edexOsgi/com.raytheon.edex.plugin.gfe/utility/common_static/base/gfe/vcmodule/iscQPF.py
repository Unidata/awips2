# -*-python-*-
import time
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

    def getInventory(self, QPF, QPF_ISC):
        # all Fcst grids get included if they overlap an ISC grid
        rval = []
        for tr in QPF:
            lst = []
            for itr in QPF_ISC:
                if self._overlaps(tr, itr):
                    lst.append(itr)
            if len(lst):
                rval.append((tr, [tr], lst))

        # Any ISC grid which does not overlap
        # or any portion that does not overlap
        # goes in as well
        for tr in QPF_ISC:
            for ntr in self.splitTR(tr, QPF):
                for ftr in QPF:
                    if self._overlaps(ntr, ftr):
                        break
                else:
                    rval.append((ntr, [], [tr]))

        #must sort in ascending order
        rval.sort(lambda x,y:cmp(x[0][0], y[0][0]))

        return rval

    # returns the percentage of tr1 that overlaps tr2
    def poverlap(self, tr1, tr2):
        itr = self._intersect(tr1, tr2)
        return (itr[1] - itr[0]) / float(tr1[1] - tr1[0])

    def calcGrid(self, QPF, QPF_ISC):
        # If getInventory() works right
        # we either get 0 or 1 QPF grids.
        # And if 0 then only 1 QPF_ISC
        if len(QPF) == 0:
            return QPF_ISC[0][1]
        rval = QPF[0][1] * 0.0
        for i in QPF_ISC:
            rval = rval + i[1] * self.poverlap(i[0], QPF[0][0])
        return rval

    def calcHistory(self, QPF_ISC):
        rval = []
        for t in QPF_ISC:
            for i in t[1]:
                rval.append(i)
        return rval

    def getWEInfo(self):
        # For some reason, setting time dependent to YES
        # makes the GFE loose this ???
        return (("QPF", SCALAR, "in", "QPF", 5.0, 0.0, 2, YES),
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
