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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Sep 01, 2014  3572     randerso  Fix getTopo
# Apr 23, 2015  4259     njensen   Updated for new JEP API
# Dec 02, 2015  18356    yteng     Fix typo in __getitem__
# Jan 04, 2018  7178     randerso  removed populate call
# Apr 27, 2018  7256     randerso  Raise KeyError instead of returning null for
#                                  missing parm
#
########################################################################
import DatabaseID, AbsTime, JUtil

from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID as JavaDatabaseID
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID

import numpy

class DBSSWE:
    def __init__(self, parm):
        self._parm = parm
        self.parmName = parm.getParmID().getCompositeName()

    def keys(self):
        #return map(lambda x : (
        #    x.getGridTime().getStart().unixTime(),
        #    x.getGridTime().getEnd().unixTime()),
        #                       self._parm.getGridInventory())
        result = []
        times = self._parm.getGridInventory()
        for x in times:
            start = AbsTime.AbsTime(x.getGridTime().getStart())
            end = AbsTime.AbsTime(x.getGridTime().getEnd())
            encodedTime = (start.unixTime(), end.unixTime())
            result.append(encodedTime)
        return result

    def __getitem__(self, key):
        for t, g in map(lambda x, y: (x, y), list(self.keys()),
                        self._parm.getGridInventory()):
            if t == key:
                gridSlice = g.getGridSlice()
                result = gridSlice.getNDArray()
                if type(result) is numpy.ndarray and result.dtype == numpy.int8:
                    # discrete or weather
                    dkeys = JUtil.javaObjToPyVal(gridSlice.getKeyList())
                    result = [result, dkeys]
                return result
        return None

    def __iter__(self):
        return iter(self.keys())

    def __contains__(self, item):
        return item in self.keys()


class DBSSDB:
    def __init__(self, pmgr, key):
        javaDbId = JavaDatabaseID(key)
        self._pmgr = pmgr
        self._dbid = DatabaseID.DatabaseID(javaDbId)

    def keys(self):
        return [str(x.getCompositeName()) for x in self._pmgr.getAvailableParms(self._dbid.toJavaObj())]

    def __getitem__(self, key):
        #pid = AFPS.ParmID_string(key + ":" + str(self._dbid))
        pid = ParmID(key + ":" + str(self._dbid))
        parm = self._pmgr.getParm(pid)
        if parm:
            return DBSSWE(parm)
        else:
            raise KeyError("Weather element %s is not loaded." % key)

    def __iter__(self):
        return iter(self.keys())

    def __contains__(self, item):
        return item in self.keys()


class DBSSClient:
    def __init__(self, dataMgr):
        self._dataMgr = dataMgr
        self._pmgr = dataMgr.getParmManager()
        self._refmgr = dataMgr.getRefManager()
        self._tmgr = dataMgr.getTopoManager()
        self.siteIDs = [dataMgr.getSiteID()]

    def keys(self):
        dbs = self._pmgr.getAvailableDbs()
        availDbs = []
        for i in range(dbs.size()):
            availDbs.append(dbs.get(i))
        return [str(x) for x in availDbs]

    def __getitem__(self, key):
        return DBSSDB(self._pmgr, key)

    def editAreaNames(self):
        result = []
        avail = self._refmgr.getAvailableSets()
        size = avail.size()
        for x in range(size):
            result.append(avail.get(x).getName())
        #return map(lambda x : x.name(), self._refmgr.getAvailableSets())
        return result

    def getEditArea(self, name):
        rs = self._refmgr.loadRefSet(ReferenceID(name))
        if rs.isQuery():
            return rs.getQuery()
        return rs.getGrid().getNDArray()

    def getOpMode(self):
        return self._dataMgr.getOpMode().name()

    def getTopo(self):
        return self._tmgr.getCompositeTopo().getNDArray()

    def __iter__(self):
        return iter(self.keys())

    def __contains__(self, item):
        return item in self.keys()
