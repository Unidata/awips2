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
import DatabaseID, AbsTime

from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID as JavaDatabaseID
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID

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
        for t, g in map(lambda x, y: (x, y), self.keys(),
                        self._parm.getGridInventory()):
            if t == key:                
                #return g.pyData()
                g.populate()     
                result = g.getGridSlice().__numpy__
                if len(result) == 1:
                    result = result[0]
                elif len(result) == 2 and isinstance(result[1], str):
                    result[1] = eval(result[1])
                return result
        return None

class DBSSDB:
    def __init__(self, pmgr, key):        
        javaDbId = JavaDatabaseID(key)
        self._pmgr = pmgr
        self._dbid = DatabaseID.DatabaseID(javaDbId)

    def keys(self):
        return map(lambda x : str(x.getCompositeName()),
                   self._pmgr.getAvailableParms(self._dbid.toJavaObj()))

    def __getitem__(self, key):
        #pid = AFPS.ParmID_string(key + ":" + str(self._dbid))
        pid = ParmID(key + ":" + str(self._dbid))
        return DBSSWE(self._pmgr.getParm(pid))


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
        return map(lambda x : str(x), availDbs)

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
        return rs.getGrid().__numpy__[0]
    
    def getOpMode(self):
        return self._dataMgr.getOpMode().name()

    def getTopo(self):
        return self._tmgr.getCompositeTopo().getScalarGrid().__numpy__[0]
