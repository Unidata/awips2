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
##
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Aug 12, 2013    2162          dgilling       Add performance logging for
#                                                 query evaluation.
########################################################################

import numpy, copy, string, time, re, sys

from com.raytheon.uf.common.status import PerformanceStatus
from com.raytheon.uf.common.time.util import TimeUtil


PERF_LOG = PerformanceStatus.getHandler("GFE:");


class Query:
    def mask(self, wx, query, isreg=0):
        # Returns a numeric mask i.e. a grid of 0's and 1's
        #  where the value is 1 if the given query succeeds
        # Arguments:
        #  wx -- a 2-tuple:
        #    wxValues : numerical grid of byte values
        #    keys : list of "ugly strings" where the index of
        #      the ugly string corresponds to the byte value in
        #      the wxValues grid.
        #  query -- a text string representing a query
        #  isreg -- if 1, the query is treated as a regular expression
        #           otherwise as a literal string
        # Examples:
        #  # Here we want to treat the query as a regular expression
        #  PoP = where(self.wxMask(wxTuple, "^Chc:", 1), maximum(40, PoP), PoP)
        #  # Here we want to treat the query as a literal
        #  PoP = where(self.wxMask(wxTuple, ":L:") maximum(5, PoP), PoP)
        #
        rv = numpy.zeros(wx[0].shape, dtype=bool)
        if not isreg:
            for i in xrange(len(wx[1])):
                #if fnmatch.fnmatchcase(wx[1][i], query):
                if string.find(wx[1][i],query) >=0:
                    rv = numpy.logical_or(rv, numpy.equal(wx[0], i))
        else:
            r = re.compile(query)
            for i in xrange(len(wx[1])):
                m = r.match(wx[1][i])
                if m is not None:
                    rv = numpy.logical_or(rv, numpy.equal(wx[0], i))
        return rv

    # Helper For wxcontains checks a subkey field
    def _cf(self, f, lst):
        return len(lst) == 0 or f in lst

    def wxcontains(self, wx, cov=[], type=[], inten=[], vis=[], att=[]):
        rv = numpy.zeros(wx[0].shape, dtype=bool)
        key = wx[1]
        for index in xrange(len(key)):
            skeys = string.split(key[index], '^')
            for sk in skeys:
                c, t, i, v, a = string.split(sk, ":")
                if self._cf(c, cov) and self._cf(t, type) \
                   and self._cf(i, inten) and self._cf(v, vis) \
                   and self._cf(a, att):
                    rv = numpy.logical_or(rv, numpy.equal(wx[0], index))
                    break
        return rv

    def contains(self, wx, keys, delim='^'):
        rv = numpy.zeros(wx[0].shape, dtype=bool)
        for i in xrange(len(wx[1])):
            for k in string.split(wx[1][i], delim):
                if k in keys:
                    rv = numpy.logical_or(rv, numpy.equal(wx[0], i))
                    break
        return rv

    def __init__(self, client):
        self._client = client
        opmode = client.getOpMode()
        if opmode == "PRACTICE":
            fcst = filter(lambda x: string.find(x, "_Prac_Fcst_") != -1,
                      self._client.keys())
        elif opmode == "TEST":
            fcst = filter(lambda x: string.find(x, "_Test_Fcst_") != -1,
                      self._client.keys())
        else:
            fcst = filter(lambda x: string.find(x, "__Fcst_") != -1,
                      self._client.keys())
        if len(fcst) == 0:
            self._fcst = {}
        else:
            self._fcst = self._client[fcst[0]]
        self._time = time.time()

    def eval(self, queryStr):
        timer = TimeUtil.getTimer()
        timer.start()
        co, glob, loc = self.getEval(queryStr)
        area = eval(co, glob, loc)
        timer.stop()
        PERF_LOG.logDuration("Executing edit area query [" + queryStr + "]", timer.getElapsedTime())
        return area

    def getTime(self):
        return self._time

    def setTime(self, time):
        self._time = time

    def getCode(self, str):
        co = compile(str, "<query>", "eval")
        return co, co.co_names

    def getGrid(self, we):
        times = we.keys()
        times = filter(lambda x,t=self._time : t >= x[0] and t < x[1], times)
        try:
            return we[times[0]]
        except IndexError:
            raise IndexError("NO GRID FOR: " + we.parmName + " at time: "
                             + time.ctime(self.getTime()))

    def getParm(self, name):
        # Attempt to find a parm in all of the databases.
        sid = self._client.siteIDs[0]
        dbs = self._client.keys()
        dbs.sort()
        dbs.reverse()
        for db in dbs:
            for p in self._client[db].keys():
                # exact match
                ename = p + "_" + db
                if ename == name:
                    return self._client[db][p]
                # SITEID_GRID_ omitted
                gname = p + '_' + re.sub(sid + "_GRID_", "", db)
                if gname == name:
                    return self._client[db][p]
                # Time specifier omited (use most recent
                elif re.sub(r"_\d{8}_\d{4}\Z", "", ename) == name:
                    return self._client[db][p]
                # Both GRID and time omited
                elif re.sub(r"_\d{8}_\d{4}\Z", "", gname) == name:
                    return self._client[db][p]
        return None


    def getLocals(self, names):
        rval = {}
        rval['mask'] = self.mask
        rval['wxcontains'] = self.wxcontains
        rval['contains'] = self.contains
        fcstParms = self._fcst.keys()
        editAreas = self._client.editAreaNames()
        timer = TimeUtil.getTimer()
        for name in names:
            timer.reset()
            timer.start()
            if name in fcstParms:
                rval[name] = self.getGrid(self._fcst[name])
                timer.stop()
                PERF_LOG.logDuration("Retrieving grid for Parm [" + name + "]", timer.getElapsedTime())
            elif name + "_SFC" in fcstParms:
                rval[name] = self.getGrid(self._fcst[name + "_SFC"])
                timer.stop()
                PERF_LOG.logDuration("Retrieving grid for Parm [" + name + "_SFC]", timer.getElapsedTime())
            elif name in editAreas:
                ea = self._client.getEditArea(name)
                if type(ea) == type(""):
                    ea = self.eval(ea)
                rval[name] = ea
                timer.stop()
                PERF_LOG.logDuration("Retrieving edit area [" + name + "]", timer.getElapsedTime())
            elif string.lower(name) == 'topo':
                rval[name] = self._client.getTopo()
                timer.stop()
                PERF_LOG.logDuration("Retrieving topo grid", timer.getElapsedTime())
            else:
                tmp = self.getParm(name)
                if tmp is not None:
                    rval[name] = self.getGrid(tmp)
                timer.stop()
                PERF_LOG.logDuration("Retrieving grid for Parm [" + name + "]", timer.getElapsedTime())
        return rval

    def willRecurse(self, name, str):
        co, names = self.getCode(str)
        editAreas = filter(lambda x,y=names: x in y,
                           self._client.editAreaNames())
        if name in editAreas:
            return 1
        return 0

    def getEval(self, queryStr):
        timer = TimeUtil.getTimer()
        timer.start()
        co, names = self.getCode(queryStr)
        timer.stop()
        PERF_LOG.logDuration("Compiling edit area query [" + queryStr + "]", timer.getElapsedTime())
        
        timer.reset()
        timer.start()
        loc = self.getLocals(names)
        timer.stop()
        PERF_LOG.logDuration("Retrieving local variables for edit area query [" + queryStr + "]", timer.getElapsedTime())
        
        timer.reset()
        timer.start()
        glob = copy.copy(getattr(numpy, '__dict__'))
        timer.stop()
        PERF_LOG.logDuration("Creating global variables for edit area query [" + queryStr + "]", timer.getElapsedTime())
        
        return co, glob, loc
