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
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02/16/12        14439         jdynina        modified haines thresholds
#    02/16/12        13917         jdynina        merged in changes from TRAC ticket 11391
#    07/25/12        #957          dgilling       implement edit areas as args to calc methods.
#    10/05/12        15158         ryu            add Forecaster.getDb()
#    04/04/13        #1787         randerso       fix validTime check to work with accumulative parms
#                                                 fix logging so you can actually determine why 
#                                                 a smartInit is not calculating a parameter
# 
##
import string, sys, re, time, types, getopt, fnmatch, LogStream, DatabaseID, JUtil, AbsTime, TimeRange
import SmartInitParams
from numpy import *
pytime = time

import RollBackImporter
rollbackImporter = RollBackImporter.RollBackImporter()
    

#--------------------------------------------------------------------------
# Main program that calls model-specific algorithms to generate ifp grids.
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#  Definition for model database class.
#--------------------------------------------------------------------------
class MDB:
    def __init__(self, dblist):
        self.__dbs = dblist

    def keys(self):
        rval = []
        for db in self.__dbs:
            rval = rval + db.keys()
        return rval

    def __getitem__(self, key):
        for db in self.__dbs:
            if key in db.keys():
                return db[key]
        raise KeyError(key + " not found")

    def __getattr__(self, name):
        for db in self.__dbs:
            if hasattr(db, name):
                return getattr(db, name)
        raise AttributeError(name + " not found")

    def getKeys(self):
        rval = []
        for db in self.__dbs:
            rval = rval + JUtil.javaStringListToPylist(db.getKeys())
        return rval

    def getItem(self, key):
        for db in self.__dbs:
            keyLow = key.lower()
            for k in JUtil.javaStringListToPylist(db.getKeys()):
                if k.lower() == keyLow: 
                    return db.getItem(key)

        #for db in self.__dbs:
        #    try:
        #        return db.getItem(key)
        #    except Exception, e:
        #        # njensen: changed this to log an error to try and determine a deeper issue
        #        # pass
        #        LogStream.logProblem(key + " not found: " + str(e))

        raise KeyError(key + " not found")

    def getModelTime(self):
        if len(self.__dbs) > 0:
            return self.__dbs[0].getModelTime()
        return 0

#-------------------------------------------------------------------------
# Utilities
#-------------------------------------------------------------------------
class GridUtilities:
    pres = []

    def squishZ(self, cube, levels):
        buckets = cube[0:levels]
        inc = cube.shape[0] / float(levels)
        start = 0
        end = int(inc)
        for i in xrange(int(levels)):
            buckets[i] = add.reduce(cube[start:end]) / (end - start)
            start = end
            end = int(inc * (i + 2))
        return buckets

    def skyFromRH(self, rh_c, gh_c, topo, p_SFC):
        tmpP_SFC = p_SFC.copy()
        tmpP_SFC /= 100.0  # convert surfp to milibars
        x = 78             # delta x (85km - 850km)

        # Make a pressure cube
        pmb = ones_like(gh_c)
        for i in xrange(gh_c.shape[0]):
            pmb[i] = self.pres[i]

        pp = pmb / tmpP_SFC
        pp = clip(pp, 0.1, 1.0)
        fmax = 78.0 + x / 15.5
        f100 = where(pp < 0.7, fmax * (pp - 0.1)/0.6,
                     30.0 + (1.0 - pp) * (fmax - 30.0) / 0.3)
        c = 0.196 + (.76 - x / 2834.0) * (1.0 - pp)
        c = (rh_c/100.0 - 1.0) / c
        c = exp(c)
        f = minimum(f100 * c, 100.0)
        f[less(gh_c, topo)] = 0.0
        f = self.squishZ(f, 5)
        f[4] *= 0.25
        f /= 100.0
        sky = f[0]
        for i in xrange(1, f.shape[0]):
            sky = sky + f[i] - sky * f[i]
        return sky * 100.0

    #=======================================================================
    #
    #  Calculate Wetbulb (C) based on temperature (C) and RH (%)
    #  (all algorithms straight out of GEMPAK - converted to numeric python)
    #
    def Wetbulb(self, tc, rh, pres):
       dpc=self.RHDP(tc,rh)
       thte=self.THTE(pres,tc,dpc)
       dpc = None
       wetbk=self.TMST(thte,pres,0)
       return wetbk-273.15
    #=======================================================================
    #
    #  Calculate parcel temp (K) given thetae (K) pressure (mb) and guess
    #                                  temperature (K)
    #
    def TMST(self, thte, pres, tguess):
       tg=ones(thte.shape)*tguess
       teclip=clip(thte-270.0,0.0,5000.0)
       #
       #  if guess temp is 0 - make a more reasonable guess
       #
       tgnu=where(less(tg,1),(thte-0.5*teclip**1.05)*(pres/1000.0)**0.2,tg)\
             -273.15
       tg = teclip = None
       epsi=0.01
       #
       #  Correct the temp up to 100 times.  Typically this takes
       #  less than 5 iterations
       #
       for i in range(1,100):
           i = i # eliminate warning in pychecker
           tgnup=tgnu+1.0
           tenu=self.THTE(pres,tgnu,tgnu)
           tenup=self.THTE(pres,tgnup,tgnup)
           cor=(thte-tenu)/(tenup-tenu)
           tenu = tenup = tgnup = None
           tgnu=tgnu+cor
           #
           #  get the maximum correction we made this time
           #  and if it is less than epsi - then we are close
           #  enough to stop.
           #
           mcor=maximum.reduce(maximum.reduce(maximum.reduce(abs(cor))))
           cor = None
           if (mcor<epsi):
              return (tgnu+273.15)
       return tgnu+273.15
    #=======================================================================
    #
    #  Calculate Dewpoint (C) based on Temperature (C) and RH (%)
    #
    def RHDP(self, tc, rh):
       log1=log(6.112)
       vaps=self.VAPR(tc)
       lvapr=log(rh*vaps/100.0)
       dpc=((243.5*(log1-lvapr))/(lvapr-log1-17.67))
       return dpc
    #=======================================================================
    #
    #  Calculate Theta-E given Pressure (mb) Temperature (C) and Dewpoint (C)
    #
    def THTE(self, pres, tc, dpc):
       rmix=self.MIXR(dpc,pres)
       tk=tc+273.15
       te=(2.0/7.0)*(1.0-(0.00028*rmix))
       thtam=tk*(1000.0/pres)**te
       tk = te = None
       tlcl=self.TLCL(tc,dpc)
       te=((3.376/tlcl)-0.00254)*(rmix*(1.0+0.00081*rmix))
       return (thtam*exp(te))
    #=======================================================================
    #
    #  Calculate temperature at LCL (K) given Temperature (C) and Dewpoint (C)
    #
    def TLCL(self,tc,dpc):
       tk=tc+273.15
       dk=dpc+273.15
       return((1.0/(1.0/(dk-56.0)+log(tk/dk)/800.0))+56.0)
    #=======================================================================
    #
    #  Calculate Mixing Ratio (g/kg) given Dewpoint (C) and pressure (mb)
    #
    def MIXR(self, dpc, pres):
       vapr=self.VAPR(dpc)
       corr=(1.001+((pres-100.)/900.)*0.0034)
       te=corr*vapr
       corr = vapr = None
       mixr=0.62197*(te/(pres-te))*1000.0
       return mixr
    #=======================================================================
    #
    #  Calculate Vapor Pressure (mb) from Dewpoint (C)
    #  or Saturation Vapor Pressure (mb) from Temperature (C)
    #
    def VAPR(self, tc):
       vapr=6.112*(exp((17.67*tc)/(tc+243.5)))
       return vapr

    #=======================================================================
    #
    #  Calculate Haines Index
    #  type is "LOW", "MEDIUM", "HIGH"
    #  NOTE, the default haines index calcaulation is defined by:
    #  self.whichHainesIndex, which can be set to "LOW", "MEDIUM", "HIGH".
    #
    #=======================================================================
    def hainesIndex(self, type, t_c, rh_c):
        dict = {}
        dict['LOW'] = {'t1Level': 950, 't2Level': 850, 'mLevel': 850,
           'stabThresh': [4, 8], 'moiThresh': [6, 10]}
        dict['MEDIUM'] = {'t1Level': 850, 't2Level': 700, 'mLevel': 850,
           'stabThresh': [6, 11], 'moiThresh': [6, 13]}
        dict['HIGH'] = {'t1Level': 700, 't2Level': 500, 'mLevel': 700,
           'stabThresh': [18, 22], 'moiThresh': [15, 21]}
        dd = dict[type]   # proper dictionary for the level

        # get the needed data, calc dewpoint
        pres = self.pres
        t1 = t_c[pres.index(dd['t1Level'])]  #  t1 level
        t2 = t_c[pres.index(dd['t2Level'])]  #  t2 level
        tMois = t_c[pres.index(dd['mLevel'])] - 273.15  #  mLevel t , in C.
        rhMois = rh_c[pres.index(dd['mLevel'])]  / 100.0  # mLevel rh
        rhMois[less_equal(rhMois, 0)] = 0.00001
        a = log10(rhMois) / 7.5 + (tMois / (tMois + 237.3))
        dpMois = (a * 237.3) / (1.0 - a)

        hainesT = t1 - t2
        hainesM = tMois - dpMois

        # now make the categories
        mask3 = greater_equal(hainesT, dd['stabThresh'][1])
        mask1 = less(hainesT, dd['stabThresh'][0])
        hainesT = where(mask3, 3, where(mask1, 1, 2))

        mask3 = greater_equal(hainesM, dd['moiThresh'][1])
        mask1 = less(hainesM, dd['moiThresh'][0])
        hainesM = where(mask3, 3, where(mask1, 1, 2))

        return hainesT + hainesM




#-------------------------------------------------------------------------
# Weather Element calculations
#-------------------------------------------------------------------------
class Forecaster(GridUtilities):
    def __init__(self, srcName, dstName=None):
        self._srcName = srcName
        self._dstName = dstName        
        self._ndbkeys = None
        self.__dbParms = None
        #host, port = self._getServer()
        #Options = getOpts()
        #if Options is not None and Options.has_key('userID'):
        #    self._client = ifpc.IFPC(host, port, Options['userID'])
        #else:
        #    self._client = ifpc.IFPC(host, port)        
        self.whichHainesIndex = "HIGH"   # or "LOW", or "MEDIUM"

        if self._srcName is not None:
            self._srcModels = [srcName]
        else:
            self._srcModels = []

    def addSources(self, models=[]):
        self._srcModels += list(models)

    #--------------------------------------------------------------------------
    #  Constructor for the Forecaster class
    #--------------------------------------------------------------------------
    def __init(self):
        from com.raytheon.edex.plugin.gfe.smartinit import InitClient
        self._client = InitClient(self.__dbName)
        if self._srcModels is not None:
            self.__srcdb, self.__newdb = self._getLatest(self._client,
                                                         self._srcModels,
                                                         self._dstName)
        if self.srcdb() is None:
            msg = "No databases for " + self._srcName
            LogStream.logProblem(msg)
            return
            #sys.exit(1)
                
        self.__topo = self.getTopo() * .3048
        srcdbkeys = self.srcdb().getKeys()
        if "staticTopo_Dflt" in srcdbkeys:
            try:
                self.__stopo = self.srcdb().getItem("staticTopo_Dflt")
                stopotr = self.__stopo.getKeys().get(0)
                self.__stopo = self.__stopo.getItem(stopotr).__numpy__[0]
            except:
                self.__stopo = None
        else:
            self.__stopo = None
        
        # TODO: this is a work around to keep smart init running 
        # until we get the staticTopo_Dflt parameter populated       
#        if self.__stopo is None:
#            LogStream.logProblem("staticTopo not available, using topo")
#            self.__stopo = self.__topo
        
        self._editAreas = self._client.getEditAreaNames()
        self._empty = self.__topo * 0
        self._minus = self._empty - 1

    #--------------------------------------------------------------------------
    #  Returns a string that corresponds to the specified time range.
    #--------------------------------------------------------------------------
    def _timeRangeStr(self, tr):
        if tr is None:
            return "<Null TR>"
        format = "%Y%m%d_%H%M"
        return "(" + pytime.strftime(format, pytime.gmtime(tr[0])) + ", " +\
               pytime.strftime(format, pytime.gmtime(tr[1])) + ")"

    #--------------------------------------------------------------------------
    # Interpolates a new value given two x, two y, and a new x value.
    #--------------------------------------------------------------------------
    def linear(self, xmin, xmax, ymin, ymax, we):
        m = (ymax - ymin) / (xmax - xmin + .0000001)
        b = ymin - m * xmin
        return m * we + b

    #--------------------------------------------------------------------------
    #  Converts the value from meters per second to knots.
    #--------------------------------------------------------------------------
    def convertMsecToKts(self, value_Msec):
        # Convert from meters/sec to Kts
        return value_Msec * 1.944

    #--------------------------------------------------------------------------
    #  Converts the value from feet to meters
    #--------------------------------------------------------------------------
    def convertFtToM(self, value_Ft):
        # Convert the value in Feet to Meters
        return value_Ft/3.28084

    #--------------------------------------------------------------------------
    # Converts the value from Fahrenheit to Kelvin
    #--------------------------------------------------------------------------
    def FtoK(self, t):
        return (t + 459.67) / 1.8
    #--------------------------------------------------------------------------
    # Converts the value from Fahrenheit to Kelvin
    #--------------------------------------------------------------------------
    def convertFtoK(self, t_F):
        return self.FtoK(t_F)

    #--------------------------------------------------------------------------
    # Converts the value from Kelvin to Fahrenheit
    #--------------------------------------------------------------------------
    def KtoF(self, t):
        return t * 1.8 - 459.67
    #--------------------------------------------------------------------------
    # Converts the value from Kelvin to Fahrenheit
    #--------------------------------------------------------------------------
    def convertKtoF(self, t):
        return self.KtoF(t)

    #--------------------------------------------------------------------------
    # Calculates the saturation vapor pressure give the temperature in K
    #--------------------------------------------------------------------------
    def esat(self, temp):
        val = 26.660820 - 0.0091379024 * temp - 6106.3960 / temp
        val[greater(val, 100)] = 100
        return exp(val)

    #--------------------------------------------------------------------------
    # Calculates the potential temp. given the temperature and pressure
    # potential temp (p = milibars, t = kelvin)
    #--------------------------------------------------------------------------
    def ptemp(self, t, p):
        p = clip(p, .00001, 1500)
        return t * pow((1000 / p), 0.286)

    # Returns the "area" (JKg-1)
    def getArea(self, pbot, tbot, ptop, ttop):
        logV = self.ptemp(ttop, ptop) / (self.ptemp(tbot, pbot) + 0.00001)
        logV = clip(logV, 0.0001, logV)
        tavg = (ttop + tbot) / 2.0
        #area = abs(1003.5 * tavg * log(logV))
        area = abs(1.0035 * tavg * log(logV))
        return where(less(tavg, 273.15), area * -1, area)

    # Returns two areas (from getArea).  The
    # second area is valid if we cross the freezing layer
    # and is indicated by the cross mask.
    def getAreas(self, pbot, tbot, ptop, ttop):
        maxm = maximum(tbot, ttop)
        minm = minimum(tbot, ttop)
        freeze = self._empty + 273.15
        crosses = logical_and(less(minm, freeze), greater(maxm, freeze))
        crossp = self.linear(pbot, ptop, tbot, ttop, freeze)
        crosst = freeze
        crossp = where(crosses, crossp, ptop)
        crosst = where(crosses, crosst, ttop)
        a1 = self.getArea(pbot, tbot, crossp, crosst)
        a2 = self.getArea(crossp, crosst, ptop, ttop)
        return a1, a2, crosses

    #--------------------------------------------------------------------------
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
    #--------------------------------------------------------------------------
    def wxMask(self, wx, query, isreg=0):
        rv = zeros(wx[0].shape)
        if not isreg:
            for i in xrange(len(wx[1])):
                #if fnmatch.fnmatchcase(wx[1][i], query):
                if string.find(wx[1][i],query) >=0:
                    rv = logical_or(rv, equal(wx[0], i))
        else:
            r = re.compile(query)
            for i in xrange(len(wx[1])):
                m = r.match(wx[1][i])
                if m is not None:
                    rv = logical_or(rv, equal(wx[0], i))
        return rv

    #--------------------------------------------------------------------------
    # Returns the byte value that corresponds to the
    #   given ugly string. It will add a new key if a new ugly
    #   string is requested.
    # Arguments:
    #   uglyStr: a string representing a weather value
    #   keys: a list of ugly strings.
    #     A Wx argument represents a 2-tuple:
    #       wxValues : numerical grid of byte values
    #       keys : list of "ugly strings" where the index of
    #        the ugly string corresponds to the byte value in the wxValues grid.
    #     For example, if our keys are:
    #       "Sct:RW:-:<NoVis>:"
    #       "Chc:T:-:<NoVis>:"
    #       "Chc:SW:-:<NoVis>:"
    #    Then, the wxValues grid will have byte values of 0 where
    #    there is "Sct:RW:-:<NoVis>:", 1 where there is "Chc:T:-:<NoVis>:"
    #    and 2 where there is "Chc:SW:-:<NoVis>:"
    #
    #--------------------------------------------------------------------------
    def getIndex(self, uglyStr, keys):
        if uglyStr == "":
            uglyStr = "<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
        for str in keys:
            if uglyStr == str:
                return keys.index(uglyStr)
        keys.append(uglyStr)
        return len(keys)-1

    #--------------------------------------------------------------------------
    #  Place holder for levels function to be implemented by derived classes.
    #--------------------------------------------------------------------------
    def levels(self):
        return []

    #--------------------------------------------------------------------------
    # Returns the topography grid.
    #--------------------------------------------------------------------------
    def getTopo(self):
        topo = self._client.getTopo()        
        topo = topo.__numpy__
        return topo[0]        

    #--------------------------------------------------------------------------
    # Returns a dictionary of magical values that will be used in other
    # functions.
    #--------------------------------------------------------------------------
    def magicArgs(self):
        rval = { "topo" : (self.__topo, (0, sys.maxint)),
                 "stopo" : (self.__stopo, (0, sys.maxint)),
                 "ctime" : (None, (0, sys.maxint)),
                 "stime" : (None, (0, sys.maxint)),
                 "mtime" : (None, (0, sys.maxint))}
        for i in self._editAreas:
            rval[i] = (None, (0, sys.maxint))
        return rval

    #--------------------------------------------------------------------------
    # Runs the main program
    #--------------------------------------------------------------------------
    def run(self):
        dbName = SmartInitParams.params['dbName']
        validTime = SmartInitParams.params['validTime']   
        
        dbInfo = dbName.split(':')
        self.__dbName = dbInfo[0]

        start = time.time()
        self.__init()
        msgDest = "Destination database:" + self.newdb().getModelIdentifier()

        if validTime is not None:
            msgDest = msgDest + ", validTime " + pytime.strftime("%Y%m%d_%H%M", pytime.gmtime(validTime.getTime() / 1000))

        LogStream.logEvent(msgDest)
        self.__newwes = {}
        self.__srcwes = {}
        self._ifpio = IFPIO(self.srcdb(), self.newdb())
        self._ifpio.setLevels(self.levels())
        methods = self.__getMethods()
        times = self.__sortTimes(methods, validTime)        
        tr, numGrids = self.__process(methods, times, int(dbInfo[1]))
        stop = time.time()
        msgTime = "Elapsed time: " + ("%-.1f" % (stop - start)) + "sec."
        LogStream.logEvent(msgTime)
        #LogStream.logEvent("Network stats: ", self._client.getStats())
        self._announce(self.newdb(), tr, numGrids)

    #--------------------------------------------------------------------------
    # Sends a message to the GFE stating that a database is complete.
    #--------------------------------------------------------------------------
    def _announce(self, db, tr, numGrids):
        if numGrids == 0:
            return
        dbTime = db.getModelTime()
        if dbTime is not None:
            modelTime = AbsTime.AbsTime(db.getModelTime())
            modelTime = modelTime.unixTime()
        else:
            modelTime = 0        
        modelIdentifier = db.getShortModelIdentifier()

        if modelTime != 0:
            trRel = ((tr[0]-modelTime)/3600, ((tr[1]-modelTime)/3600)-1)
            msg = "Model: " + modelIdentifier + ' ' +\
              `trRel[0]` + '-' + `trRel[1]` + 'h #Grids=' + `numGrids`
        else:
            msg = "Model: " + modelIdentifier \
              + ' #Grids=' + `numGrids`
        self._client.sendUserMessage(msg, "SMARTINIT")

    #--------------------------------------------------------------------------
    # Returns the IFPDB object for the given db
    #--------------------------------------------------------------------------
    def getDb(self, dbString):
        from com.raytheon.edex.plugin.gfe.smartinit import IFPDB
        return IFPDB(dbString)

    #--------------------------------------------------------------------------
    # Returns the source and destination databases, given the srcName.
    #--------------------------------------------------------------------------
    def _getLatest(self, client, srcNames, fcstName=None):
        # ryu: Added/modified code to allow multiple sources. The srcdb is
        # now an MDB. This is needed for (AK)NAM40 init, which sources
        # from both NAM40 and NAM20.

        srcdbs = []
        modelName = DatabaseID.databaseID(self.__dbName).modelName()

        if len(srcNames) == 0:
            srcNames = [modelName]

        for src in srcNames:
            # source model at same model time
            fullDBName = self.__dbName.replace(modelName, src)
            db = self.getDb(fullDBName)
            if db.getKeys().size() == 0:
                LogStream.logEvent("Source database " + fullDBName + \
                                   " is empty.")
            else:
                srcdbs.append(db)
                
        srcdb = MDB(srcdbs)

        # I (njensen) removed most of what was here.  It was looking at
        # the available D2D netcdf data, and then forming a GFE db id
        # from that for the target.  Instead I'm just passing in 
        # the target from Java.
                
        newdb = self.__dbName.replace("D2D", "")
        if fcstName and fcstName != modelName:
            newdb = newdb.replace(modelName, fcstName)

        # create db if not singleton db
        singletons = client.getSingletonIDs()
        singletonNeeded = 0
        singletonsize = singletons.size()
        for i in range(singletonsize):
            d = singletons.get(i)
            s = DatabaseID.DatabaseID(d)
            if newdb[:-13] == s.modelIdentifier()[:-13]:
                singletonNeeded = 1
                break
        if singletonNeeded:
            newdb = newdb[:-13] + '00000000_0000'
        else:
            client.createDB(newdb)

        newdb = self.getDb(newdb)    

        return srcdb, newdb

    #--------------------------------------------------------------------------
    #  Returns the target database
    #--------------------------------------------------------------------------
    def newdb(self):
        return self.__newdb

    #--------------------------------------------------------------------------
    #  Returns the source database.
    #--------------------------------------------------------------------------
    def srcdb(self):
        return self.__srcdb

    #--------------------------------------------------------------------------
    #  Convert magnitude and direction to u and v components.
    #--------------------------------------------------------------------------
    def _getUV(self, mag, dir):
        rad = dir * 0.0174
        u = mag * sin(rad)
        v = mag * cos(rad)
        return (u, v)

    #--------------------------------------------------------------------------
    #  Returns a mask where points are set when the specified query is true.
    #--------------------------------------------------------------------------
    def _wxMask(self, wx, query, isreg=0):
        rv = zeros(wx[0].shape)
        if not isreg:
            for i in xrange(len(wx[1])):
                if fnmatch.fnmatchcase(wx[1][i], query):
                    rv = logical_or(rv, equal(wx[0], i))
        else:
            r = re.compile(query)
            for i in xrange(len(wx[1])):
                m = r.match(wx[1][i])
                if m is not None:
                    rv = logical_or(rv, equal(wx[0], i))
        return rv

    #--------------------------------------------------------------------------
    # Returns the magnitude and direction from u and v components.
    #--------------------------------------------------------------------------
    def _getMD(self, u, v):
        mag = hypot(u, v)
        dir = degrees(arctan2(u, v))
        dir[less(dir, 0)] += 360
        return (mag, dir)

    #--------------------------------------------------------------------------
    #  Returns true if the specified time is found within the specified
    #  timeRange.
    #--------------------------------------------------------------------------
    def _contains(self, timerange, time):
        if timerange[1] - timerange[0]:
            return ((time >= timerange[0]) and (time < timerange[1]))
        return time == timerange[0]

    #--------------------------------------------------------------------------
    #  Returns true if the two timeRanges overlap (share a common time period).
    #--------------------------------------------------------------------------
    def _overlaps(self, tr1, tr2):        
        if self._contains(tr2, tr1[0]) or self._contains(tr1, tr2[0]):
            return 1
        return 0

    #--------------------------------------------------------------------------
    #  Returns the overlapping timeRange between two timeRanges (if any).
    #--------------------------------------------------------------------------
    def _intersect(self, t1, t2):
        if self._overlaps(t1, t2):
            return (max(t1[0], t2[0]), min(t1[1], t2[1]))
        return None

    #--------------------------------------------------------------------------
    #  Returns the "calc" methods for the specified Forecaster object.
    #--------------------------------------------------------------------------
    def __getObjMethods(self, obj):
        rval = []
        for o in obj.__bases__:
            rval += self.__getObjMethods(o)

        magicArgs = self.magicArgs().keys()
        for fn in filter(lambda x : x[:4] == "calc", dir(obj)):
            mthd = eval("self." + fn)
            co = mthd.im_func.func_code
            args = co.co_varnames[1:co.co_argcount]
            fargs = []
            for a in args:
                if a not in magicArgs and string.find(a, '_') == -1:
                    a += "_SFC"
                fargs.append(a)
            wename = fn[4:]
            if string.find(wename, "_") != -1:
                wenameLevel = wename
            else:
                wenameLevel = wename + "_SFC"
            #if wenameLevel not in self.newdb().keys():            
            if wenameLevel not in JUtil.javaStringListToPylist(self.newdb().getKeys()):
                msg = wenameLevel + " not in " + \
                      self.newdb().getModelIdentifier() + " " + "SKIPPING"
                LogStream.logProblem(msg)
                continue
            rval = filter(lambda x,y=wenameLevel : x[0] != y, rval)
            rval.append((wenameLevel, mthd, fargs))
        return rval        

    #--------------------------------------------------------------------------
    #  Gets and returns a list of dependencies.
    #--------------------------------------------------------------------------
    def __getdeps(self, m, lst):
        rval = []
        for i in m[2]:
            if i != m[0]:
                for j in lst:
                    if j[0] == i:
                        rval = rval + self.__getdeps(j, lst)
            else:
                if len(m[2]) == 1:
                    raise ValueError("calc" + i
                                     + " must depend on more than itself")
        lst.remove(m)
        return rval + [m]

    #--------------------------------------------------------------------------
    #  Returns this objects methods
    #--------------------------------------------------------------------------
    def __getMethods(self):
        rval = []
        methods = self.__getObjMethods(self.__class__)
        while len(methods):
            rval += self.__getdeps(methods[0], methods)                
        return rval

    def __request(self, db, pname, time):
        if pname[-2:] == "_c":                    
            time = self.__getSrcWE(
                pname[:-2] + "_MB500", 0).getTimeRange(time[0])            
            rval = (pname[:-2], time, 1)
        else:
            time = self.__getSrcWE(pname, 0).getTimeRange(time[0])
            rval = (pname, time)
        return rval

    def __unpackParm(self, parm):
        if parm[-2:] == "_c":
            list = []
            base = parm[:-2]
            if len(self.levels()) == 0:
                raise Exception("Request for " + parm
                                + " and levels() is empty!")
            for l in self._ifpio.levels():
                list.append(base + "_" + l)
            return list
        return [parm]

    #--------------------------------------------------------------------------
    # Internal function that returns the time periods shared by tr and times.
    #--------------------------------------------------------------------------
    def __compTimes(self, tr, times):
        for time in times:
            if len(time) == 0:
                return []   

        rval = []
        if len(times) == 1:
            if tr is None:
                return times[0]
            for time in times[0]:
                inter = self._intersect(tr, time)
                if inter is not None:
                    rval.append(inter)
            return rval
        else:
            mtimes = times[-1:][0]
            ntimes = times[:-1]
            for time in mtimes:
                if tr is not None:
                    time = self._intersect(time, tr)
                if time is not None:
                    trv = self.__compTimes(time, ntimes)
                    for r in trv:
                        if r not in rval:
                            rval.append(r)
        return rval

    #--------------------------------------------------------------------------
    # Internal function that sorts times.
    #--------------------------------------------------------------------------
    def __sortTimes(self, methods, validTime):
        rval = []
        calced = []
        for we, mthd, args in methods:
#            LogStream.logEvent("Evaluating times for calc"+we)
            calced.append(we)
            args = filter(lambda x, ma=self.magicArgs().keys() + [we]:
                          x not in ma, args)
            nargs = []
            for a in args:
                nargs = nargs + self.__unpackParm(a)

            ttimes = [] 
            for p in nargs:
                # p is an arg, e.g. gh_MB900
                try:
                    ttimes.append(rval[calced.index(p)])
                except:
                    # get all available times for that param
                    try:
                        ranges = self.__getSrcWE(p).getKeys()
                        size = ranges.size()
                    except:
                        size = 0

                    pylist = []
                    for i in range(size):
                       jtr = ranges.get(i)
                       valid = False
                       
                       if validTime is None:
                           valid = True
                       else:
                          # need check to be inclusive on both ends for methods that
                          # need both accumulative and non-accumulative parms
                          valid = validTime.getTime() >= jtr.getStart().getTime() and \
                                  validTime.getTime() <= jtr.getEnd().getTime()
                       
                       if valid:
                          timelist = TimeRange.encodeJavaTimeRange(jtr)        
                          pylist.append(timelist)

                    ttimes.append(pylist)
                    
#                msg = "Times available for " + p + " " + str(validTime) + ":\n"
#                timeList = ttimes[len(ttimes)-1]
#                for xtime in timeList:
#                    msg += '('                                                
#                    stime = time.gmtime(xtime[0])
#                    etime = time.gmtime(xtime[1])
#                    stime = time.strftime('%Y%m%d_%H%M', stime)
#                    etime = time.strftime('%Y%m%d_%H%M', etime)
#                    msg += stime + ", " + etime
#                    msg += ')\n'
#                LogStream.logEvent(msg)                    

            # compare the times of each parm and find where they match up
            times = self.__compTimes(None, ttimes)
#            LogStream.logEvent("nargs:",nargs)
#            LogStream.logEvent("ttimes:",ttimes)
#            LogStream.logEvent("times:",times)

            hadDataButSkipped = {}
            for i in range(len(ttimes)):                
                timeList = ttimes[i]
                parmName = nargs[i]
                for xtime in timeList:
                    if xtime not in times:
                        if hadDataButSkipped.has_key(xtime):
                            hadDataButSkipped[xtime].append(parmName)
                        else:
                            hadDataButSkipped[xtime] = [parmName]
#            LogStream.logEvent("hadDataButSkipped:",hadDataButSkipped)

            hadNoData = []            
            for i in range(len(nargs)):
                timeList = ttimes[i]
                parmName = nargs[i]
                if len(timeList) == 0:
                    hadNoData.append(parmName)
#            LogStream.logEvent("hadNoData:",hadNoData)

            missing = {}                                    
            for xtime in hadDataButSkipped:
                stime = time.gmtime(xtime[0])
                etime = time.gmtime(xtime[1])
                stime = time.strftime('%Y%m%d_%H%M', stime)
                etime = time.strftime('%Y%m%d_%H%M', etime)
                msg = stime + ", " + etime
                missing[msg] = []
                
                for parmName in nargs:
                    if not hadDataButSkipped[xtime].__contains__(parmName):
                        missing[msg].append(parmName)
                        
            if len(missing) == 0 and len(hadNoData) > 0:
                msg = ''
                if (validTime is not None):
                    vtime = validTime.getTime()/1000
                    vtime = time.gmtime(vtime)
                    msg = time.strftime('%Y%m%d_%H%M', vtime)
                missing[msg] = hadNoData
#            LogStream.logEvent("missing:",missing)

            if len(missing):              
                LogStream.logEvent("Skipping calc" + we + " for some times due to the following " +
                                   "missing data:", missing)
            # these become the times to run the method for
            rval.append(times)

        return rval

    #--------------------------------------------------------------------------
    # Returns a WeatherElement object given it's name.
    #--------------------------------------------------------------------------
    def __getSrcWE(self, wename, lock=1):
        return self._ifpio.getSrcWE(wename, lock)

    #--------------------------------------------------------------------------
    #  Returns a new weather element given the weName.
    #--------------------------------------------------------------------------
    def __getNewWE(self, wename, lock=1):
        return self._ifpio.getNewWE(wename, lock)

    def __recursiveArg(self, cache, arg, time):
        p = self.newdb().getItem(arg)
        #p = self.newdb()[arg + "_SFC"]
        tr = p.getTimeRange(time[0])
        pytr = TimeRange.encodeJavaTimeRange(tr)
        pkeys = TimeRange.javaTimeRangeListToPyList(p.getKeys())
        if  pytr in pkeys:            
            slice = p.getItem(tr)
            slice = slice.__numpy__
            if len(slice) == 1:
                slice = slice[0]
            elif len(slice) == 2 and type(slice[1]) is str:
                exec "slice[1] = " + slice[1]
            cache[arg] = (slice, pytr)
        else:            
            cache[arg] = (None, time)

    def __argFill(self, cache, method, time):
        we, mthd, args = method
        gargs = []
        if self._ndbkeys is None:
            self._ndbkeys = JUtil.javaStringListToPylist(self.newdb().getKeys())
        ndbkeys = self._ndbkeys        
        for arg in args:
            if arg in self._editAreas:
                if cache[arg][0] is None:
                    p = self.newdb().getItem(we)
                    ea = p.getEditArea(arg).__numpy__[0]
                    cache[arg] = (ea, (0, sys.maxint))
                gargs.append(cache[arg][0])
                continue
            if not cache.has_key(arg):                
                if arg in ndbkeys:
                    self.__recursiveArg(cache, arg, time)
                else:
                    val = self._ifpio.get(self.__request(self.srcdb(),
                                                         arg, time))
                    if arg[-2:] == "_c":
                        self.pres = val[0]
                        val = val[1]
                    cache[arg] = (val, time)
            else:
                if cache[arg][1] is not None and \
                   not self._overlaps(time, cache[arg][1]):                    
                    if arg in ndbkeys:
                        self.__recursiveArg(cache, arg, time)
                        val = cache[arg][0]
                    else:                        
                        val = self._ifpio.get(self.__request(self.srcdb(),
                                                             arg, time))                                        
                    if arg[-2:] == "_c":
                        self.pres = val[0]
                        val = val[1]
                    cache[arg] = (val, time)
            
            gargs.append(cache[arg][0])
        return gargs

    def __runMethod(self, method, time, cache):
        we, mthd, args = method
        
        if self.mostRecentCacheClear != time:
            self.mostRecentCacheClear = time            
            for key in cache.keys():
                cacheValue = cache[key]
                if len(cacheValue) == 2 and key.find('_') > -1:
                    # these are WeatherElements, if they are for time ranges that
                    # we've completed calculations for, immediately set them to
                    # None to free up the memory                    
                    if time[0] != cacheValue[1][0]:
                        cache[key] = (None, cacheValue[1])
            
        gargs = self.__argFill(cache, method, time)

        doStore = False
        if mthd.im_func is Forecaster.__exists.im_func:
            msg = "Get : " + we + " " + self._timeRangeStr(time)
            LogStream.logEvent(msg)            
        else:        
            doStore = True
            msg = "Calc : " + we + " " + self._timeRangeStr(time)
            LogStream.logEvent(msg)            
        
        try:
            rval = apply(mthd, tuple(gargs))            

            if type(rval) is not ndarray:
                if type(rval) is not tuple:
                    rval = rval.__numpy__
                    if len(rval) == 1:
                        rval = rval[0]
                    elif len(rval) == 2 and type(rval[1]) is str:
                        exec "rval[1] = " + rval[1]
            cache[we] = (rval, time)        
            if rval is not None and cache['mtime'][0] is not None and doStore:
                parm = self.__getNewWE(we)          
                self._ifpio.store(parm, cache['mtime'][0], cache[we][0])
        except:
            LogStream.logProblem("Error while running method " + str(we) +
                                 "\n" + LogStream.exc())
            cache[we] = (None, time)

    def __tsort(self, x, y):
        if x[1][0] < y[1][0]:
            return -1
        if x[1][0] > y[1][0]:
            return 1
        return x[2] - y[2]

    def __flattenTimes(self, methods, times):
        lst = []
        for i in xrange(len(methods)):
            for t in times[i]:
                lst.append((methods[i], t, i))
        lst.sort(self.__tsort)        
        return lst

    def __exists(self, mtime, wename):        
        #parm = self.__getNewWE(wename + "_SFC")        
        parm = self.__getNewWE(wename)        
        return parm.getItem(mtime)

    def __prune(self, lst):
        rval = []
        for m, t, i in lst:
            # m is (parmname_level, bound method calcParmname, argument list)
            # t is a time range (list of two times)
            # i is order?
            parm = self.__getNewWE(m[0])
            #parm = self.__getNewWE(m[0] + "_SFC")
            tr = TimeRange.encodeJavaTimeRange(parm.getTimeRange(t[0]))
            if tr is None:
                continue                        
            parmtr = TimeRange.javaTimeRangeListToPyList(parm.getKeys())
            if tr in parmtr:
                # Skip (maybe)
                for m2, t2, i2 in lst:
                    if m2[0] == m[0]:
                        continue
                    if m2[0] in m[2] and self._overlaps(t2, t):
                        parm2 = self.__getNewWE(m2[0])
                        #parm2 = self.__getNewWE(m2[0] + "_SFC")
                        tr2 = parm2.getTimeRange(t2[0])
                        tr2 = TimeRange.encodeJavaTimeRange(tr2)
                        parm2tr = TimeRange.javaTimeRangeListToPyList(parm2.getKeys())
                        if tr2 is not None and tr2 not in parm2tr:
                            f = ((m[0], self.__exists,
                                  ('mtime', 'wename')), t, i)
                            if f not in rval:
                                rval.append(f)
                                rval.append((m, t, i))
                                continue
                    if m[0] in m2[2] and self._overlaps(t2, tr):
                        parm2 = self.__getNewWE(m2[0])
                        #parm2 = self.__getNewWE(m2[0] + "_SFC")
                        tr2 = parm2.getTimeRange(t2[0])
                        tr2 = TimeRange.encodeJavaTimeRange(tr2)
                        parm2tr = TimeRange.javaTimeRangeListToPyList(parm2.getKeys())
                        if tr2 is not None and tr2 not in parm2tr:
                            # Have to fetch the already calced one.
                            f = ((m[0], self.__exists,
                                  ('mtime', 'wename')), t, i)
                            if f not in rval:
                                rval.append(f)
            else:
                # Need to calc this one
                rval.append((m, t, i))
        return rval

    def sourceBaseTime(self):
        modelTime = self.srcdb().getModelTime()
        if modelTime is None:
            modelTime = 0 
        t = AbsTime.AbsTime(modelTime)
        return t.unixTime()
        
    # JULIYA MODIFY HERE
    def __process(self, methods, times, mode):
        numGrids = 0
        trSpan = None
        cache = self.magicArgs()  
        all = mode#Options['all'] manual=1 automatic=0
        list = self.__flattenTimes(methods, times)                    
        if not all:
            list = self.__prune(list)

        self.mostRecentCacheClear = None
        for m, t, i in list:
            cache['ctime'] = (t, (0, sys.maxint))
            parm = self.__getNewWE(m[0])
            tr = parm.getTimeRange(t[0])
            
            # A valid time range was not found so the parameter 
            # cannot be calculated, so continue
            if not tr.isValid():
                continue
            
            cache['mtime'] = (tr, (0, sys.maxint))
            cache['wename'] = (m[0], (0, sys.maxint))
            cache['stime'] = (t[0] - self.sourceBaseTime(), (0, sys.maxint))
            
            try:
                self.__runMethod(m, t, cache)
                numGrids = numGrids + 1
                if trSpan is None:
                    trSpan = t
                else:
                    trSpan = (min(trSpan[0],t[0]), max(trSpan[1], t[1]))
            except:
                LogStream.logProblem("Error in method setup for " + str(m[0])
                                     + "\n" + LogStream.exc())

        return (trSpan, numGrids)

#-------------------------------------------------------------------------
# Weather Element calculations
#-------------------------------------------------------------------------
class IFPIO:
    def __init__(self, indb, outdb):
        self.__srcwes = {}
        self.__newwes = {}
        self.eta = indb
        self.newdb = outdb

    def getSrcWE(self, wename, lock=1):
        rval = None
        try:            
            rval = self.__srcwes[wename]
        except:
            rval = self.eta.getItem(wename)
            self.__srcwes[wename] = rval
        return rval

    def getNewWE(self, wename, lock=1):
        try:
            rval = self.__newwes[wename]
        except:
            rval = self.newdb.getItem(wename)
            self.__newwes[wename] = rval
        return rval

    def get(self, qv):        
        if len(qv) == 2:
            name, time = qv
            docube = 0
        else:
            name, time, docube = qv        
        if not docube:
            slice = self.getSrcWE(name, 0).getItem(time)
            out = slice.__numpy__
            if len(out) == 1:
                out = out[0]
            elif len(out) == 2 and type(out[1]) is str:
                exec "out[1] = " + out[1]
        else:
            out = self._getcube(self.eta, name, time)
        return out

    #--------------------------------------------------------------------------
    #  Sets the grid levels used for many algorithms.
    #--------------------------------------------------------------------------
    def setLevels(self, levels):
        self._levels = levels

    #--------------------------------------------------------------------------
    #  Returns the current list of levels
    #--------------------------------------------------------------------------
    def levels(self):
        return self._levels

    #--------------------------------------------------------------------------
    #  Returns the data cube for the specified db, parm, and time.
    #--------------------------------------------------------------------------
    def _getcube(self, db, parm, time):
        lvls = self.levels()
        lst = []
        pres = []
        for l in lvls:
            p = self.getSrcWE(parm + "_" + l, 0)
            slice = p.getItem(time)            
            slice = slice.__numpy__
            if len(slice) == 1:
                slice = slice[0]
            elif len(slice) == 2 and type(slice[1]) is str:
                exec "slice[1] = " + slice[1]
            lst.append(slice)
            pres.append(int(l[2:]))
        if type(lst[0]) == types.TupleType or type(lst[0]) == types.ListType:            
            ml = []
            dl = []
            for i in lst:
                ml.append(i[0])
                dl.append(i[1])
            rval = (array(ml), array(dl))
        else:            
            rval = array(lst)
        return (pres, rval)

    #--------------------------------------------------------------------------
    # Stores the specified grid in the element specified by newwe at the
    # specified time
    #--------------------------------------------------------------------------
    def store(self, newwe, time, grid):
        gridType = newwe.getGridType()            
        if gridType == "SCALAR":
            grid = clip(grid, newwe.getMinAllowedValue(), newwe.getMaxAllowedValue())
        elif gridType == "VECTOR":
            mag = clip(grid[0], newwe.getMinAllowedValue(), newwe.getMaxAllowedValue())
            dir = clip(grid[1], 0, 359.5)
            grid = (mag, dir)
        tr = TimeRange.encodeJavaTimeRange(time)        
        # safety checks        
        wrongType = None
        saved = False
        if type(grid) is ndarray:
            if grid.dtype != dtype('float32'):
                grid = grid.astype('float32')
            # scalar save
            newwe.setItemScalar(newwe.getTimeRange(tr[0]), grid)
            saved = True
        elif (type(grid) is list or type(grid) is tuple) and len(grid) == 2:
            if type(grid[0]) is ndarray and type(grid[1]) is ndarray:
                magGrid = grid[0]
                dirGrid = grid[1]
                if magGrid.dtype != dtype('float32'):
                    magGrid = magGrid.astype('float32')
                if dirGrid.dtype != dtype('float32'):
                    dirGrid = dirGrid.astype('float32')
                # vector save
                newwe.setItemVector(newwe.getTimeRange(tr[0]), magGrid, dirGrid)
                saved = True                                                    
            elif type(grid[0]) is ndarray and type(grid[1]) is list:
                bgrid = grid[0]
                if bgrid.dtype != dtype('byte'):
                    bgrid = bgrid.astype('byte')
                    
                if gridType == "DISCRETE":
                    newwe.setItemDiscrete(newwe.getTimeRange(tr[0]), bgrid, str(grid[1]))
                elif gridType == "WEATHER":
                    newwe.setItemWeather(newwe.getTimeRange(tr[0]), bgrid, str(grid[1]))
                    
                saved = True                
        if not saved:
            if wrongType is None:
                wrongType = type(grid)
            msg = str(wrongType) + " type returned from calcMethod is not safe to store for " + newwe.toString()
            raise TypeError(msg)

#--------------------------------------------------------------------------
# Main program
#--------------------------------------------------------------------------
def runFromJava(dbName, model, validTime):    
    SmartInitParams.params['dbName'] = dbName
    SmartInitParams.params['validTime'] = validTime
    
    mod = __import__(model)
    mod.main()
    rollbackImporter.rollback()
