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
# doConfig - validate and convert serverConfig into simple Java objects
#
# this file was originally config.py
# it was renamed to avoid a conflict with jep's built-in config module
#
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/09/2013          #1571     randerso       Changed projections to use the Java             
#                                                 ProjectionType enumeration
#    07/09/2014          #3146     randerso       Added check for duplicate smartInit
#
########################################################################
import types

from java.util import ArrayList,LinkedHashMap
from java.lang import Integer,Float
from com.vividsolutions.jts.geom import Coordinate
from java.awt import Point

Databases = {}
Projections = {}
DiscreteDef = LinkedHashMap()   #from parseKeys()
        
# Check a python sequence to see that
# it matches the format.
# data: is a sequence of objects
# fmt : is a parallel sequence of type objects
# message : optional message to print on exception
#
# Returns data.
# If len(data) != len(fmt)
# or the type of each element in data does not
# match the coresponding type in fmt, then
# a TypeError is raised.
# Example:  a, b = check(([1, 3], "foo"), (list, str))
def check(data, fmt, message, allData = None):
     if len(data) != len(fmt):
         m = message + ": Wrong number of items found, " + \
           "Expected " + `len(fmt)` + ", got " + `len(data)` + \
           " Input: " + `data`
         if allData is not None:
             m = m + ' All: ' + `allData`
         raise AssertionError, m
     for i in xrange(len(data)):
         obj = data[i]
         if hasattr(obj, "jclassname"):
             t = obj.jclassname
         else:
             t = type(obj)
         
         if t != fmt[i]:
             m = message + ": Wrong data type found, " + \
               "Expected " + `fmt[i]` + ", got " + `t` + \
               " for position #" + `i+1` + " Input: " + `data`
             if allData is not None:
                 m = m + ' All: ' + `allData`
             raise AssertionError, m
     return data

# dictionary check, keys are strings, values/subvalues  as specified
def dictCheck(dictionary, value, subvalue, configName):
    map = LinkedHashMap()
    if type(dictionary) == dict:
        for k in dictionary.keys():
            if type(k) != str:
                raise TypeError, configName + " key [" + `k` + "] not a str"
            l = dictionary[k]
            if type(l) != value:
                raise TypeError, configName + " value [" + `l` + "] not a " \
                  + `value`
            if value == list or value == tuple:
                n = ArrayList()
                for m in l:
                    if type(m) != subvalue:
                        raise TypeError, configName + " value [" + `l` \
                          + "] isn't a " + `subvalue` + ": " + `m`
                    elif subvalue == int:
                        n.add(Integer(m))
                    elif subvalue == float:
                        n.add(Float(m))
                    else:
                        n.add(m)
                map.put(k, n)
            else:
                if value == int:
                    map.put(k,Integer(l))
                elif value == float:
                    map.put(k,Float(l))
                else:
                    map.put(k, l)
    else:
        raise TypeError, configName + " not a dict:" + `dictionary`
    return map

def getWx(wxtypes, wxvisibilities):
    from com.raytheon.uf.common.dataplugin.gfe.weather import WeatherCoverage, WeatherIntensity, WeatherAttribute
    from com.raytheon.edex.plugin.gfe.config import SimpleWeatherTypeConfig
    types = ArrayList()
    for t in wxtypes:
        symbol, description, cov, inten, attr = \
          check(t, (str, str, list, list, list), "Error in WeatherType")
        coverages = ArrayList()
        intensities = ArrayList()
        attributes = ArrayList()
        for c in cov:
            csym, cdes = check(c, (str, str), "Error in Weather Coverage", t)
            coverages.add(WeatherCoverage(csym, cdes))
        for i in inten:
            isym, ides = check(i, (str, str), "Error in Weather Intensity", t)
            intensities.add(WeatherIntensity(isym, ides))
        for a in attr:
            asym, ades = check(a, (str, str), "Error in Weather Attributes", t)
            attributes.add(WeatherAttribute(asym, ades))

        types.add(SimpleWeatherTypeConfig(symbol, description, coverages,
                          intensities, attributes))
    vis = ArrayList()
    for v in wxvisibilities:
        vis.add(v)
    return (vis, types)

#note that DiscreteDef is a dictionary that contains the following
#coded strings:  ['OVERLAPS', 'AuxLength', sym1, des1, sym2, des2,  ....]
#We do this to pass to C++ as a InitDict, which
#is a Dict<TextString, SeqOf<TextString>.
def parseKeys(name, overlaps, auxLength, keys):
    if not DiscreteDef.containsKey(name):
        ts = ArrayList()
        if overlaps:
            ts.add("OVERLAPS")
        else:
            ts.add("MUTEXC")
        ts.add(`auxLength`)
        for symdes in keys:
            sym, des = check(symdes, (str, str),
              "Error in DiscreteKey Definition", keys)
            ts.add(sym)
            ts.add(des)
        if overlaps and len(keys) > 0 and keys[0][0] != "<None>":
            s = "1st discrete key must be <None> for OVERLAP-defined " +\
              "weather element. [" + name + "]" + `keys`
            raise Exception, s
        DiscreteDef.put(name, ts);

def createParm(parminfo, domain, tc):
    from com.raytheon.edex.plugin.gfe.config import SimpleGridParmConfig

    m = "Format Error in Weather Element Definition"
    if len(parminfo) < 2:
        raise TypeError, m + ': ' +  `parminfo`
    
    dim, origin, extent, timezone, projection,officeType = domain

    if parminfo[1] == 'Scalar' or parminfo[1] == 'Vector':
        parmInfoFmt = (str, str, str, str, float, float, int, int)
        name, type, units, description, max, min, precision, \
          rateParm = check(parminfo, parmInfoFmt, m)

    elif parminfo[1] == 'Weather':
        name, type, units, description = \
          check(parminfo, (str, str, str, str), m)
        max = 0
        min = 0
        precision = 0
        rateParm = False

    elif parminfo[1] == 'Discrete':
        if len(parminfo) == 6:
            parmInfoFmt = (str, str, str, str, int, list)
            name, type, units, description, overlaps, keys = \
              check(parminfo, parmInfoFmt, m)
            auxSize = 0
        else:
            parmInfoFmt = (str, str, str, str, int, list, int)
            name, type, units, description, overlaps, keys, auxSize = \
              check(parminfo, parmInfoFmt, m)
        max = 0.0
        min = 0.0
        precision = 0
        rateParm = False
        parseKeys(name, overlaps, auxSize, keys)

    else:
        raise Exception, "Illegal WE type specified for " + `parminfo[0]`

    #don't add parms with your own office type in the name.
    if name.find(officeType) != -1:
        return None     #skip this one
    
    updateProjections(projection)
    start, repeat, duration = tc
    timeIndependentParm = (repeat == 0 and duration == 0)

#    return WeatherElement(name, type, units, description, max, min,
#                          precision, timeIndependentParm, dim, origin,
#                          extent, start, repeat, duration, rateParm)
    return SimpleGridParmConfig(name, type, units, description, 1.0*max, 1.0*min,
                          precision, timeIndependentParm, Point(dim[0], dim[1]), Coordinate(origin[0], origin[1]),
                          Coordinate(extent[0], extent[1]), start, repeat, duration, rateParm)

def getDB(site, projID, dbinfo):
    from com.raytheon.edex.plugin.gfe.config import SimpleModelConfig

    dbinfoFmt = (str, str, str, int, int, int, int)
    name, format, type, single, official, numVer, purgeAge = \
      check(dbinfo, dbinfoFmt, "Error in Database Attribute Definition")

    if not Databases.has_key(name+type):
        Databases[name+type] = SimpleModelConfig(site, format, type, name, projID,
                                        single, official, numVer, purgeAge)
        
    return Databases[name+type]

def parseDBItm(site, domain, item):
#    import serverConfig
#    domain = serverConfig.SITES[site]
    
    dbinfo, parminfo = check(item, (tuple, list),
      "Database Definition or Parm Group Format Error")
    projID = domain[4][0]

    db = getDB(site, projID, dbinfo)
    grids = db.grids
    for ptc in parminfo:
        parms, tc = check(ptc, (list, tuple),
          "Parm Group/Time Constraint Tuple Error")
        check(tc, (int, int, int), "Time Constraint Format Error", ptc)
        for parm in parms:
            grids.add(createParm(parm, domain, tc))
    db.grids = grids

def updateProjections(projection):
    from com.raytheon.uf.common.dataplugin.gfe.config import ProjectionData
    # extract projection data
    projFmt = (str, 
      "com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData$ProjectionType", 
      tuple, tuple, tuple, float, float, tuple, tuple, float, float, float)
    projID, ptype, pllll, pllur, pllo, pspo, pspt, pgpll, pgpur, pli, \
      plc, plo = check(projection, projFmt, "Format error in Projection")
    check(pllll, (float, float),
      "Format error lower left long/lat in Projection", projection)
    check(pllur, (float, float),
      "Format error upper right long/lat in Projection", projection)
    check(pllo, (float, float),
      "Format error long/lat origin in Projection", projection)
    check(pgpll, (int, int),
      "Format error lower left grid point in Projection", projection)
    check(pgpur, (int, int),
      "Format error upper right grid point in Projection", projection)

    if not Projections.has_key(projID):
        Projections[projID] = ProjectionData(projID, ptype, 
                                             Coordinate(pllll[0],pllll[1]),
                                             Coordinate(pllur[0],pllur[1]), 
                                             Coordinate(pllo[0],pllo[1]), 
                                             pspo, pspt,
                                             Point(pgpll[0], pgpll[1]), 
                                             Point(pgpur[0], pgpur[1]), 
                                             pli, plc, plo)

def parseGridLocation(domain):
    from com.raytheon.edex.plugin.gfe.config import SimpleGridLocation
    
    #if office type is present:
    if len(domain) == 6:
        domainFmt = (list,tuple,tuple,str,tuple,str)
        gridSize, origin, extent, tz, proj, officeType = check(domain, domainFmt, "Format error in SITES line")
    #if office type is not present:
    else:
        domainFmt = (list, tuple, tuple, str, tuple)
        gridSize, origin, extent, tz, proj = check(domain, domainFmt,
          "Format error in SITES line")
    check(gridSize, (int, int), "GridSize format error from SITES", domain)
    check(origin, (float, float), "Origin format error from SITES", domain)
    check(extent, (float, float), "Extent format error from SITES", domain)

    projFmt = (str, 
      "com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData$ProjectionType", 
      tuple, tuple, tuple, float, float, tuple, tuple, float, float, float)
    projID, projType, llll, llur, llo, sp1, sp2, gpll, gpur, li, lc, lo = \
        check(proj, projFmt, "Format error in Projection")
    check(llll, (float, float),
      "Format error lower left long/lat in Projection", proj)
    check(llur, (float, float),
      "Format error upper right long/lat in Projection", proj)
    check(llo, (float, float),
      "Format error long/lat origin in Projection", proj)
    check(gpll, (int, int),
      "Format error lower left grid point in Projection", proj)
    check(gpur, (int, int),
      "Format error upper right grid point in Projection", proj)

    gloc = SimpleGridLocation(Point(gridSize[0], gridSize[1]), 
                              Coordinate(origin[0], origin[1]), 
                              Coordinate(extent[0], extent[1]), 
                              projID, projType, 
                              Coordinate(llll[0], llll[1]), 
                              Coordinate(llur[0], llur[1]), 
                              Coordinate(llo[0], llo[1]), 
                              sp1, sp2, 
                              Point(gpll[0], gpll[1]), 
                              Point(gpur[0], gpur[1]), 
                              li, lc, lo)
    return gloc

def parse(site, databases, wxtypes, wxvisibilities, allSites, inProjections):
    from com.raytheon.edex.plugin.gfe.config import SimpleGridLocation
    domain = parseGridLocation(allSites[site])
    for itm in databases:
        parseDBItm(site, allSites[site], itm)

    if type(wxtypes) != list:
        raise TypeError, "Format Error in WeatherTypes,  not a list: " \
          + `wxtypes`
    if type(wxvisibilities) != list:
        raise TypeError, "Format Error in visibilities,  not a list: " \
          + `wxvisibilities`
    vis, types = getWx(wxtypes, wxvisibilities)

    models = ArrayList()
    for key in Databases.keys():
        models.add(Databases[key])

    projections = ArrayList()
    if type(inProjections) != list:
        raise TypeError, "Format Error in Projections,  not a list: " \
          + `inProjections`
    for p in inProjections: 
        updateProjections(p)
    for key in Projections.keys():
        projections.add(Projections[key])

    allSiteIDs = ArrayList()
    allOfficeTypes = ArrayList()
    for key in allSites.keys():
        allSiteIDs.add(key)
        try:
            ot = allSites[key][5]
            if type(ot) != str:
               raise TypeError, "Format Error in office type, not a str:",allSites[key]
        except:
            ot = "wfo"  #assumes wfo if not present
        allOfficeTypes.add(ot)
    
    siteId = ArrayList()
    siteId.add(site)

    timeZone = ArrayList()
    timeZone.add(allSites[site][3])

    return models, projections, vis, types, DiscreteDef, allSiteIDs, domain, siteId, timeZone, allOfficeTypes

def d2dParse(d2dmodels):
    dict = LinkedHashMap()
    for entry in d2dmodels:
        if type(entry) is types.TupleType:
            d2dModelName, gfeModelName = check(entry, (str, str),
              "Format error in D2DMODELS entry", d2dmodels)

            dict.put(d2dModelName, gfeModelName)

        elif type(entry) is types.StringType:
            d2dModelName = entry
            dict.put(d2dModelName, d2dModelName)

        else:
            raise SyntaxError, "Invalid syntax for D2DMODELS" + `d2dmodels`

    return dict

def netcdfParse(netcdfDirs):
    dict = LinkedHashMap()
    for entry in netcdfDirs:
        if type(entry) is types.TupleType:
            direct, modelName = check(entry, (str, str),
              "Format error in NETCDFDIRS entry", netcdfDirs)

            if direct[-1] == '/':
                direct = direct[0:-1]
            dict.put(direct, modelName)

        elif type(entry) is types.StringType:
            direct = entry
            if direct[-1] == '/':
                direct = direct[0:-1]
            dict.put(direct,  '')

        else:
            raise SyntaxError, "Invalid syntax for NETCDFDIRS" + `netcdfDirs`

    return dict

def parseSat(satdata):
    rval = LinkedHashMap()
    for e in satdata:
        if type(e) is types.TupleType:
            direct, name = check(e, (str, str),
              "Format error in SATDATA entry", satdata)
            rval.put(direct, name)
        else:
            raise SyntaxError, "Invalid syntax for SATDATA" + `satdata`
    return rval

def otherParse(serverhost, mhsid, port,  
  initmodules, accumElem,
  initskips, d2ddbver, logfilepurge, prddir, home,
  extraWEPrec, vtecRequestTime, autoConfigureNotifyTextProd,
  iscRoutingTableAddress, requestedISCsites, requestISC, sendiscOnSave,
  sendiscOnPublish, requestedISCparms, transmitScript):
    if type(serverhost) != str:
        raise TypeError, "GFESUITE_HOST not an str: " + `serverhost`
    if type(mhsid) != str:
        raise TypeError, "GFESUITE_MHSID not an str: " + `mhsid`
    if type(vtecRequestTime) != int:
        raise TypeError, "VTECPartners: VTEC_REMOTE_TABLE_FETCH_TIME " + \
          "not an int: " + `vtecRequestTime`
    if type(port) != int:
        raise TypeError, "GFESUITE_PORT not an int: " + `port`
    javainitmodules = dictCheck(initmodules, list, str, "INITMODULES")
    accumElem = dictCheck(accumElem, list, str, "D2DAccumulativeElements")
    initskips = dictCheck(initskips, list, int, "INITSKIPS")
    d2ddbver = dictCheck(d2ddbver, int, None, "D2DDBVERSIONS")
    if type(logfilepurge) != int:
        raise TypeError, "LOG_FILE_PURGE_AFTER not an int: " + `logfilepurge`
    if type(autoConfigureNotifyTextProd) != int:
        raise TypeError, "AUTO_CONFIGURE_NOTIFYTEXTPROD not an int: " + \
          `logfilepurge`
    if type(prddir) != str:
        raise TypeError, "GFESUITE_PRDDIR not an str: " + `prddir`
    if type(home) != str:
        raise TypeError, "GFESUITE_HOME not an str: " + `home`
    if type(extraWEPrec) != list:
        raise TypeError, "ExtraWEPrec not an list: " + `extraWEPrec`
    else:
        extraWEPrecision = LinkedHashMap()
        for e in extraWEPrec:
            if type(e) == str:
                extraWEPrecision.put(e, Integer(1))
            elif type(e) == tuple and len(e) == 2 and type(e[0]) == str and \
              type(e[1]) == int:
                extraWEPrecision.put(e[0], Integer(e[1]))
            else:
                raise TypeError, \
                  "Entry in ExtraWEPrec not str or (str, int): " + `e`

    iscRoutingTableAddress = dictCheck(iscRoutingTableAddress,str,str,"ISC_ROUTING_TABLE_ADDRESS")
    #if type(iscRoutingTableAddress) not in [str, types.NoneType]:
    #    raise TypeError, "ISC_ROUTING_TABLE_ADDRESS not None or a str: " + \
    #      `iscRoutingTableAddress`
    #elif iscRoutingTableAddress is None:
    #    iscRoutingTableAddress = ""

    reqISCsites = ArrayList()
    if type(requestedISCsites) not in [list, types.NoneType]:
        raise TypeError, "REQUESTED_ISC_SITES not None or a list: " + \
          `requestedISCsites`
    elif type(requestedISCsites) is list:
        for r in requestedISCsites:
            if type(r) != str:
                raise TypeError, "REQUESTED_ISC_SITES not list of strings: " + \
                  `requestedISCsites`
            else:
                reqISCsites.add(r);

    reqISCparms = ArrayList()
    if type(requestedISCparms) not in [list, types.NoneType]:
        raise TypeError, "REQUESTED_ISC_PARMS not None or a list: " + \
          `requestedISCparms`
    elif type(requestedISCparms) is list:
        for r in requestedISCparms:
            if type(r) != str:
                raise TypeError, "REQUESTED_ISC_PARMS not list of strings: " + \
                  `requestedISCparms`
            else:
                reqISCparms.add(r)

    if type(requestISC) != int:
        raise TypeError, "REQUEST_ISC not an int: " + `requestISC`
    if type(sendiscOnSave) != int:
        raise TypeError, "SEND_ISC_ON_SAVE not an int: " + `sendiscOnSave`
    if type(sendiscOnPublish) != int:
        raise TypeError, "SEND_ISC_ON_PUBLISH not an int: " + `sendiscOnPublish`

    if type(transmitScript) not in [str, types.NoneType]:
        raise TypeError, "TRANSMIT_SCRIPT not None or str: " + `transmitScript`
    elif transmitScript is None:
        transmitScript = ""
        
    # build model to init mapping
    modelToInit = {}
    for module in initmodules:
        for model in initmodules[module]:
            if modelToInit.has_key(model):
                modelToInit[model].append(module)
            else:
                modelToInit[model] = [module]
        
    # check for duplicate init modules
    for model in modelToInit:
        modules = modelToInit[model]
        if len(modules) > 1:
            message = "Multiple smartInit modules " + str(modules) + \
            " are enabled for D2D model: " + model + ". " + str(modules[1:]) + \
            " will be disabled. Please edit your localConfig.py file and disable all but one."

            # log error message to edex log
            import LogStream
            LogStream.logProblem(message);
            
            # log error to alertViz
            from ufpy import NotificationMessage
            nfm = NotificationMessage.NotificationMessage(message=message,
                  category="GFE", priority=1, source="GFE")
            nfm.send()
                                 
            # remove duplicate
            for module in modules[1:]:
                javainitmodules.remove(module)

    return serverhost, mhsid, \
      port, javainitmodules, accumElem, \
      initskips, d2ddbver, logfilepurge, prddir, home,\
      extraWEPrecision, vtecRequestTime, \
      autoConfigureNotifyTextProd, \
      iscRoutingTableAddress, reqISCsites, requestISC, sendiscOnSave, \
      sendiscOnPublish, reqISCparms, transmitScript
