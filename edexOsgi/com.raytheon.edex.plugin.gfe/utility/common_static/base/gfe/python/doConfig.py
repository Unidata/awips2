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
#                                  rferrel        Corrected log to alertviz.
#    11/18/2014          #4953     randerso       Added check for empty unit string
#    04/09/2015          #4383     dgilling       Added support for FireWx ISC.       
#    Apr 23, 2015        #4259     njensen        Updated for new JEP API       
#    09/01/2015          16287     amoore         Additional validation of user input      
#    05/24/2016          15633     bhunder        Modified so that a parm name could
#                                                 contain your office type.
#    09/12/2016          #5861     randerso       Change getSiteID() to return a single value
#                                                 instead of a list containing only one value.
#    05/14/2019        DCS21081    dfriedman      Removed log purging configuration.
#                      DR 21355
#
########################################################################

##
# This is a base file that is not intended to be overridden.
##



import re,configProps

from java.util import ArrayList,LinkedHashMap
from java.lang import Integer,Float
from org.locationtech.jts.geom import Coordinate
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
           "Expected " + repr(len(fmt)) + ", got " + repr(len(data)) + \
           " Input: " + repr(data)
         if allData is not None:
             m = m + ' All: ' + repr(allData)
         raise AssertionError(m)
     for i in range(len(data)):
         obj = data[i]
         if hasattr(obj, "java_name"):
             t = obj.java_name
         else:
             t = type(obj)
         
         if t != fmt[i]:
             m = message + ": Wrong data type found, " + \
               "Expected " + repr(fmt[i]) + ", got " + repr(t) + \
               " for position #" + repr(i+1) + " Input: " + repr(data)
             if allData is not None:
                 m = m + ' All: ' + repr(allData)
             raise AssertionError(m)
     return data

# dictionary check, keys are strings, values/subvalues  as specified
def dictCheck(dictionary, value, subvalue, configName):
    map = LinkedHashMap()
    if type(dictionary) is dict:
        for k in dictionary:
            if type(k) is not str:
                raise TypeError(configName + " key [" + repr(k) + "] not a str")
            l = dictionary[k]
            if type(l) is not value:
                raise TypeError(configName + " value [" + repr(l) + "] not a " \
                  + repr(value))
            if value is list or value is tuple:
                n = ArrayList()
                for m in l:
                    if type(m) is not subvalue:
                        raise TypeError(configName + " value [" + repr(l) \
                          + "] isn't a " + repr(subvalue) + ": " + repr(m))
                    elif subvalue is int:
                        n.add(Integer(m))
                    elif subvalue is float:
                        n.add(Float(m))
                    else:
                        n.add(m)
                map.put(k, n)
            else:
                if value is int:
                    map.put(k,Integer(l))
                elif value is float:
                    map.put(k,Float(l))
                else:
                    map.put(k, l)
    else:
        raise TypeError(configName + " not a dict:" + repr(dictionary))
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
        ts.add(repr(auxLength))
        for symdes in keys:
            sym, des = check(symdes, (str, str),
              "Error in DiscreteKey Definition", keys)
            ts.add(sym)
            ts.add(des)
        if overlaps and len(keys) > 0 and keys[0][0] != "<None>":
            s = "1st discrete key must be <None> for OVERLAP-defined " +\
              "weather element. [" + name + "]" + repr(keys)
            raise Exception(s)
        DiscreteDef.put(name, ts)

def createParm(parminfo, domain, tc):
    from com.raytheon.edex.plugin.gfe.config import SimpleGridParmConfig

    m = "Format Error in Weather Element Definition"
    if len(parminfo) < 2:
        raise TypeError(m + ': ' +  repr(parminfo))
    
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
        raise Exception("Illegal WE type specified for " + repr(parminfo[0]))

    #don't add parms with your own office type in the name.
    if name.endswith(officeType):
        return None     #skip this one
    
    if len(units) == 0:
        raise Exception('Unit string must not be empty. For unitless quantities enter "1"')
    
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

    if name+type not in Databases:
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

    if projID not in Projections:
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

def parse(siteId, databases, wxtypes, wxvisibilities, allSites, inProjections):
    from com.raytheon.edex.plugin.gfe.config import SimpleGridLocation
    domain = parseGridLocation(allSites[siteId])
    for itm in databases:
        parseDBItm(siteId, allSites[siteId], itm)

    if type(wxtypes) != list:
        raise TypeError("Format Error in WeatherTypes,  not a list: " \
          + repr(wxtypes))
    if type(wxvisibilities) != list:
        raise TypeError("Format Error in visibilities,  not a list: " \
          + repr(wxvisibilities))
    vis, types = getWx(wxtypes, wxvisibilities)

    models = ArrayList()
    for val in Databases.values():
        models.add(val)

    projections = ArrayList()
    if type(inProjections) is not list:
        raise TypeError("Format Error in Projections,  not a list: " \
          + repr(inProjections))
    for p in inProjections: 
        updateProjections(p)
    for val in Projections.values():
        projections.add(val)

    allSiteIDs = ArrayList()
    allOfficeTypes = ArrayList()
    for key in allSites:
        allSiteIDs.add(key)
        try:
            ot = allSites[key][5]
            if type(ot) != str:
               raise TypeError("Format Error in office type, not a str:").with_traceback(allSites[key])
        except:
            ot = "wfo"  #assumes wfo if not present
        allOfficeTypes.add(ot)
    
    timeZone = ArrayList()
    timeZone.add(allSites[siteId][3])

    return models, projections, vis, types, DiscreteDef, allSiteIDs, domain, siteId, timeZone, allOfficeTypes

def d2dParse(d2dmodels):
    dict = LinkedHashMap()
    for entry in d2dmodels:
        if type(entry) is tuple:
            d2dModelName, gfeModelName = check(entry, (str, str),
              "Format error in D2DMODELS entry", d2dmodels)

            dict.put(d2dModelName, gfeModelName)

        elif type(entry) is str:
            d2dModelName = entry
            dict.put(d2dModelName, d2dModelName)

        else:
            raise SyntaxError("Invalid syntax for D2DMODELS" + repr(d2dmodels))

    return dict

def netcdfParse(netcdfDirs):
    dict = LinkedHashMap()
    for entry in netcdfDirs:
        if type(entry) is tuple:
            direct, modelName = check(entry, (str, str),
              "Format error in NETCDFDIRS entry", netcdfDirs)

            if direct[-1] == '/':
                direct = direct[0:-1]
            dict.put(direct, modelName)

        elif type(entry) is str:
            direct = entry
            if direct[-1] == '/':
                direct = direct[0:-1]
            dict.put(direct,  '')

        else:
            raise SyntaxError("Invalid syntax for NETCDFDIRS" + repr(netcdfDirs))

    return dict

def parseSat(satdata):
    rval = LinkedHashMap()
    for e in satdata:
        if type(e) is tuple:
            direct, name = check(e, (str, str),
              "Format error in SATDATA entry", satdata)
            rval.put(direct, name)
        else:
            raise SyntaxError("Invalid syntax for SATDATA" + repr(satdata))
    return rval

def otherParse(validSites, serverhost, mhsid, port,  
  initmodules, accumElem,
  initskips, d2ddbver, prddir, home,
  extraWEPrec, vtecRequestTime, autoConfigureNotifyTextProd,
  iscRoutingTableAddress, requestedISCsites, requestISC, sendiscOnSave,
  sendiscOnPublish, requestedISCparms, transmitScript):
    if type(serverhost) is not str:
        raise TypeError("GFESUITE_HOST not an str: " + repr(serverhost))
    if type(mhsid) is not str:
        raise TypeError("GFESUITE_MHSID not an str: " + repr(mhsid))
    if type(vtecRequestTime) is not int:
        raise TypeError("VTECPartners: VTEC_REMOTE_TABLE_FETCH_TIME " + \
          "not an int: " + repr(vtecRequestTime))
    if type(port) is not int:
        raise TypeError("GFESUITE_PORT not an int: " + repr(port))
    initmodules = dictCheck(initmodules, list, str, "INITMODULES")
    accumElem = dictCheck(accumElem, list, str, "D2DAccumulativeElements")
    initskips = dictCheck(initskips, list, int, "INITSKIPS")
    d2ddbver = dictCheck(d2ddbver, int, None, "D2DDBVERSIONS")
    if type(autoConfigureNotifyTextProd) is not int:
        raise TypeError("AUTO_CONFIGURE_NOTIFYTEXTPROD not an int: " + \
          repr(autoConfigureNotifyTextProd))
    if type(prddir) is not str:
        raise TypeError("GFESUITE_PRDDIR not an str: " + repr(prddir))
    if type(home) is not str:
        raise TypeError("GFESUITE_HOME not an str: " + repr(home))
    if type(extraWEPrec) is not list:
        raise TypeError("ExtraWEPrec not an list: " + repr(extraWEPrec))
    else:
        extraWEPrecision = LinkedHashMap()
        for e in extraWEPrec:
            if type(e) is str:
                extraWEPrecision.put(e, Integer(1))
            elif type(e) is tuple and len(e) == 2 and type(e[0]) is str and \
              type(e[1]) is int:
                extraWEPrecision.put(e[0], Integer(e[1]))
            else:
                raise TypeError("Entry in ExtraWEPrec not str or (str, int): " + repr(e))

    iscRoutingTableAddress = dictCheck(iscRoutingTableAddress,str,str,"ISC_ROUTING_TABLE_ADDRESS")
    #if type(iscRoutingTableAddress) not in [str, types.NoneType]:
    #    raise TypeError, "ISC_ROUTING_TABLE_ADDRESS not None or a str: " + \
    #      `iscRoutingTableAddress`
    #elif iscRoutingTableAddress is None:
    #    iscRoutingTableAddress = ""

    reqISCsites = ArrayList()
    if type(requestedISCsites) not in [list, type(None)]:
        raise TypeError("REQUESTED_ISC_SITES not None or a list: " + \
          repr(requestedISCsites))
    elif type(requestedISCsites) is list:
        for r in requestedISCsites:
            if type(r) is not str:
                raise TypeError("REQUESTED_ISC_SITES not list of strings: " + \
                  repr(requestedISCsites))
                #Verify requested ISC site is of desired pattern
            elif r not in validSites:
                raise ValueError("Requested ISC site: " + str(r) + " could not be found in serverConfig.py.")
            else:
                reqISCsites.add(r)

    reqISCparms = ArrayList()
    if type(requestedISCparms) not in [list, type(None)]:
        raise TypeError("REQUESTED_ISC_PARMS not None or a list: " + \
          repr(requestedISCparms))
    elif type(requestedISCparms) is list:
        for r in requestedISCparms:
            if type(r) is not str:
                raise TypeError("REQUESTED_ISC_PARMS not list of strings: " + \
                  repr(requestedISCparms))
                  #Verify requested ISC parm is of desired pattern
            elif not re.match(configProps.ISC_PARM_PATTERN, str(r)):
                raise ValueError("Requested ISC parm: " + str(r) + " does not match desired pattern: " + configProps.ISC_PARM_PATTERN)
            else:
                reqISCparms.add(r)

    if type(requestISC) is not bool:
        #If the type is boolean, it is already a valid value
        #If the type is not boolean, and is not int, then it is not valid
        if type(requestISC) is not int:
            raise TypeError("REQUEST_ISC not an int or boolean: " + repr(requestISC))
        #Verify request ISC is of valid value
        elif not ((requestISC == 0) or (requestISC == 1)):
            raise ValueError("REQUEST_ISC is: " + repr(requestISC) + ", but expected True, False, 0 or 1")
    
    if type(sendiscOnSave) is not bool:
        #If the type is boolean, it is already a valid value
        #If the type is not boolean, and is not int, then it is not valid
        if type(sendiscOnSave) is not int:
            raise TypeError("SEND_ISC_ON_SAVE not an int or boolean: " + repr(sendiscOnSave))
        #Verify send ISC on save is of valid value
        elif not ((sendiscOnSave == 0) or (sendiscOnSave == 1)):
            raise ValueError("SEND_ISC_ON_SAVE is: " + repr(sendiscOnSave) + ", but expected True, False, 0 or 1")
    
    if type(sendiscOnPublish) is not bool:
        #If the type is boolean, it is already a valid value
        #If the type is not boolean, and is not int, then it is not valid
        if type(sendiscOnPublish) is not int:
            raise TypeError("SEND_ISC_ON_PUBLISH not an int or boolean: " + repr(sendiscOnPublish))
        #Verify send ISC on publish is of valid value
        elif not ((sendiscOnPublish == 0) or (sendiscOnPublish == 1)):
            raise ValueError("SEND_ISC_ON_PUBLISH is: " + repr(sendiscOnPublish) + ", but expected True, False, 0 or 1")

    if type(transmitScript) not in [str, type(None)]:
        raise TypeError("TRANSMIT_SCRIPT not None or str: " + repr(transmitScript))
    elif transmitScript is None:
        transmitScript = ""

    return serverhost, mhsid, \
      port, initmodules, accumElem, \
      initskips, d2ddbver, prddir, home,\
      extraWEPrecision, vtecRequestTime, \
      autoConfigureNotifyTextProd, \
      iscRoutingTableAddress, reqISCsites, requestISC, sendiscOnSave, \
      sendiscOnPublish, reqISCparms, transmitScript

def parseAdditionalISCRouting(iscRoutingData):
    from com.raytheon.edex.plugin.gfe.config import ISCRoutingConfig
    
    retVal = ArrayList()
    if iscRoutingData:
        try:
            iter(iscRoutingData)
        except TypeError:
            raise TypeError("AdditionalISCRouting should be a list or tuple.")
        
        for entry in iscRoutingData:
            (pyParms, dbName, editAreaPrefix) = check(entry, (list, str, str), "AdditionalISCRouting entry not in correct format.")
            javaParms = ArrayList()
            for parm in pyParms:
                javaParms.add(str(parm[0]))
            retVal.add(ISCRoutingConfig(javaParms, dbName, editAreaPrefix))
    
    return retVal
