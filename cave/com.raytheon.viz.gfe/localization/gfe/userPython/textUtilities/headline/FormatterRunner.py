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

import TimeRange, AbsTime
import logging
import TextFormatter
import time, os, string, inspect, sys
import JUtil, VarDictGroker
import RedirectLogging
import UFStatusHandler

from com.raytheon.uf.viz.core import VizApp
from com.raytheon.uf.common.gfe.ifpclient import PyFPClient

#
# Runs the text formatter to generate text products
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/29/08                      njensen       Initial Creation.
#    12/10/14        #14946        ryu           Add getTimeZones() function.
#    04/16/15        #14946        ryu           Fix getTimeZones to return the office TZ if timezone
#                                                is not set for any zone in a segment.
#    04/20/2015      #4027         randerso      Fixes for formatter autotests
#    Apr 25, 2015     4952         njensen        Updated for new JEP API
#    05/06/2015      #4467         randerso      Convert to upper case before writing to files if
#                                                mixed case is not enabled for the product.
#                                                Cleaned up file writing code
#    07/29/2015      #4263         dgilling      Support updated TextProductManager.
#    11/30/2015      #5129         dgilling      Support new IFPClient.
# 
#

displayNameDict = {}

# Set up logging info
PLUGIN_NAME = 'com.raytheon.viz.gfe'
CATEGORY = 'GFE'
DEFAULT_LOG_FILENAME = '/tmp/gfe.log'
FILEMODE='w'
PATH_MGR = None

try:
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger("FormatterRunner")
    
         
    formatter = logging.Formatter("%(asctime)s:%(name)s:%(levelname)s:%(message)s")
    
    # Get the information for the file logger
    from com.raytheon.uf.common.localization import PathManagerFactory
    from com.raytheon.uf.common.localization import LocalizationContext
    LocalizationType = LocalizationContext.LocalizationType 
    LocalizationLevel = LocalizationContext.LocalizationLevel 
    PATH_MGR = PathManagerFactory.getPathManager()    
except:    
    logging.basicConfig(filename=DEFAULT_LOG_FILENAME,level=logging.DEBUG)
    logger = logging.getLogger()
    logger.exception("Exception occurred")

## TODO: Remove use of DataManager in this code. Will need to coordinate with
## the field developers to ensure local site overrides aren't relying on having
## access to it.
def executeFromJava(databaseID, site, username, dataMgr, forecastList, logFile, cmdLineVarDict=None,
                    drtTime=None, vtecMode=None, vtecActiveTable="active", testMode=0 ):
    if type(forecastList) is not list:
        forecastList = [str(forecastList)]
        
    # Set up the file logger for this product
#    ctx = PATH_MGR.getContext(LocalizationType.valueOf('CAVE_STATIC'), LocalizationLevel.valueOf('USER'))
#    logFile = PATH_MGR.getFile(ctx, os.path.join('gfe', 'logs', forecastList[0])).getPath()    
    logger.info("logFile: " + str(logFile))     
    fh = logging.FileHandler(filename=logFile, mode=FILEMODE)
    fh.setLevel(logging.INFO)
    fh.setFormatter(formatter)
    logger.addHandler(fh)
    
    # redirect stdout and stderr to logger
    RedirectLogging.redirect(logger, stdout=True, stderr=True)
        
    logger.info(forecastList[0])

    site = str(site)
    databaseID = str(databaseID)
    username = str(username)    

    startTime = time.time()
    logger.info("Text Formatter Starting")
    
    forecasts = runFormatter(databaseID=databaseID, site=site, forecastList=forecastList, testMode=testMode,
                        cmdLineVarDict=cmdLineVarDict, vtecMode=vtecMode, username=username,
                        dataMgr=dataMgr, drtTime=drtTime, vtecActiveTable=vtecActiveTable)
    
    elapsedTime = (time.time() - startTime)*1000
    logger.info("Text Formatter Finished, took: %d ms",elapsedTime)

    RedirectLogging.restore()
    return forecasts
    
def getPid(forecast):
    # taken from ProductParser.py
    import re
    
    sl = r'^'                            # start of line
    el = r'\s*?\n'                       # end of line
    id3 = r'[A-Za-z]{3}'                 # 3 charater word
    empty = r'^\s*' + el                 # empty line
    
    wmoid = r'(?P<wmoid>[A-Z]{4}\d{2})' # wmoid
    fsid  = r'(?P<fsid>[A-Z]{4})'       # full station id
    pit   = r'(?P<pit>\d{6})'           # product issuance time UTC
    ff    = r'(?P<funnyfield> ' + id3 + ')?'          # "funny" field
    
    # CI block
    ci_start = sl + wmoid + ' ' + fsid + ' ' + pit + ff + el
    awipsid = r'(?P<pil>(?P<cat>[A-Z0-9]{3})(?P<lid>[A-Z0-9]{1,3}))' + el
    ci_block = r'(?P<ciblock>' + ci_start + awipsid + '\n?)' 
    
    ci_re = re.compile(ci_block)

    pid = None
    m = ci_re.search(forecast)
    if m is not None:
        pid = m.group('cat')

    return pid

def runFormatter(databaseID, site, forecastList, cmdLineVarDict, vtecMode,
                    username, dataMgr, serverFile=None,
                    editAreas=[], timeRanges=[], timePeriod=None, drtTime=None,
                    vtecActiveTable='active', testMode=0, experimentalMode=0, serverOutputFile=None,
                    startTime=None, endTime=None, language=None, outputFile=None, appendFile=None
                    ):
    
    if cmdLineVarDict:
        exec "cmdLineVarDict = " + cmdLineVarDict
    else:
        cmdLineVarDict = {}
    
    # Set default Forecast Type
    if len(forecastList) == 0:
        usage()
        logger.error("ForecastList [-t] is empty or missing")
        return

    # Can't have both T and E modes
    if testMode and experimentalMode:
        usage()
        logger.error("Can't have both -T and -E switches")
        return

    if drtTime is not None:
        import offsetTime
        offsetTime.setDrtOffset(drtTime)

    # Create Time Range
    useRawTR = 0
    if startTime is not None and endTime is not None:
        start = getAbsTime(startTime)
        end = getAbsTime(endTime)
        timeRange = TimeRange.TimeRange(start, end)
        # Set so this time range will override all others
        useRawTR = 1
    else:
        timeRange = None

    # Handle the VTEC modes
    if vtecMode is not None and vtecMode not in ['X','O','T','E']:
        usage()
        logger.error("-v vtecMode must be ['X', 'O', 'T', 'E']")
        sys.exit(1)

    #force VTEC mode to "T" if in TEST mode and another vtecCode is specified
    if testMode and vtecMode is not None:
        vtecMode = "T"

    #force VTEC mode to "E" if in EXPERIMENTAL mode and another vtecCode
    #is specified
    elif experimentalMode and vtecMode is not None:
        vtecMode = "E"

    #force into TEST mode, if vtec code is 'T'
    if vtecMode == "T":
        testMode = 1
        experimentalMode = 0
    elif vtecMode == "E":
        experimentalMode = 1
        testMode = 0

    # Create an ifpClient
    ifpClient = PyFPClient(VizApp.getWsId(), site)
    
    global GridLoc
    GridLoc = ifpClient.getDBGridLocation()
    #importer = TextIFPImporter(ifpClient)
    #importer.install()
    import Utility

    import ForecastNarrative
    import ForecastTable

    import Analysis

    site = str(ifpClient.getSiteID()[0])

    # Create dictionary of arguments
    argDict = {
        #"host" : host,
        #"port" : port,
        "databaseID": databaseID,
        "site" : site,
        "cmdLineVarDict": cmdLineVarDict,
        "serverFile": serverFile,
        "editAreas": editAreas,
        "timeRanges": timeRanges,
        "timeRange": timeRange,
        "timePeriod": timePeriod,
        "useRawTR": useRawTR,
        "vtecMode": vtecMode,
        "vtecActiveTable": vtecActiveTable,
        "testMode": testMode,
        "experimentalMode": experimentalMode,
        "serverOutputFile": serverOutputFile,
        }
    # Handle command line switches for variables that can be
    # set elsewhere i.e. in the command line varDict OR the
    # product definition section.
    # If there was a command line switch for these items,
    # make an entry in argDict. Otherwise, do not.
    for item in ["language", "outputFile", "appendFile"]:
        exec "if " + item + " is not None: argDict['" + item + "'] = " + item

    logger.info("Arguments: " + str(argDict))

    argDict["ifpClient"] = ifpClient
    argDict["utility"] = Utility.Utility(None, None, ifpClient)
    #argDict["AFPS"] = AFPS
    #argDict["AFPSSup"] = AFPSSup
    argDict["Analysis"] = Analysis
    argDict["ForecastNarrative"] = ForecastNarrative
    argDict["ForecastTable"] = ForecastTable

    # get product creation time to the minute - almost all fmtrs use this
    argDict['creationTime'] = int(time.time()/60)*60.0

    # Set the Site Time Zone
    tz = str(ifpClient.getSiteTimeZone())
    os.environ['TZ'] = tz
    time.tzset()

    # Create the formatter
    formatter = TextFormatter.TextFormatter(dataMgr, ifpClient)

    # For each Forecast Type,
    #   Create generate forecast
    forecasts = ""      # returned value
    outForecasts = ""   # written to output files
    for forecastType in forecastList:           
        forecast = formatter.getForecast(forecastType, argDict)        
        forecasts = forecasts + forecast
        
        # Convert data written to files to upper case if required
        mixedCase = False
        pid = getPid(forecast)
        if pid is None:
            logger.warning("Unable to determine PID: defaulting to upper case")
        else:
            from com.raytheon.uf.common.dataplugin.text.db import MixedCaseProductSupport
            mixedCase = MixedCaseProductSupport.isMixedCase(str(pid))
        
        if mixedCase:
            outForecasts = outForecasts + forecast
        else:
            outForecasts = outForecasts + forecast.upper()

    logger.info("Text:\n" + str(forecasts))
    
    try:
        outputFile = argDict["outputFile"]
        success = writeToFile(outForecasts, outputFile, "w")
        if success == 0:
            print "Couldn't open output file", outputFile
            logger.error("Couldn't open output file: ", outputFile)
            sys.exit(1)
    except:
        pass

    try:
        outputFile = argDict["serverOutputFile"]
        success = writeToFile(outForecasts, outputFile, "w")
        if success == 0:
            print "Couldn't open output file", outputFile
            logger.error("Couldn't open output file: ", outputFile)
            sys.exit(1)
    except:
        pass


    try:
        appendFile = argDict["appendFile"]
        success = writeToFile(outForecasts, appendFile, "a")
        if success == 0:
            print "Couldn't open append file", appendFile
            logger.error("Couldn't write to append file: ", appendFile)
        sys.exit(1)
    except:
        pass

    try:
        serverFile = argDict["serverFile"]
        writeToSite = (username == "SITE")
        success = writeToServerFile(outForecasts, serverFile, writeToSite)
        if success == 0:
            print "Couldn't open server output file", serverFile
            logger.error("Couldn't open server output file: ", serverFile)
            sys.exit(1)
    except:
        pass

    del outForecasts
    
    # Remove any lat/lon areas created temporarily
    #global LatLonIds
    #argDict["ifpClient"].deleteReferenceData(LatLonIds)

    # Somebody is holding onto an ifpClient and thus the C++
    # object is not being destroyed.  This causes the network
    # connection to stay open.  Below is a kludge to force
    # the destruction of the C++ object.
    #del ifpClient.this

    # This also means that you may not import any new modules after this
    # point!!!!!!!!!!!!!!!
    return forecasts

def getAbsTime(timeStr):
    "Create an AbsTime from a string: YYYYMMDD_HHMM"

    year = int(timeStr[0:4])
    month = int(timeStr[4:6])
    day = int(timeStr[6:8])
    hour = int(timeStr[9:11])
    minute = int(timeStr[11:13])

    return AbsTime.absTimeYMD(year, month, day, hour, minute)
    
def writeToFile(forecasts, outputFile, mode):
    if outputFile:
        logger.info("Writing forecast to " + outputFile)
        try:
            with open(outputFile, mode) as outfile:
                outfile.write(forecasts)
                
            os.chmod(outputFile, 0644)
        except:
            logger.exception("Error writing forecast to "+outputFile)
            return 0
    return 1

def writeToServerFile(forecasts, outputFile, writeToSite):
    if outputFile:
        try:
            if writeToSite:
                ctx = PATH_MGR.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.SITE)
            else:
                ctx = PATH_MGR.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.USER)
            filePath = PATH_MGR.SEPARATOR.join(["gfe", "text", "PRODGEN", outputFile + ".PRODGEN"])
            lFile = PATH_MGR.getLocalizationFile(ctx, filePath)
            logger.info("Writing forecast to " + str(lFile))
    
            from LockingFile import File
            with File(lFile.getFile(), "", 'w') as outfile:
                outfile.write(forecasts)
    
            return lFile.save()
        except:
            logger.exception("Error writing forecast to " + str(lFile))
            return 0
    return 1

def importModules(paths):
    global displayNameDict
    displayNameDict = {}
    
    split = paths.split(os.path.pathsep)
    for path in split:
        if not path in sys.path:
            sys.path.append(path)

        inv = []
        if os.path.exists(path):
            inv = os.listdir(path)
            inv = filter(filterScripts, inv)

        for pid in inv:
            name = os.path.splitext(pid)[0]
            if sys.modules.has_key(name):
                del sys.modules[name]
            try:
                mod = __import__(name)
            except:
                logger.exception("Import Failed " + name)
                mod = None
            definition = None
            if mod is not None:
                d = mod.__dict__
                #search for Definition at top-level
                definition = d.get('Definition', None)
                if definition is None:
                    # search for definition within class name
                    definition = d.get(name, None)
                if definition is None:
                    tp = d.get('TextProduct', None)
                    if tp is not None:
                        #search for definition within TextProduct class                    
                        definition = getattr(tp, 'Definition', None)                    
            if definition is None or type(definition) is not dict:
                logger.info("Formatter: No Definition Found " + 
                                     name)
                continue
            dspName = getDisplayName(definition)
            if dspName is None or dspName == "None":
                continue
            displayNameDict[dspName] = (mod, definition)

def getScripts(paths, getVtecCodes):
    from java.util import ArrayList
    from com.raytheon.uf.common.dataplugin.gfe.textproduct import ProductDefinition
    from com.raytheon.viz.gfe.textformatter import TextProductConfigData
    from com.raytheon.viz.gfe.textformatter import TextProductMetadata
    
    logger.info("TextProduct FormatterLauncher Processing....")
    importModules(paths)    
    textProducts = ArrayList()
    for (displayName, value) in displayNameDict.items():
        (module, definition) = value
        moduleName = module.__name__
        pdef = ProductDefinition(JUtil.pyDictToJavaMap(definition))
        productMetadata = TextProductMetadata(moduleName, displayName, pdef)
        textProducts.add(productMetadata)
    
    vtecCodes = {}
    if getVtecCodes:
        import VTECMessageType
        vtecCodes = VTECMessageType.VTECMessageTypeDict

    logger.info("TextProduct FormatterLauncher Done....")
    return TextProductConfigData(JUtil.pyValToJavaObj(vtecCodes), textProducts)

def filterScripts(name):
    (filename, ext) = os.path.splitext(name)
    return ext == ".py" and not filename.endswith("Definition")

def getDisplayName(definition):
    try:
        dspName = definition['displayName']
    except:
        dspName = None
    return dspName

def ppDef(definition):
    "pretty prints the definition to make it more readable. Returns string."
        
    s = "\n"
    if definition is None:
        return "<Definition is None>"
    if type(definition) == dict and len(definition.keys()):
        keys = definition.keys()
        keys.sort()
        #get maximum length of key
        maxL = 0
        for k in keys:
            maxL = max(len(k), maxL)
        # output the data, formatted
        fmt = "%-" + `maxL` + "s"
        for k in keys:
            s = s + fmt % k + ": " + str(definition[k]) + '\n'
        return s
    else:
        return "<Definition not dictionary>\n" + `definition`

## TODO: Investigate if the dependency on DataManager can be removed here.
## At the moment this passes through to ValuesDialog for building special
## widgets in the DialogAreaComposite.    
def getVarDict(paths, dspName, dataMgr, ifpClient, issuedBy, dataSource):
    importModules(paths)
    
    tz = str(ifpClient.getSiteTimeZone())
    os.environ['TZ'] = tz
    time.tzset()
    productDef = displayNameDict[dspName][1]
    productDef['database'] = dataSource
    vdg = VarDictGroker.VarDictGroker(displayNameDict[dspName][0], productDef, dspName, issuedBy, dataMgr)
    return vdg.getVarDict()

def getVTECMessageType(productCategory):
    import VTECMessageType
    return VTECMessageType.getVTECMessageType(productCategory)

def getTimeZones(zones, officeTZ):
    import AreaDictionary
    timezones = []
    if zones is not None:
        for zone in zones:
            zdict = AreaDictionary.AreaDictionary.get(zone, {})
            tzs = zdict.get("ugcTimeZone", [])
            if type(tzs) is str:
                tzs = [tzs]
            for tz in tzs:
                if tz not in timezones:
                    timezones.append(tz)
    if officeTZ in timezones and officeTZ != timezones[0]:
        timezones.remove(officeTZ)
        timezones.insert(0, officeTZ)
    if len(timezones) == 0:
        timezones.append(officeTZ)
    return JUtil.pylistToJavaStringList(timezones)

def reloadModule(moduleName):
#    m = __import__(moduleName)
#    reload(m)
    if sys.modules.has_key(moduleName):
        del sys.modules[moduleName]
    try:
        __import__(moduleName)
    except:
        logger.exception("Import Failed " + moduleName)

    
