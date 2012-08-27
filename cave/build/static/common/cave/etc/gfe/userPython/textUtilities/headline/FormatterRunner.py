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

from java.io import File
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
#    
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
    from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType, LocalizationContext_LocalizationLevel as LocalizationLevel
    PATH_MGR = PathManagerFactory.getPathManager()    
except:    
    logging.basicConfig(filename=DEFAULT_LOG_FILENAME,level=logging.DEBUG)
    logger = logging.getLogger()
    logger.exception("Exception occurred")

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
    logger.info("TextFormatter Starting")
    forecasts = runFormatter(databaseID=databaseID, site=site, forecastList=forecastList, testMode=testMode,
                        cmdLineVarDict=cmdLineVarDict, vtecMode=vtecMode, username=username,
                        dataMgr=dataMgr, drtTime=drtTime)
    
    RedirectLogging.restore()
    return forecasts
    

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
    ifpClient = dataMgr.getClient()
    
    global GridLoc
    GridLoc = ifpClient.getDBGridLocation()
    #importer = TextIFPImporter(ifpClient)
    #importer.install()
    import Utility

    import ForecastNarrative
    import ForecastTable

    import Analysis

    site = str(ifpClient.getSiteID().get(0))

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
    #tz = ifpClient.getSiteTimeZone(site)
    #os.environ['TZ'] = tz
    tz = str(ifpClient.getSiteTimeZone())
    os.environ['TZ'] = tz

    # Create the formatter
    formatter = TextFormatter.TextFormatter(dataMgr)

    # For each Forecast Type,
    #   Create generate forecast
    forecasts = ""
    for forecastType in forecastList:           
        forecast = formatter.getForecast(forecastType, argDict)        
        forecasts = forecasts + forecast

    logger.info("Text:\n" + str(forecasts))
    try:
        outputFile = argDict["outputFile"]
        success = writeToFile(forecasts, outputFile, "w")
        if success == 0:
            print "Couldn't open output file", outputFile
            logger.error("Couldn't open output file: ", outputFile)
            sys.exit(1)
    except:
        pass

    try:
        outputFile = argDict["serverOutputFile"]
        success = writeToFile(forecasts, outputFile, "w")
        if success == 0:
            print "Couldn't open output file", outputFile
            logger.error("Couldn't open output file: ", outputFile)
            sys.exit(1)
    except:
        pass


    try:
        appendFile = argDict["appendFile"]
        success = writeToFile(forecasts, appendFile, "a")
        if success == 0:
            print "Couldn't open append file", appendFile
            logger.error("Couldn't write to append file: ", appendFile)
        sys.exit(1)
    except:
        pass

    try:
        serverFile = argDict["serverFile"]
        writeToSite = (username == "SITE")
        success = writeToServerFile(forecasts, serverFile, writeToSite)
        if success == 0:
            print "Couldn't open server output file", serverFile
            logger.error("Couldn't open server output file: ", serverFile)
            sys.exit(1)
    except:
        pass

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
    logger.info("Text Formatter Finished")
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
    if not outputFile is None and outputFile != "":
        outfile = open(outputFile, mode)
        os.chmod(outputFile, 0644)
        if outfile is None:
            return 0
        else:
            outfile.write(forecasts)
            outfile.close()
    return 1

def writeToServerFile(forecasts, outputFile, writeToSite):
    if not outputFile is None and outputFile != "":
        if writeToSite:
            ctx = PATH_MGR.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.SITE)
        else:
            ctx = PATH_MGR.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.USER)
        filePath = File.separatorChar.join(["gfe", "text", "PRODGEN", outputFile + ".PRODGEN"])
        lFile = PATH_MGR.getLocalizationFile(ctx, filePath)
        javaFile = lFile.getFile()        
        outfile = open(javaFile.getAbsolutePath(), 'w')
        outfile.write(forecasts)
        outfile.close()
        return lFile.save()
    return 1

def getScripts(paths, nameMap, definitionMap):
    global displayNameDict
    displayNameDict = {}
    
    rval = []
    split = paths.split(':')
    for path in split:
        if not path in sys.path:
            sys.path.append(path)
        inv = os.listdir(path)
        inv = filter(filterScripts, inv)
        logger.info("TextProduct FormatterLauncher Processing....")
        for pid in inv:
            name = string.split(pid, ".")[0]
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
            #fs = FormatterScript(self._dataMgr, name, mod, definition)
            dspName = getDisplayName(definition)
            if dspName is None or dspName == "None":
                #LogStream.logVerbose("Formatter displayName is None: " + 
                #    ppDef(definition))
                continue
            #LogStream.logVerbose("Formatter displayName set: ", fs.dspName(),
            #    ppDef(definition))        
            nameMap.put(dspName, name)
            from com.raytheon.uf.common.dataplugin.gfe.textproduct import ProductDefinition
            pdef = ProductDefinition()
            pdef.setDefinition(JUtil.pyDictToJavaMap(definition))
            definitionMap.put(dspName, pdef)
            displayNameDict[dspName] = (mod, definition)
            #rval.append(fs)
    #rval.sort()
    logger.info("TextProduct FormatterLauncher Done....")
    #return rval

def filterScripts(name):
    result = False
    if name.endswith(".py") and not name.endswith("Definition.py"):
        result = True
    return result

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
    
def getVarDict(dspName, dataMgr, issuedBy, dataSource):
    tz = str(dataMgr.getClient().getSiteTimeZone())
    os.environ['TZ'] = tz
    productDef = displayNameDict[dspName][1]
    productDef['database'] = dataSource
    vdg = VarDictGroker.VarDictGroker(displayNameDict[dspName][0], productDef, dspName, issuedBy, dataMgr)
    return vdg.getVarDict()

def getVTECMessageType(productCategory):
    import VTECMessageType
    return VTECMessageType.getVTECMessageType(productCategory)

def reloadModule(moduleName):
#    m = __import__(moduleName)
#    reload(m)
    if sys.modules.has_key(moduleName):
        del sys.modules[moduleName]
    try:
        __import__(moduleName)
    except:
        logger.exception("Import Failed " + moduleName)

    
