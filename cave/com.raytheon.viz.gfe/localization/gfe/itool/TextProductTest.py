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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TextProductTest
#
# Author:
# ----------------------------------------------------------------------------

##INSTRUCTIONS FOR USE:

##    0. Set up a site with all products.  From GFESuite directory:
##        --make SITE=TBW dev
##        --Add to release/data/textInstall/afos2awips.txt:

##            MIAOFFABC UFUS42 KTBW
##            MIANSHABC UFUS42 KTBW
##            MIAGLFABC UFUS42 KTBW
##            MIAGLFDEF UFUS42 KTBW
##            MIASAFABC UFUS42 KTBW

##        --start (or re-start the server)
##        NOTE: You will have to repeat this step every time you do
##          --make SITE=TBW dev

##    1. Bring up GFETEST with TestConfig file.
##    1.1 From release/bin, set up default edit areas:
##       run/setupTextEA
##    1.2 Copy examples/iTool/IToolLocalConfig.py release/etc/SITE/localConfig.py
##    2. Bring up the iTools dialog: GFE-->Define iTools
##    3. Activate "TextProductTest": MB3-->Activate
##    4. To Test CivilEmergency Products:
##        --MB1 over "TextProductTest"
##        --Choose mode: Verbose mode will display product output in terminal window.
##        --Click "Run"
##        --You can watch the progress of the products running from the
##          GFE Process Monitor:  Products-->Process Monitor
##    5. To Test Hazard Products:
##        --MB1 over "TextProductTest"
##        --Choose "Hazards_TestScript"
##        --Choose mode: Verbose mode will display product output in terminal window.
##        --Click "Run"
##        --You can watch the progress of the products running from the
##          GFE Process Monitor:  Products-->Process Monitor
##    6. To test the Routine Forecast products:
##        --Run release/bin/run/setupTextEA
##        --Edit AFD_Site_Definition file "state_IDS" entry
##        --MB1 over "TextProductTest"
##        --Choose "RoutineLevel1_TestScript"
##        --Choose mode: Verbose mode will display product output in terminal window.
##        --Click "Run"
##        --You can watch the progress of the products running from the
##          GFE Process Monitor:  Products-->Process Monitor
##
##    NOTE:  To abort a script while running, click MB2 over TextProductTest.

##    To make your own test script:
##        --Follow the example of the existing Test Scripts
##        --Each script entry is a dictionary with the following fields:
##            "name": required. This must be a unique test name
##            "productType": required. This is the name of the text product
##                for the -t option in the command line. For example: ZFP_BOU
##                You may use the variable <site> in the string.
##                Can be None.
##            "commentary": optional. A text string describing the test.
##            "cmdLineVars": optional. Command line variables for the product.
##                Can be a method to call to get the command line variables.
##                The method will be sent the product Definition.
##            "callVariables": optional. Applied only if cmdLineVars is a method.
##                Dictionary of "key:value" pairs to be added to the Defintion
##                before calling the cmdLineVars method.
##            "database": optional. Default is Fcst database.
##            "checkMethod": optional. Method to call to check the product results.
##                The method will be given the product text output and should return
##                1 if the tests pass, 0 if failed.
##            "checkStrings": optional. List of strings which must be included
##                in the product results for the test to pass.
##            "orderStrings": optional. If 1, will require that "checkStrings" appear
##                in the product in the order given in the checkStrings list.
##            "notCheckStrings": optional. List of strings which must NOT be included
##                in the product results for the test to pass.
##            "combinationsFileName": optional. Name of Combinations file to create.
##                May include the variable <site>.
##            "combinations": optional. Combinations for the Combinations file.
##            "deleteGrids": optional. List of tuples each
##                representing a grid that should be deleted before running the
##                product.  The tuple consists of:
##                    model,
##                    weather element name,
##                    level, (Can be "SFC", or D2D level e.g. "MB500")
##                    start hour for grid to be deleted (relative to gridsStartTime)
##                    end hour for grid to be deleted (relative to gridsStartTime)
##                NOTE: "deleteGrids" happens prior to "createGrids"
##            "createGrids": optional. List of tuples each
##                representing a grid that should be created before running the
##                product.  The tuple consists of:
##                    model,
##                    weather element name,
##                    elementType, (Can be "SCALAR", "VECTOR", "WEATHER", or "DISCRETE")
##                    start hour for grid to be created (relative to gridsStartTime)
##                    end hour for grid to be created (relative to gridsStartTime)
##                      Note: start and end hour can be a string expression involving
##                         ONE OF: MaxTBegin, MaxTEnd, MinTBegin, MinTEnd,
##                                 MaxRHBegin, MaxRHEnd, MinRHBegin, MinRHEnd
##                         which are relative to midnight of the day of gridsStartTime.
##                    data value for grid to be created
##                        (for hazards, value will be combined with other
##                         values specified for the grid)
##                    edit areas for the value: may be "all" to specify entire grid
##                    defaultValue (optional): If the default value for this grid
##                        is not zero, include it here.  For example, "<NoWx>", or 1 for LAL
##            "writableCopies": optional.  List of tuples each representing a
##                writable file copy that will be performed prior to running
##                the product.  The file is deleted after running the text
##                formatter. The tuple consists of:
##                   fileName: file to be copied from BASE. The name can 
##                    contain <site> which will be replaced by the siteID. No
##                    file extension is needed.
##                   fileType: directory of source of file, relative to the
##                    the databases level, such as "TEXT/Combinations"
##                   destFileName: renaming of the original filename may be
##                    accomplished here. The "user" is determined by the
##                    "user" definition in the test script, and defaults to
##                    GFETEST.
##            "fileChanges": optional. List of tuples each representing a file
##                that should be changed before running the product. The tuple consists
##                of:
##                    fileName: file to be changed.  The name can contain <site>
##                       which will be replaced by the siteID
##                    fileType: TEXT category, such as "TextUtility",
##                    type of change: can be
##                       "add": the string will be added to the file
##                       "replace": a given string will be replaced by another
##                    strings: If add, the string to be added.
##                        If replace, a tuple of original and replacement strings or
##                        a list of tuples for multiple replacements in the same file 
##                    cleanup action: What to do when product is finished. Can be:
##                       "delete": Delete the file and revert to baseline version
##                       "undo": Undo the add or replace
##                       "leave": Leave altered file as is
##            "publishGrids": optional.  If 1, will publish grids before running product.
##            "gridsStartTime": optional. Format YYYYMMDD_HHMM OR AbsTime. If present, will
##                    create and delete grids relative to this time. If not present,
##                    will use the "Today" select time range start time.
##            "drtTime": optional. Format YYYYMMDD_HHMM OR AbsTime. If present,
##                    will run the product in this displaced real time.
##                    If not present, and gridsStartTime is present,
##                    will run the product using gridsStartTime as the displaced real time.
##            "decodeVTEC": optional. If 1, will update the active table after running
##                    the product.
##            "internalStrip": optional. If set to 0, will not strip multiple spaces from the
##                    result string before matching checkString.  Default is 1.
##                    If set to 2, will try both the stripped and non-stripped
##                    fcstStr for a match.  if at least one matches each
##                    checkstring, the test will succeed.  Note: This will
##                    handle products with intermixed narrative and tabular
##                    formats such as the FWS.
##            "clearHazardsTable": optional. If 1, clear out the HazardsTable.
##            "vtecMode": optional. Can be set to "X", "E", "T", or "O" (-T/-E)

##   NOTE:  by default, VTEC iTool Tests are run in operational mode i.e. -v O (and no -T option)
##             (unless 'vtecMode" is specified)
##          At some point, we should make sure they run in PRACTICE mode as well
##             (for NGIT testing)


import sys, time, os, types, copy, inspect, errno
import LogStream
import AbsTime, TimeRange
import numpy, cPickle

OUTPUT_DIR = "/tmp/products/autoTest"

# Triggers can be:
#   Message enums and executeMsg will have the message as its argument

#Triggers = [AFPS.Message.PROCESS_STATUS]
HideTool = 0
InitialActivation = 1

### If desired, Set up variables to be solicited from the user:
VariableList = [
         ("Test Script Name", [], "check",
          [
           "CreateGrids",
           "RoutineLevel1_TestScript",
           "RoutineLevel1_Baseline_TestScript",
           "RoutineLevel1_Region_TestScript ",
           "RoutineLevel1_AFD_TestScript",
           "RoutineLevel1_AFM_TestScript",
           "RoutineLevel1_CCF_TestScript",
           "RoutineLevel1_FWF_TestScript",
           "RoutineLevel1_FWFTable_TestScript",
           "RoutineLevel1_FWS_TestScript",
           "RoutineLevel1_GLF_TestScript",
           "RoutineLevel1_PFM_TestScript",
           "RoutineLevel1_SAF_TestScript",
           "RoutineLevel1_SFT_TestScript",
           "RoutineLevel1_SRF_TestScript",
           "RoutineLevel1_ZFP_TestScript",
           "RoutineLevel2_1_TestScript",
           "RoutineLevel3_1_TestScript",
           "RoutineLevel3_2_TestScript",
           "RoutineLevel3_3_TestScript -- Local Effects",
           "RoutineLevel4_1_TestScript -- More Local Effects",
           "RoutineLevel5_1_TestScript -- SnowAccum, PopWx",
           "RoutineLevel5_2_TestScript -- Miscellaneous Product Tests",
           "RoutineLevel5_3_TestScript -- Miscellaneous Phrase Tests",
           "RoutineLevel5_4_TestScript -- Temp Phrases",
           "SPW_1_TestScript -- Automatically generated tests for SPW",
           "AllowedHazards_TestScript",
           "ExpireTime_TestScript",
           "ExpireAlg_TestScript",
           "   ",
           "CivilEmerg_TestScript",
           "Hazards_TestScript",
           "Hazard_HLS_TestScript",
           "Hazards_Overview_Options_TestScript",
           "HazardsComplex1_TestScript -- CFW",
           "VTEC_GHG_Complex1_TestScript -- WSW",
           "VTEC_GHG_Complex2_TestScript",
           "VTEC_GHG_Complex3_TestScript",
           "VTEC_GHG_Complex4_TestScript",
           "VTEC_GHG_Complex5_TestScript",
           "VTEC_GHG_Complex6_TestScript",
           "VTEC_GHG_WCN_TestScript",
           "VTEC_GHG_FFA_TestScript",
           "VTEC_GHG_GenHaz_TestScript",
           "HeadlinesTiming_Watch_TestScript",
           "HeadlinesTiming_Warn_TestScript",
           "VTEC_EXP_NEW_TestScript",
           "VTEC_CrossingYear_TestScript",
           "VTEC_TestMode_TestScript",
           "VTEC_EXTtoNOW_TestScript",
           "VTEC_EXT_UPG_TestScript",
           "VTEC_GHG_UPG_SplitETNs_TestScript",
           "VTEC_ETN_RESET_Tmode_TestScript",
           "VTEC_ETN_Reuse_TestScript",
           "VTEC_Reset_Start_TestScript",
           "MultipleTZ_TestScript",
           "Headline_UPG_TestScript",
           "HeadlineSort_TestScript",
           "VTEC_Hazard_DR21021_TestScript",
           ]),
         ("Enter Test Script Name" , "", "alphaNumeric"),
         ("Output File           " , "/tmp/TestResults.txt", "alphaNumeric"),
         ("Failure Limit         ", 0, "numeric"),
         ("Test Suite", "Run All Tests", "radio", ["Run All Tests", "Individual Tests"]),
         ("Reporting Mode" , "Verbose", "radio", ["Verbose", "Moderate", "Brief", "Pretty"]),
         ("Run\nSetupTextEA?" , "no", "radio", ["yes", "no"]),
         ("Processor?" , "Local", "radio", ["Server", "Local"]),
         ("Create Grids?" , "yes", "radio", ["yes", "no"]),
         ("Leave File\nChanges?" , "no", "radio", ["yes", "no"]),
        ]

# Set up Class
import ISmartScript
import ProcessVariableList

from com.raytheon.viz.gfe.textformatter import TextProductFinishWaiter, FormatterUtil, TextProductManager
from com.raytheon.viz.gfe.smarttool import TextFileUtil
from com.raytheon.viz.gfe.dialogs.formatterlauncher import ConfigData
ProductStateEnum = ConfigData.ProductStateEnum
from com.raytheon.uf.common.activetable import ActiveTableMode

class ProcessInfo:
    def __init__(self, entry, name, pid, script):
        self.__entry = entry
        self.__name = name
        self.__pid = pid
        self.__script = script
    def entry(self):
        return self.__entry
    def name(self):
        return self.__name
    def pid(self):
        return self.__pid
    def script(self):
        return self.__script
    
class ITool (ISmartScript.ISmartScript):
    def __init__(self, dbss):
        ISmartScript.ISmartScript.__init__(self, dbss)        
        self._dataMgr = dbss
        self._process = None            

    # Button 1 in ITool Dialog
    def execute(self, varDict):
        "Testing for Text Products"
        self._failLimit = varDict["Failure Limit         "]
        self._outFile = open(varDict["Output File           "], 'w')
        self._reportingMode = varDict["Reporting Mode"]
        if self._reportingMode not in ["Pretty"]:
            self.output("Calling TextProductTest User Invoked", self._outFile)
        self._timer = time.time()
        self._testSuite = varDict["Test Suite"]
        self._lastCreateGrids = None
        self._createGridsRunTime = varDict["Create Grids?"]
        self._leaveFileChanges = varDict["Leave File\nChanges?"]
        processor = varDict["Processor?"]
        setupTextEA = varDict["Run\nSetupTextEA?"]
        if setupTextEA == "yes":
            if self._reportingMode not in ["Pretty"]:
                self.output("Calling setupTextEA", self._outFile)
            import SetupTextEA
            SetupTextEA.main()
        scriptNames = varDict["Test Script Name"]
        enterName = varDict["Enter Test Script Name"]
        if enterName:
            scriptNames.append(enterName)
        if scriptNames == []:
            return
        self._testScript = []
        for scriptName in scriptNames:
            scriptName = scriptName.split(" -- ")[0]
            scriptName = scriptName.strip()
            if len(scriptName) == 0:
                continue
            
            if sys.modules.has_key(scriptName):
                del sys.modules[scriptName]
            exec "import " + scriptName
            exec "testScript = " + scriptName + ".testScript(self, self._dataMgr)"
            self._testScript = self._testScript + testScript
        self._process = None
        self._passed = 0
        self._failures = 0
        self._scripts = 0
        self._scriptName = `scriptNames`

        # Let User Choose Individual Tests
        if self._testSuite == "Individual Tests":
            success = self._pareTestScript()
            if success is None:
                return

        # Run the test scripts
        for index in range(len(self._testScript)):
            self._runTestScript(index)
            if self._failures > self._failLimit:
                break
            time.sleep(2) # avoid some race conditions with fileChanges

        self._finished()

        
        # We will wait for it to finish before incrementing index and running the
        # next script  (see executeMsg below)

    def _pareTestScript(self):
        newScript = []
        nameList = []
        for entry in self._testScript:
            nameList.append(entry["name"])
        varList = [("Choose Tests", [], "check", nameList)]
        processVarList = ProcessVariableList.ProcessVariableList(
            "Choose Individual Tests", varList, varDict={}, parent=None)
        self._selectionStatus = processVarList.status()
        if not self._selectionStatus == "OK":
            return None   # User Cancelled
        varDict = processVarList.varDict()
        testList = varDict["Choose Tests"]
        for entry in self._testScript:
            if entry["name"] in testList:
                newScript.append(entry)
        self._testScript = newScript
        return 1

    def _runTestScript(self, index):
        entry = self._testScript[index]
        
        # Set defaults
        database, user, checkMethod, checkStrings = self._setDefaults()
        # Process entry
        name = entry["name"]
        productType = entry["productType"]
        database = entry.get("database", database)
        user = entry.get("user", user)
        # gridsStartTime
        self._gridsStartTime = entry.get("gridsStartTime", None)
        if self._gridsStartTime is None:
            self._gridsStartTime = self.getTimeRange("Today").startTime()
        else:
            if not isinstance(self._gridsStartTime, AbsTime.AbsTime):
                self._gridsStartTime = self.getAbsTime(self._gridsStartTime)
        #print "gridsStartTime", self._gridsStartTime
        # drtTime
        drtTime = entry.get("drtTime")
        if drtTime is None:
            drtTime = entry.get("gridsStartTime", None)
        #print "\n*********drtTime", drtTime
        if drtTime is not None:
            if isinstance(drtTime, AbsTime.AbsTime):
                drtTime = self.getTimeStr(drtTime)
            drtStr = drtTime
            self._drtString = drtTime
        else:
            drtStr = ""
            self._drtString = None
        #print "drtStr", drtStr        


        self._clearHazardsTable(entry)
        self._createCombinationsFile(entry)
        self._deleteGrids(entry)
        self._createGrids(entry)
        self._makeWritableCopy(entry)
        self._fileChanges(entry)
        
        cmdLineVars = self._getCmdLineVars(entry)
        vtecMode = entry.get("vtecMode", None)

        if productType is None:
            return

        database = database.replace("<site>", self.getSiteID())

        # Run the product
        if self._reportingMode not in ["Pretty"]:
            self.output("Running " + name, self._outFile)
        message = "Running " + name
        self.statusBarMsg(message, "R", category="GFE")
           
        # this way goes through java in separate threads, debugging doesn't work with it cause each
        # thread has its own interpreter....     
        # however, running the other way has issue with sampler caches not getting dumped between runs
        waiter = TextProductFinishWaiter()
        FormatterUtil.runFormatterScript(productType, vtecMode, database, cmdLineVars, "PRACTICE", drtTime, 0, waiter, self._dataMgr)
        fcst = waiter.waitAndGetProduct()
        state = waiter.getState()
            
#         import FormatterRunner
#         try:
#             fcst = FormatterRunner.runFormatter(databaseID=database, site="TBW",
#                                      forecastList=[productType], cmdLineVarDict=cmdLineVars,
#                                      vtecMode=vtecMode, vtecActiveTable='PRACTICE', drtTime=drtTime,
#                                      username='GFETEST', dataMgr=self._dataMgr)
#         except:
#             fcst = ''
#             LogStream.logProblem("Error generating product: " + LogStream.exc())

        # write product to OUTPUT_DIR
        
        fname = name + ".txt"
        path = os.path.join(OUTPUT_DIR, fname)
        with open(path, 'w') as out:
            out.write(fcst)

        self._doExecuteMsg(name, fcst, entry, drtTime, state)        
                    
    def _getCmdLineVars(self, entry):
        cmdLineVars = entry.get("cmdLineVars", None)
        if cmdLineVars is None:
            return ""
        # See if this is a dictionary
        try:
            exec "varDict = " + cmdLineVars
        except:
            # Process as method
            # Otherwise, get the varDict from calling the given method
            callMethod = cmdLineVars
            productType = entry["productType"]
            if sys.modules.has_key(productType):
                del sys.modules[productType]            
            module = __import__(productType)
            exec "callMethod = module.TextProduct()." + callMethod
            definition = module.TextProduct().Definition
            defVars = entry.get("callVariables", None)
            if defVars is not None:
                for key in defVars.keys():
                    definition[key] = defVars[key]
            varDict = callMethod(definition)
            #for key in varDict.keys():
            #    print "varDict['"+str(key)+"'] =", varDict[key]
            cmdLineVars = `varDict`
        if cmdLineVars is not None:
            return cmdLineVars
        else:
            return ""


    def _setDefaults(self):
        database = self.getSiteID() + "_GRID__Fcst_00000000_0000"
        user = "GFETEST"
        return database, user, None, None

    def _clearHazardsTable(self, entry):
        clearHazards = entry.get("clearHazardsTable", 0)
        if clearHazards:
            if self._reportingMode not in ["Pretty"]:
                self.output("WARNING::Clearing Hazards Table", self._outFile)
            self._dataMgr.getClient().clearVTECTable(ActiveTableMode.PRACTICE)

    def _createCombinationsFile(self, entry):
        fn = entry.get("combinationsFileName", None)
        combinations = entry.get("combinations", None)
        if fn is None or combinations is None:
            return
        fn = fn.replace("<site>", self.getSiteID())
        self.writeActiveComboFile(combinations, fn)

    def _createGrids(self, entry):
        createGrids = entry.get("createGrids", None)
        if createGrids is None:
            return
        if self._createGridsRunTime == "no":
            return
        # Check to see if the last time we created grids we used the same list
        # If so, do not repeat
        if self._lastCreateGrids == createGrids:
            return
        self._lastCreateGrids = createGrids
        
        wxKeys = []
        hazKeys = []
        gridsTR = TimeRange.TimeRange(self._gridsStartTime, self._gridsStartTime + 12 * 3600)
        self._determineMaxMinBeginEnd(entry)    
        hazardGrid = None
        createdGrids = {}
        for gridEntry in createGrids:
            if len(gridEntry) == 7:
                model, elementName, elementType, startHour, endHour, value, editAreas = gridEntry
                defValue = 0
            elif len(gridEntry) == 8:
                model, elementName, elementType, startHour, endHour, value, editAreas, defValue = gridEntry
            else:
                #print "GridEntries: ", gridEntry
                raise Exception("Improper # of Grid Entries")
            startHour = self._translateHour(startHour)
            endHour = self._translateHour(endHour)
            timeRange = TimeRange.TimeRange(gridsTR.startTime() + startHour * 3600,
                                gridsTR.startTime() + endHour * 3600)
            #self.output("element name, type " + elementName + " " + elementType, self._outFile)
            #self.output("startHour, endHour " + `startHour` +" "+`endHour`, self._outFile)
            #self.output("   timeRange "+`timeRange`, self._outFile)
            # Get the grid we already created, if it exists
            key = (model, elementName, startHour, endHour)
            if createdGrids.has_key(key):
                grid = createdGrids[key]
            else:
                grid = self.newGrid(defValue)

            if editAreas == "all":
                mask = self.newGrid(True, bool)
            else:
                mask = self._makeMask(editAreas)
            #self.output("mask "+`size(mask)`, self._outFile)
            #self.output("grid "+`size(grid)`, self._outFile)
            #self.output("value "+`value` , self._outFile)
            if elementType == "DISCRETE":
                #self._addHazard(elementName, timeRange, value, mask)
                value = self.getIndex(value, hazKeys)
                #self.output("setting value "+value+" "+hazKeys, self._outFile)
                grid[mask] = value
                grid = grid.astype('int8')
                elementType = self.getDataType(elementName)
                self.createGrid(model, elementName, elementType, (grid, hazKeys), timeRange)
            elif elementType == "WEATHER":
                if value == "NoWx":
                    value = "<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
                value = self.getIndex(value, wxKeys)
                #self.output("setting value "+value+" "+wxKeys, self._outFile)
                grid[mask] = value
                grid = grid.astype('int8')
                elementType = self.getDataType(elementName)
                self.createGrid(model, elementName, elementType, (grid, wxKeys), timeRange)
            elif elementType == "VECTOR":
                grid[mask] = value[0]
                dirGrid = self.empty()
                dirGrid[mask] = self.textToDir(value[1])
                elementType = self.getDataType(elementName)
                self.createGrid(model, elementName, elementType, (grid, dirGrid), timeRange)
            else:
                grid[mask] = value
                elementType = self.getDataType(elementName)
                self.createGrid(model, elementName, elementType, grid, timeRange)
            # Save the grid in the createdGridDict
            createdGrids[key] = grid            
            self.saveElements([elementName], model)
            if entry.get("publishGrids", 0):
                self.publishElements([elementName], timeRange)
            #LogStream.logDebug("Created grid: ", key)

    def _makeWritableCopy(self, entry, user='GFETEST'):
        writables = entry.get("writeableCopies", None)
        if writables is None:
            return
        failed = 0
        for fileSrc, fileType, destFilename in writables:
            source = fileSrc.replace("<site>", self.getSiteID())
            dest = destFilename.replace("<site>", self.getSiteID())
            #try:
            if 1 == 1:
                TextFileUtil.makeWritableCopy(source, fileType,
                  dest, False);
                self.output("Made makeWritableCopy: " + source + ' ' + \
                  fileType + ' ' + dest, self._outFile)
            #except:
            else:
                failed = failed + 1
                self.output("failed makeWritableCopy: " + source + ' ' + \
                  fileType + ' ' + dest, self._outFile)

        if failed == 0:
            if self._reportingMode not in ["Pretty"]:
                self.output("All Writable Copies successful", self._outFile)
                
    def _fileChanges(self, entry):
        fileChanges = entry.get("fileChanges", None)
        if not fileChanges:
            return False

        from LockingFile import File
        
        failed = 0        
        for fileName, fileType, changeType, strings, cleanUp in fileChanges:
            fileName = fileName.replace("<site>", self.getSiteID())
            # Get the file
            lf = TextFileUtil.getTextFile(fileName, fileType)
            if lf.getName().endswith(".py"):
                if sys.modules.has_key(fileName):
                    del sys.modules[fileName]
            try:
                with File(lf.getFile(), '', 'r') as pythonFile:
                    text = pythonFile.read()
            except:
                failed = 1
                print "FILE CHANGES failed reading from " + str(lf)
                raise
            #self.output("FILE CHANGES (initial) from " +str(lf) + "\n" + text, self._outFile) #DEBUG

            # Modify it
            if changeType == "add":
                text = text + strings
            elif changeType == "replace":
                # strings may be a tuple (orig, repl) or 
                # a list of tuples for multiple changes to the same file 
                if type(strings) == tuple:
                    strings = [strings]
                for orig, repl in strings:
                    strIndex = text.find(orig)
                    text = text.replace(orig, repl)

                    #self.output("FILE CHANGES (chg): " + orig + ' ' + repl, self._outFile) #DEBUG
                    #self.output("FILE CHANGES (mod): " +  text, self._outFile) #DEBUG
                    
                    if strIndex < 0:
                        self.output("File change failed for " + orig,
                          self._outFile)
                        failed = 1
            # Write it
            destLf = TextFileUtil.getUserTextFile(lf)
            try:
                with File(destLf.getFile(), '', 'w') as pythonFile:
                    pythonFile.write(text)

                destLf.save()

            except:
                failed = 1
                print "FILE CHANGES failed writing to " + str(destLf)
                raise      
            #self.output("FILE CHANGES (saved) to " + str(destLf) + "\n" +  text, self._outFile) #DEBUG         

        if len(fileChanges) and not failed:            
            if self._reportingMode not in ["Pretty"]:
                self.output("All File Changes successful", self._outFile)
        return True

    def _determineMaxMinBeginEnd(self, entry):
        # Determine MaxT MinT MaxRH MinRH begin and end times
        # relative to gridsStartTime
        localtime = time.localtime(self._gridsStartTime.unixTime())
        localHour = localtime[3]
        if localtime[8]:  # daylight
            maxBegin = 8
        else:
            maxBegin = 7
        self._MaxTBegin = maxBegin - localHour # MaxT begins at 7 am standard time
        self._MaxTEnd = self._MaxTBegin + 13
        self._MinTBegin = self._MaxTBegin + 12
        self._MinTEnd = self._MaxTBegin + 12 + 14
        
        self._MinRHBegin = maxBegin - 4 - localHour # MinRH begins at 3 am standard time
        self._MinRHEnd = self._MinRHBegin + 18
        self._MaxRHBegin = self._MinRHBegin + 12
        self._MaxRHEnd = self._MinRHBegin + 12 + 18

    def _translateHour(self, hour):
        if type(hour) is not types.StringType:
            return hour
        # Suppose hour == "MaxTBegin + 24" and self._MaxTBegin == 1
        for tStr in ["MaxTBegin", "MaxTEnd", "MinTBegin", "MinTEnd",
                     "MaxRHBegin", "MaxRHEnd", "MinRHBegin", "MinRHEnd"]:
            if hour.find(tStr) >= 0:
                exec "tHour = self._" + tStr  # tHour = self._MaxTBegin
                hour = hour.replace(tStr, `tHour`) # hour == "1 + 24"
                exec "newHour = " + hour
                return newHour

    def _deleteGrids(self, entry):
        deleteGrids = entry.get("deleteGrids", None)
        if deleteGrids is None or deleteGrids == []:
            return
        self._lastCreateGrids = []  #clear it after deleting grids
        for gridEntry in deleteGrids:
            model, elementName, level, startHour, endHour = gridEntry
            if startHour == "all" or endHour == "all":
                timeRange = TimeRange.allTimes()
            else:
                gridsTR = TimeRange.TimeRange(self._gridsStartTime, self._gridsStartTime + 12 * 3600)
                timeRange = TimeRange.TimeRange(gridsTR.startTime() + startHour * 3600,
                                gridsTR.startTime() + endHour * 3600)
            self.deleteGrid(model, elementName, level, timeRange)
            self.saveElements([elementName], model)
            if entry.get("publishGrids", 0):
                self.publishElements([elementName], timeRange)
            
    # Required if Message is a trigger  
    def executeMsg(self, msg):
        if self._process is None:
            return
        status = msg.status()  #list of messages
        for msg in status:
            if msg.status() == AFPS.ProcessStatus.FINISHED:
                self._doExecuteMsg(msg)  #call for each possible message

    # Performs the processing
    def _doExecuteMsg(self, name, fcst, entry, drtTime, state):
        if self._reportingMode not in ["Pretty"]:
            self.output("Calling TextProductTest Message Invoked " + `entry`, self._outFile)
            
        checkMethod = entry.get("checkMethod", None)
        checkStrings = entry.get("checkStrings", None)
        notCheckStrings = entry.get("notCheckStrings", None)
        orderStrings = entry.get("orderStrings", None)
        internalStrip = entry.get("internalStrip", 1)
        commentary = entry.get("commentary", None)
                    
        if True:
            # Clean up fileChanges
            self._cleanUpFiles(entry)
            self._cleanUpWritableCopies(entry)
            
            self.output("\n----------------------------------------------", self._outFile)

            if self._reportingMode not in ["Pretty"]:
                self.output(name + "    (Elapsed time:" + self._getElapsedTimeStr() + ")",
                            self._outFile)
            else:
                self.output(name, self._outFile)
            if commentary is not None:
                self.output(commentary + "\n", self._outFile)
            self._scripts += 1

            if state.equals(ProductStateEnum.Failed): 
                self.output("Formatter failed!", self._outFile)
                success = False

            else:        
                # Look at results
                # If any of the check fails, the test fails
                check1 = 1
                check2 = 1
                check3 = 1
                if checkMethod is not None:
                    check1 = checkMethod(fcst)
                    if self._reportingMode not in ["Pretty"]:
                        if not check1:
                            failMsg = "CHECK METHOD FAILED:" + name
                            self.output(failMsg, self._outFile)
                        else:
                            self.output("CHECK METHOD PASSED: " + name, self._outFile)
                            
                # Prepare results for string searches
                if fcst is not None:
                    fcstStr = fcst.replace("\n", " ")
                    fcstStrRaw = fcstStr
                    if internalStrip:
                        fcstStr = self.internalStrip(fcstStr)
                    fcstStr = fcstStr.replace("... ", "...")
                    fcstStrRaw = fcstStrRaw.replace("... ", "...")
                    
                    if checkStrings is not None:
                        check2 = self._checkStrs(name, fcst, checkStrings,
                          orderStrings, fcstStr, fcstStrRaw, internalStrip)
                        if check2:
                            if self._reportingMode not in ["Pretty"]:
                                self.output("STRING SEARCHES PASSED ", self._outFile)

                    if notCheckStrings is not None:
                        check3 = self._checkStrs(name, fcst, notCheckStrings, 0,
                          fcstStr, fcstStrRaw, internalStrip, checkMode=0)
                        if check3:
                            if self._reportingMode not in ["Pretty"]:
                                self.output("'NOT' STRING SEARCHES PASSED ", self._outFile)

                success = check1 and check2 and check3
                
            if success:
                self._passed += 1
                logmsg = name + " Passed"
                self.statusBarMsg(logmsg, "R", category="ISC")
            else:
                self._failures += 1
                logmsg = name + " Failed"
                self.statusBarMsg(logmsg, "A", category="ISC")
            self.output(logmsg, self._outFile)
            
            if self._failures > self._failLimit:
                self._cleanUp(entry, drtTime)
                # Stop processing
                return
            if self._reportingMode in ["Verbose", "Moderate"]:
                if fcst and success: # checkStrings failure will print out the product already
                    self.output("\n" + fcst, self._outFile)
            # DecodeVTEC if requested
            # Note for later:  if in practice mode, set active
            # table to runVTECDecoder("PRACTICE", fcst)            
            if success and entry.get("decodeVTEC", 0):                
                self.__runVTECDecoder(fcst, drtTime)

                # wait until table has been modified or 5 seconds
#                count = 0
                t1 = time.time();
#                time.sleep(0.1);
                time.sleep(2)
#                while count < 50:
#                    modTime1 = self._dataMgr.ifpClient().vtecTableModTime(
#                      "PRACTICE")
#                    if modTime1 != modTime:
#                        break
#                    time.sleep(0.1)
#                    count = count + 1
                t2 = time.time();
                if self._reportingMode in ["Verbose", "Moderate"]:
                    self.output("Vtec Decoder wait time: " + "%6.2f" % (t2-t1),
                      self._outFile)

            self._cleanUp(entry, drtTime)
    
    def _cleanUp(self, entry, drtTime):
        if drtTime is not None:
            import offsetTime           
            offsetTime.reset()
            reload(offsetTime)
            
#         fileChanges = entry.get("fileChanges", [])
#         for fileName, fileType, changeType, strings, cleanUp in fileChanges:
#             fileName = fileName.replace("<site>", self.getSiteID())
#             reload(sys.modules[fileName])
#         productType = entry['productType']
#         if sys.modules.has_key(productType):
#             del sys.modules[productType]            

    def _cleanUpWritableCopies(self, entry, user="GFETEST"):
        writables = entry.get("writeableCopies", None)
        if writables is None:
            return
        for fileSrc, fileType, destFilename in writables:
            source = fileSrc.replace("<site>", self.getSiteID())
            dest = destFilename.replace("<site>", self.getSiteID())
            #try:
            if 1 == 1:
                TextFileUtil.makeWritableCopy(source, fileType,
                  dest, True);
            #except:
            else:
                pass
            self.output("Cleanup writable copies: " + fileSrc, self._outFile)

    def _cleanUpFiles(self, entry):        
        fileChanges = entry.get("fileChanges", [])
        for fileName, fileType, changeType, strings, cleanUp in fileChanges:
            fileName = fileName.replace("<site>", self.getSiteID())
            textFileID = TextFileUtil.getTextFile(fileName, fileType)            
            if self._leaveFileChanges == "no":
                if cleanUp in ["delete", "undo"]:
                    # File changes are made as overrides at the GFETEST user level
                    # We just remove these files to restore the previous file
                    destLf = TextFileUtil.getUserTextFile(textFileID)                  
                    TextFileUtil.deleteTextFile(destLf)

    def _checkStrs(self, name, fcst, checkStrings, orderStrings, fcstStr,
      fcstStrRaw, internalStrip, checkMode=1):
        # Check the fcstStr for the list of checkStrings
        # If a checkString is a tuple, at least one of the
        # given tuple strings must be found in the fcstStr (or fcstStrRaw)
        # If orderStrings == 1, the strings must occur in order
        # in the fcstStr
        # If checkMode == 0, the strings should NOT be found in the fcstStr
        # If internalStrip == 2, check both the fcstStr, and fcstStrRaw 
        #  versions. If at least one succeeds, the checkString succeeds.
        curIndex = -1
        for cStr in checkStrings:
            if type(cStr) == types.TupleType:
                # Will pass if ANY of these strings are found

                # Not valid with checkMode of zero
                if not checkMode:
                    continue

                found = 0
                for subStr in cStr:
                    found, strIndex, strIndexFlag = self._checkStr(
                        subStr, fcstStr, fcstStrRaw, internalStrip)
                    if found:
                        if self._reportingMode in ["Verbose"]:
                            self.output("StringCHECK: " + subStr + ' ' + `strIndex`, self._outFile)
                        elif self._reportingMode in ["Pretty"]:
                            self.output("CHECK String: " + subStr, self._outFile)
                        break
                if not found:
                    self._failed(subStr, name, fcst, fcstStr, checkMode)
                    return 0
            else:
                # Must find exact string
                found, strIndex, strIndexFlag = self._checkStr(cStr, fcstStr, fcstStrRaw, internalStrip)
                if self._reportingMode in ["Verbose"]:
                    self.output("StringCHECK: " + cStr + ' ' + `strIndex`, self._outFile)
                elif self._reportingMode in ["Pretty"]:
                    self.output("CHECK String: " + cStr, self._outFile)
                if strIndex == -1:
                    # Handle special case of SHOWERS/RAIN SHOWERS
                    if cStr.find("showers") >= 0:
                        cStr = cStr.replace("showers", "rain showers")
                        found, strIndex, strIndexFlag = self._checkStr(
                            cStr, fcstStr, fcstStrRaw, internalStrip)
                    if cStr.find("Showers") >= 0:
                        cStr = cStr.replace("Showers", "Rain showers")
                        found, strIndex, strIndexFlag = self._checkStr(
                            cStr, fcstStr, fcstStrRaw, internalStrip)
                    if strIndex < 0:
                        if checkMode:
                            self._failed(cStr, name, fcst, fcstStr, checkMode)
                            return 0
                    else:
                        if not checkMode:
                            self._failed(cStr, name, fcst, fcstStr, checkMode)
                            return 0
                elif checkMode == 0: #not check strings, and not check mode
                    self._failed(cStr, name, fcst, fcstStr, checkMode)
                    return 0

            # Check the ordering
            if orderStrings:
                if strIndexFlag == "raw":
                    fcstStrRaw = fcstStrRaw[strIndex:]
                else:
                    fcstStr = fcstStr[strIndex:]
        return 1

    def _checkStr(self, subStr, fcstStr, fcstStrRaw, internalStrip):
        # Check for the given string.
        # If internalStrip is 2, check both the raw and stripped fcstStrs.
        found = 0
        strIndexFlag = "regular"
        strIndex = fcstStr.find(subStr)
        if strIndex >= 0:
            found = 1
        elif internalStrip == 2:
            # Try the raw fcstStr
            strIndex = fcstStrRaw.find(subStr)
            if strIndex >= 0:
                found = 1
                strIndexFlag = "raw"
        return found, strIndex, strIndexFlag

    def _failed(self, str, name, fcst, fcstStr, checkMode):
        failMsg = "STRING SEARCH FAILED: " + name
        if checkMode:
            errorStr = "Cannot find: "
        else:
            errorStr = "Found and should not have found: "
        failMsg = failMsg + "   " + errorStr + `str`
        failMsg = failMsg + "   Product Result \n" + fcst
        self.output(failMsg, self._outFile)

    def _finished(self):
        message = "TESTING COMPLETE " + \
          self._scriptName + " \n" + \
          self._getElapsedTimeStr() + \
          "\n" + `self._scripts` + " SCRIPTS RUN.\n" + \
          `self._passed` + " TESTS PASSED.\n" + \
          `self._failures` + " TESTS FAILED."
      
        self.output(message, self._outFile)
        self._outFile.close()

        #send user alert message to GFEs
        if self._failures:
            status = "U"
        else:
            status = "S"
        self.statusBarMsg(message, status, category="GFE")        

    def _getElapsedTimeStr(self):
        eTime = (time.time() - self._timer) / 60.0
        return "%4.2f" % eTime + " minutes"
              
    # Optional -- Invoked by Button 2 in ITool Dialog
    def cleanUp(self):
        self.output("Calling MyTool CleanUp method", self._outFile)
        # Can be called at any time to abort the script
        self._testScript = []
        self._outFile.close()
        
    def __runVTECDecoder(self, fcst, drtString=None):
        import tempfile, urlparse
        from com.raytheon.uf.viz.core import VizApp
        
        with tempfile.NamedTemporaryFile(mode='w', prefix="autoTestProd", delete=False) as file:
            file.write(fcst)
        
        url = urlparse.urlparse(VizApp.getHttpServer())
        commandString = "VTECDecoder -f " + file.name + " -d -a practice -h " + url.hostname
        if drtString is not None:
            commandString += " -z " + drtString
        os.system(commandString)

def main():
    os.environ["TZ"] = 'EST5EDT'
    time.tzset()
    import _strptime
    _strptime._cache_lock.acquire()
    _strptime._TimeRE_cache = _strptime.TimeRE()
    _strptime._regex_cache = {}
    _strptime._cache_lock.release()

    from java.lang import System
    System.setProperty('user.name', 'GFETEST')
    
    from com.raytheon.uf.viz.core.localization import LocalizationManager
    from com.raytheon.uf.common.localization import LocalizationContext
    LocalizationLevel = LocalizationContext.LocalizationLevel
    LocalizationManager.registerContextName(LocalizationLevel.USER, 'GFETEST');
    
    import loadConfig
    loadConfig.loadPreferences("gfeConfig")
    
    from com.raytheon.viz.gfe.core import DataManager
    dm = DataManager.getInstance(None)
    import IToolInterface
    # have to add the user dir to the python path here since we just switched users
    # TODO look into switching it from the java
    from com.raytheon.uf.common.dataplugin.gfe.python import GfePyIncludeUtil

    for s in sys.path:
        if 'textUtilities' in s \
        or 'textProducts' in s \
        or 'combinations' in s:
            sys.path.remove(s) 

    for s in str(GfePyIncludeUtil.getHeadlineIncludePath()).split(':'):
        sys.path.append(s)

    for s in str(GfePyIncludeUtil.getTextUtilitiesIncludePath()).split(':'):
        sys.path.append(s)

    for s in str(GfePyIncludeUtil.getTextProductsIncludePath()).split(':'):
        sys.path.append(s)
    
    for s in str(GfePyIncludeUtil.getCombinationsIncludePath()).split(':'):
        sys.path.append(s)
    
    # create output directory for products
    try:
        os.makedirs(OUTPUT_DIR)
    except OSError, e:
        if e.errno != errno.EEXIST:
            self.output("%s: '%s'" % (e.strerror,e.filename))
    
    scriptDir = GfePyIncludeUtil.getIToolIncludePath()
    runner = IToolInterface.IToolInterface(scriptDir)
    runner.instantiate('TextProductTest', 'ITool', dbss=dm)    
    processVariableList = ProcessVariableList.ProcessVariableList('TextProductTest', VariableList)
    varDict = processVariableList.varDict()
    if varDict is None or len(varDict) == 0:
        return    
    runner.runITool('TextProductTest', 'ITool', 'execute', varDict=varDict)


if __name__ == "__main__":
    main()
