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
# runProcedure.py
# Main program and class for running Procedures from the command line
#
# Author: hansen
# ----------------------------------------------------------------------------

import getopt
import sys
import time

import TimeRange, AbsTime, LogStream
import loadConfig
import MasterInterface
import Exceptions

from java.lang.System import getProperty
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.viz.gfe.core import DataManager


CLASS_NAME = 'Procedure'
METHOD_NAME = 'execute'


class ProcedureRunner(MasterInterface.MasterInterface):
    def __init__(self, procName):
        MasterInterface.MasterInterface.__init__(self)
        self.addModule(procName)
    
    def runProcedure(self, moduleName, className, methodName, **kwargs):
        try:
             return self.runMethod(moduleName, className, methodName, **kwargs)
        except Exceptions.EditActionError, e:
            if "Cancel" == e.errorType() and "Cancel" == e.errorInfo():
                return None
            msg = moduleName + ":" + e.errorType() + ": " + e.errorInfo()
            raise RuntimeError(msg)
    
    def run(self, dataMgr, moduleName, className, methodName, varDict=None, editArea=None, timeRange=None):
        tr = None
        if timeRange:
            tr = timeRange.toJavaObj()
        preview = dataMgr.getEditActionProcessor().prepareExecute("Procedure", moduleName, editArea, tr, False)
        
        procArgNames = self.getMethodArgs(moduleName, CLASS_NAME, METHOD_NAME)
                
        procArgs = {}
        for arg in procArgNames:
            if arg == 'varDict':
                procArgs['varDict'] = varDict
            if arg == 'editArea':
                procArgs['editArea'] = editArea
            if arg == 'timeRange':
                procArgs['timeRange'] = timeRange
                
        try:
            self.runProcedure(moduleName, CLASS_NAME, METHOD_NAME, **procArgs)
        finally:
            # FIXME: This sleep() call is a timing hack
            # There seems to be a small delay in processing unlock notifications
            # and calling wrapUpExecute() was trying to send save requests for
            # grids in the process of already being unlocked
            time.sleep(1.5)
            dataMgr.getEditActionProcessor().wrapUpExecute(preview, False)
        
        
class RunProcedure:
    def __init__(self, procName, host, port, userName,
                 configFile, startTime, endTime, timeRange, editArea,
                 mutableModel, varDict):
        
        # import the config file
        prefs = loadConfig.loadPreferences(configFile)
        
        LogStream.logEvent("Configuration File: ", configFile)
        
        if mutableModel is None:
            mutableModel = prefs.getString('mutableModel')
        else:
            prefs.setValue('mutableModel', mutableModel)

        self.__dataMgr = DataManager.getInstance(None)                

        # Create Time Range
        if startTime is not None and endTime is not None:
            start = self.getAbsTime(startTime)
            end = self.getAbsTime(endTime)
            self.__timeRange = TimeRange.TimeRange(start, end)
        elif timeRange is not None:
            self.__timeRange = TimeRange.TimeRange(self.__dataMgr.getSelectTimeRangeManager().getRange(timeRange).toTimeRange());
        else:
            self.__timeRange = TimeRange.default()

        if editArea is not None:
            refID = ReferenceID(editArea)
            self.__editArea = \
                 self.__dataMgr.getRefManager().loadRefSet(refID)
        else:            
            self.__editArea = self.__dataMgr.getRefManager().emptyRefSet()                    

        LogStream.logVerbose("varDict=",varDict)
        
        runner = ProcedureRunner(procName)        

        errors = runner.getImportErrors()
        if len(errors) > 0:
            print "Error importing the following procedures:\n"
            for s in errors:
                print s
            sys.exit(1)
        
        runner.instantiate(procName, CLASS_NAME, **{'dbss':self.__dataMgr})        
        runner.run(self.__dataMgr, procName, CLASS_NAME, METHOD_NAME, varDict, self.__editArea, self.__timeRange)                    

    def getAbsTime(self, timeStr):
        "Create an AbsTime from a string: YYYYMMDD_HHMM"

        year = int(timeStr[0:4])
        month = int(timeStr[4:6])
        day = int(timeStr[6:8])
        hour = int(timeStr[9:11])
        minute = int(timeStr[11:13])

        return AbsTime.absTimeYMD(year, month, day, hour, minute)

def usage():
    print "Usage: gfe runProcedure.py "
    print "             -n procedureName "
    print "             -u userName"
    print "             -c configFile"
    print "             [-h host]"
    print "             [-p port]"
    print "             [-a editAreaName]"
    print "             [-s startTime -e endTime]  OR"
    print "             [-t timeRange]"
    print "             [-m mutable database]"
    print "             [-z displaced real time -- format YYYYMMDD_HHMM]"
    print """           [-V vardict] use this option to provide a run-time VariableList
                  instead of displaying the user dialog.
                  The dictionary must be in the form of a Python
                  dictionary string, e.g.
                '{"Input Variable":"variable value"}'
                """
def main():
    argv = sys.argv

    # Parse command line
    try:
        optlist, args = getopt.getopt(
            argv[1:], 'n:p:h:u:a:s:e:t:v:c:d:g:m:z:V:')
    except getopt.error, val:
        print val
        usage()
        sys.exit(1)

    procName = None    
    configFile = None
    userName = None
    host = None
    port = None
    startTime = None
    endTime = None
    timeRange = None
    editArea = None
    mutableModel = None

    argDict = {}
    varDict = {}
    for switch, val in optlist:
        if switch == "-n":
            procName = val
        elif switch == "-s":
            startTime = val
        elif switch == "-e":
            endTime = val
        elif switch == "-a":
            editArea = val
        elif switch == "-t":
            timeRange = val
        elif switch == "-h":
            host = val
        elif switch == "-p":
            port = int(val)
        elif switch == "-u":
            userName = val
        elif switch == "-c":
            configFile = val
        elif switch == "-z":
            import offsetTime
            offsetTime.setDrtOffset(val)
        elif switch == "-m":
            mutableModel = val
        elif switch == "-V":
            exec "varDict = " + val

    if userName is None:
        userName = getProperty("user.name")
        
    if procName is None or configFile is None:
        usage()
        sys.exit(1)
        
    runProc = RunProcedure(procName, host, port, userName,
                          configFile, startTime, endTime,
                          timeRange, editArea, mutableModel, varDict)

if __name__ == "__main__":
    main()
