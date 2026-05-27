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
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Aug 10, 2016  19248    ryu       Fix java import issue (by Nate Jensen)
# Feb 07, 2017  6092     randerso  Refactored to support calling validateArgs()
#                                  from gfeClient.py
# Mar 16, 2017  6092     randerso  Added check for change of mutableModel
#
##
CLASS_NAME = 'Procedure'
METHOD_NAME = 'execute'


def runProcedure(args):
    ############################################################################
    # ProcedureRunner and required imports nested in this function because they
    # can only be run under Jep. This allows validateArgs to be called from
    # a pure Python environment
    ############################################################################

    import sys
    import time

    import TimeRange, AbsTime, LogStream
    import loadConfig
    import Exceptions
    import MasterInterface

    from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
    from com.raytheon.viz.gfe.core import DataManagerUIFactory

    class ProcedureRunner(MasterInterface.MasterInterface):
        def __init__(self, procName):
            MasterInterface.MasterInterface.__init__(self)
            self.addModule(procName)

        def runProcedure(self, moduleName, className, methodName, **kwargs):

            try:
                 return self.runMethod(moduleName, className, methodName, **kwargs)
            except Exceptions.EditActionError as e:
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
        def __init__(self, procName,
                     configFile, startTime, endTime, timeRange, editArea,
                     mutableModel, varDict):

            # import the config file
            prefs = loadConfig.loadPreferences(configFile)

            LogStream.logEvent("Configuration File: ", configFile)

            if mutableModel is None:
                mutableModel = prefs.getString('mutableModel')
            else:
                prefs.setValue('mutableModel', mutableModel)

            self.__dataMgr = DataManagerUIFactory.getInstance(None)

            currentMutableModel = self.__dataMgr.getParmManager().getMutableDatabase()
            desiredMutableModel = self.__dataMgr.getParmManager().decodeDbString(mutableModel)
            if currentMutableModel != desiredMutableModel:
                DataManagerUIFactory.dispose(None)
                self.__dataMgr = DataManagerUIFactory.getInstance(None)

            # Create Time Range
            if startTime is not None and endTime is not None:
                start = self.decodeTimeStruct(startTime)
                end = self.decodeTimeStruct(endTime)
                self.__timeRange = TimeRange.TimeRange(start, end)
            elif timeRange is not None:
                self.__timeRange = TimeRange.TimeRange(self.__dataMgr.getSelectTimeRangeManager().getRange(timeRange).toTimeRange())
            else:
                self.__timeRange = TimeRange.default()

            if editArea is not None:
                refID = ReferenceID(editArea)
                self.__editArea = \
                     self.__dataMgr.getRefManager().loadRefSet(refID)
            else:
                self.__editArea = self.__dataMgr.getRefManager().emptyRefSet()

            LogStream.logVerbose("varDict=", varDict)

            runner = ProcedureRunner(procName)

            errors = runner.getImportErrors()
            if len(errors) > 0:
                msg = "\n\t".join(["Error importing the following procedures:"] + errors)
                LogStream.error(msg)

            runner.instantiate(procName, CLASS_NAME, **{'dbss':self.__dataMgr})
            runner.run(self.__dataMgr, procName, CLASS_NAME, METHOD_NAME, varDict, self.__editArea, self.__timeRange)

        def decodeTimeStruct(self, timeStruct):
            return AbsTime.absTimeYMD(timeStruct.tm_year, timeStruct.tm_mon,
                                      timeStruct.tm_mday,
                                      timeStruct.tm_hour, timeStruct.tm_min)


    runProc = RunProcedure(args.procName, args.configFile,
                           args.startTime, args.endTime, args.timeRange,
                           args.editArea, args.mutableModel, args.varDict)

def validateArgs(args=None, parents=[]):
    ############################################################################
    # imports required for this method must be here so it can be invoked
    # from gfeClient.py
    ############################################################################
    from ufpy import UsageArgumentParser
    from ufpy.UsageArgumentParser import StoreTimeAction

    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve",
                                                     parents=parents,
                                                     prog='runProcedure')
    parser.add_argument("-n", action="store", dest="procName", required=True,
                        help="procedureName",
                        metavar="procName")
    parser.add_argument("-c", "--config", action="store", dest="configFile", required=False,
                      default="gfeConfig",
                      help="GFE config file -- default gfeConfig",
                      metavar="configFile")
    parser.add_argument("-u", action="store", dest="userName", required=False,
                        help="user name -- default SITE",
                        default="SITE",
                        metavar="userName")
    parser.add_argument("-a", action="store", dest="editArea", required=False,
                        help="editAreaName",
                        metavar="editArea")
    parser.add_argument("-s", action=StoreTimeAction, dest="startTime", required=False,
                        help="startTime -- format YYYYMMDD_hhmm",
                        metavar="startTime")
    parser.add_argument("-e", action=StoreTimeAction, dest="endTime", required=False,
                        help="endTime -- format YYYYMMDD_hhmm",
                        metavar="endTime")
    parser.add_argument("-t", action="store", dest="timeRange", required=False,
                        help="named time range (e.g. Today, Tonight)",
                        metavar="timeRange")
    parser.add_argument("-m", action="store", dest="mutableModel", required=False,
                        help="mutable database",
                        metavar="mutableModel")
    parser.add_argument("-z", "--drt", action=StoreTimeAction, dest="drt", required=False,
                      help="displaced real time -- format YYYYMMDD_hhmm",
                      metavar="drt")
    parser.add_argument("-V", action="store", dest="varDict", required=False,
                        help="""use this option to provide a run-time VariableList
                                instead of displaying the user dialog.
                                The dictionary must be in the form of a Python
                                dictionary string, e.g.
                                '{"Input Variable":"variable value"}'""",
                        default="{}",
                        metavar="varDict")

    return parser.parse_args(args)

def main():
    args = validateArgs()

    if args.drt:
        import offsetTime
        offsetTime.setDrtOffset(args.drt)

    if args.varDict:
        exec("args.varDict = " + args.varDict)

    runProcedure(args)

    if args.drt:
        import offsetTime
        offsetTime.reset()

if __name__ == "__main__":
    main()
