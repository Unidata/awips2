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
# runIFPText.py
# Main program and class for running text formatters from the command-line.
# Based on AWIPS-1 TextFormatter.py code written by hansen.
#
# Author: dgilling
# ----------------------------------------------------------------------------

import getopt
import logging
import os
import sys

import FormatterRunner
import loadConfig

from com.raytheon.viz.gfe.core import DataManager


LOGGER = None


def usage():
    print """
Usage: runIFPText
              -d database
              -t forecastType
             [-c configFile -- default gfeConfig]
             [-o output file for text -- default None]
             [-O server output file for text -- default None]
             [-S server controlled output file -- default None]
             [-A append text to given file name]
             [-server server -- example ec:9581/services]
             [-site siteID]
             [-l language -- english, french, spanish: default english]
             [-z displaced real time -- format YYYYMMDD_HHMM]
             [-T] Generates a "TEST" product.
             [-E] Generates a "EXPERIMENTAL" product.
             [-v vtecMode] Specifies vtec mode ('X','O','T','E')
             [-a vtecActiveTableName] Specifies alternate active table
             [-V vardict] use this option to provide a run-time VariableList
                  instead of displaying the user dialog.
                  The dictionary must be in the form of a Python
                  dictionary string, e.g.,
                '{("Forecast Product", "productType"):"Morning",
                  ("Issuance", "issuanceType"):"Routine"}'
                  The entries must be complete or the product will be cancelled.

             For Simple Table products:
             [-r Edit Area Name]
             [-w Time Range Name]  OR
             [-s startTime -e endTime]
             [-i Period for Tables with variable period (rows or columns)]
             """

def main2(argv):
    # Set up logging
    __initLogger()
    LOGGER.info("TextFormatter Starting")
    LOGGER.info("CmdLine: " + str(sys.argv[1:]))

    # Parse command line
    try:
        optlist, args = getopt.getopt(
            argv[1:], 'd:t:c:o:u:l:A:V:O:z:Tv:a:r:w:s:e:i:ES:')
    except getopt.error, val:
        LogStream.logProblem("Bad/missing command line arguments", val)
        usage()
        sys.exit(1)

    # Set up defaults
    outputFile = serverFile = databaseID = serverOutputFile = None
    appendFile = None
    userName = "SITE"
    language = None
    cmdLineVarDict = {}
    forecastList = []
    vtecMode = None
    testMode = 0
    experimentalMode = 0
    vtecActiveTable = "active"
    editAreas = []
    timeRanges = []
    startTime = endTime = None
    timePeriod = None
    offsetTime = None
    configFile = "gfeConfig"

    argDict = {}
    for switch, val in optlist:
        if switch == "-d":
            databaseID = val
        elif switch == "-t":
            forecastList.append(val)
        elif switch == "-c":
            configFile = val
        elif switch == "-o":
            outputFile = val
        elif switch == "-S":
            serverOutputFile = val
        elif switch == "-u":
            userName = val
        elif switch == "-A":
            appendFile = val
        elif switch == "-O":
            serverFile = val
        elif switch == "-V":
            cmdLineVarDict = val
        elif switch == "-a":
             vtecActiveTable = val
        elif switch == "-T":
             testMode = 1
        elif switch == "-E":
             experimentalMode = 1
        elif switch == "-v":
             vtecMode = val
        elif switch == "-l":
            language = val
        elif switch == "-z":
            offsetTime = val
        elif switch == "-r":
            editAreas.append((val,val))
        elif switch == "-w":
            timeRanges.append(val)
        elif switch == "-s":
            startTime = val
        elif switch == "-e":
            endTime = val
        elif switch == "-i":
            timePeriod = float(val)

    # Set default Forecast Type
    if len(forecastList) == 0:
        usage()
        LOGGER.error("ForecastList [-t] is empty or missing")
        sys.exit(1)

    # Can't have both T and E modes
    if testMode and experimentalMode:
        usage()
        LOGGER.error("Can't have both -T and -E switches")
        sys.exit(1)

    # Handle the VTEC modes
    if vtecMode is not None and vtecMode not in ['X','O','T','E']:
        usage()
        LOGGER.error("-v vtecMode must be ['X', 'O', 'T', 'E']")
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

    # set default gfe config so no popup windows appear
    loadConfig.loadPreferences(configFile)
#    LOGGER.debug("Configuration File: " + configFile)
    
    dataMgr = DataManager.getInstance(None)
    
    forecasts = FormatterRunner.runFormatter(databaseID, dataMgr.getSiteID(), 
                                             forecastList, cmdLineVarDict, 
                                             vtecMode, userName, dataMgr, 
                                             serverFile, editAreas, 
                                             timeRanges, timePeriod, 
                                             offsetTime, 
                                             vtecActiveTable, 
                                             testMode, 
                                             experimentalMode, 
                                             serverOutputFile, startTime, 
                                             endTime, language, 
                                             outputFile, appendFile)
    
    # Output of the forecasts is not needed as we can let formatterrunner do it,
    # LOGGER.info("Text:\n" + forecasts)

    LOGGER.info("Text Formatter Finished")
    
def __initLogger():
    global LOGGER
    LOGGER = logging.getLogger("runIFPText.py")
    LOGGER.setLevel(logging.INFO)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    LOGGER.addHandler(ch)

PROFILE = False
def profMain(arg):
    if PROFILE:
        import profile, pstats, sys
        limit = 20
        profile.run('main2(sys.argv)', 'pyprof.out')
        p = pstats.Stats('pyprof.out')
        p.strip_dirs()
        p.sort_stats('time', 'calls').print_stats(limit)
        p.print_callers(limit)
    else:
        try:
            main2(arg)
        except:
            LOGGER.exception("Caught Exception: ")

main = profMain
if __name__ == "__main__":
    profMain(sys.argv)
