#!/usr/bin/env python
#
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

import os
import re
import datetime
import sys
import string
from optparse import OptionParser
from datetime import datetime, timedelta
import shutil


def showUsage():
    log("./pT.py [-b|--broker-log-dir] <dir> [-t|--test-dir] <dir>")


ACCESS="Access"
MODIFY="Modify"

BROKER_LOG="broker.log"
BROKER_PID="broker.pid"
BROKER_CPU="broker_cpu.log"
BROKER_CPU_DATED="broker_cpu.log.dated"
BROKER_GC="gc.log"

GRAPH_DATA="graph.data"

_verbose = False
_debug = False
_brokerLogs = "" 

def exitError(message):
    log(message)
    sys.exit(1)

def main():
    global _log, _verbose, _debug, _brokerLogs

    # Load the
    parser = OptionParser()

    parser.add_option("-v", "--verbose", dest="verbose",
                      action="store_true", default=False, help="enable verbose output")

    parser.add_option("-d", "--debug", dest="debug",
                      action="store_true", default=False, help="enable debug output")

    parser.add_option("-b", "--broker-log-dir", dest="brokerLogs",
                      action="store", default=True, help="Broker Logs")

    parser.add_option("-t", "--test-dir", dest="testDir",
                      action="store", default="", help="Test Results")


    (options, args) = parser.parse_args()
    
    _verbose = options.verbose
    _debug = options.debug
    testDir = options.testDir
    _brokerLogs = options.brokerLogs
    
    if testDir == "" or _brokerLogs == "" :
        log("Broker Log Dir and Test Dir are both requried.")
    	showUsage()

    if not os.path.exists(testDir):
        exitError("Test directory does not exist:" + testDir)

    if not os.path.exists(_brokerLogs):
        exitError("Broker log directory does not exist:" + _brokerLogs)


    # Standardize the format of the broker logs    
    preProcessBrokerLogs(_brokerLogs)
    
    # Get list of test results from test_dir
    processTestResults(testDir)

#
# Process the log files we know of
#
def preProcessBrokerLogs(resultDir):
    # Pre-Process GC - no pre processing required 
    
    # Process Log4j - no processing required as file is already time stamped.
    
    # Pre-Process broker_cpu    
    processCPUUsage(resultDir)
    
#
# Process the broker CPU log file and create an output file of format
# <Date Time> <CPU Usage>
#
#    
def processCPUUsage(resultDir):
    logfile=resultDir+os.sep+BROKER_CPU
    datedFile=resultDir+os.sep+BROKER_CPU_DATED
    
    start = extractTime(ACCESS, logfile+".stat")        
    
    pid = getPID(BROKER_PID)
    
    topRate = getFirstLine(_brokerLogs+os.sep+"top.rate")
    
    #
    # Calulate addition required per process line output
    #
    if topRate.find(".") == -1:
       second = topRate
       millis = 0
    else:
       split = topRate.split('.')
       seconds = split[0]
       # Convert 
       millis = float("0."+split[1]) * 1000 
        
    offset = timedelta(seconds=int(seconds),milliseconds=int(millis))

    #
    # Process the CPU log file and make a file of format:
    # datetime <CPU% usage> <MEM% usage>
    #
    # Open log CPU file for reading
    logfile = open(logfile, "r")

    # Open the output file, erasing any existing version
    output= open(datedFile, "w")
    for line in logfile:
        if line.find(pid) != -1:
	    # Split line on whitespace
	    data = line.split()

	    #
	    # Data format
	    # 0  1     2   3  4    5     6    7    8   9   10 11
	    # PID USER PR  NI %CPU TIME+ %MEM VIRT RES SHR S  COMMAND    
	    # 
	    
	    #Write out the date time (ISO-8601 format)
	    output.write(str(start))
	    # Output the %CPU value
	    output.write(" "+str(data[4]))
   	    # Output the %MEM value
	    output.write(" "+str(data[5]))
	    output.write('\n')

	    # Add the offset based on the logging rate
	    start = start + offset

    # Close the files
    logfile.close
    output.close  
    
    log("Pre Process of CPU Log file '"+BROKER_CPU+"' complete")
    
        
#
# Give an known process type get the recorded PID.
#	
def getPID(process):
    return getFirstLine(_brokerLogs+os.sep+process)
    
#
# Get the first line of the file without EOL chars.
# NOTE: this will load the entire file into memory to do it.
#
def getFirstLine(fileName):  
    f = open(fileName,"r")
    line = f.read().splitlines()[0]
    f.close  
    return line


#
# Walk the directory given and process all csv test results
#
def processTestResults(resultDir):
    for root, dirs, files in os.walk(resultDir, topdown=False):
        if len(files) == 0:
	    exitError("Test result directory is empty:" + resultDir)
        for file in files:	   
	    if file.endswith(".csv"):
	        processTestResult(root , file)

def processTestResult(root, resultFile):    
    # Open stat file and extract test times, we determine:
    #  -start time based on the 'Access' value 
    #  -end  time based on the 'Modify' value  'Change' would also work
    
    statFile=root+os.sep+resultFile+".stat"
    
    if not os.path.exists(statFile):
        log("Unable to process : Unable to open stat file:" + statFile)
	return

  
    createResultSetPackage(root, resultFile)
    
    
def extractTime(field, statFile):
    stats = open(statFile, "r")
    for line in stats:
    	if line.startswith(field):
	    if line.find("(") == -1:
		dt = lineToDate(" ".join(line.split()[1:]))
		
		#
		# TODO We need to handle time time zone issues as I'm sure we will have issues with the
		# log4j matching.
		
		stats.close
		return dt		
		
#
# Given a text line in ISO format convert it to a date object
#				
def lineToDate(line):
    #2009-06-22 17:04:44,320
    #2009-06-22 17:04:44.320
    pattern = re.compile(r'(?P<year>^[0-9][0-9][0-9][0-9])-(?P<month>[0-9][0-9])-(?P<day>[0-9][0-9]) (?P<hour>[0-9][0-9]):(?P<minute>[0-9][0-9]):(?P<seconds>[0-9][0-9])')
    
    
    m = pattern.match(line)
    if m:			
        year = int(m.group('year'))
        month = int(m.group('month'))
        day = int(m.group('day'))
        hour = int(m.group('hour'))
        minute = int(m.group('minute'))
        seconds = int(m.group('seconds'))
	
        pattern = re.compile(r'(?P<year>^[0-9][0-9][0-9][0-9])-(?P<month>[0-9][0-9])-(?P<day>[0-9][0-9]) (?P<hour>[0-9][0-9]):(?P<minute>[0-9][0-9]):(?P<seconds>[0-9][0-9])[.|,](?P<micro>[0-9]+)')
        m = pattern.match(line)
        micro = None
        if m:
	    micro = m.group('micro')
	    
        if micro == None:
	   micro = 0
	
        return datetime(year,month,day,hour,minute,seconds,int(micro))
    else:
        # Error we shouldn't get here
        return null
		
def createResultSetPackage(root, resultFile):
    # Get the Name of the test to make a directory with said name
    testName = resultFile.split(".csv")[0]
    resultDir = root+ os.sep + testName

    log("Processing Result set for:"+ testName)
    
    mkdir(resultDir)
    
    # Move result file to new directory
    shutil.move(root + os.sep + resultFile, resultDir)
    
    # Move stat file to new directory
    shutil.move(root + os.sep + resultFile + ".stat",  resultDir)
     
    statFile=resultDir + os.sep + resultFile + ".stat"
    
    #
    # Get start and end time for test run
    #
    start = extractTime(ACCESS, statFile)
    end   = extractTime(MODIFY, statFile)
     
    sliceBrokerLogs(resultDir, start, end)
    createGraphData(resultDir, testName)
    
    log("Created Result Package for:"+ testName)
    
def sliceBrokerLogs(resultDir, start, end):
    sliceCPULog(resultDir, start, end)
    sliceLog4j(resultDir, start, end)
    sliceGCLog(resultDir, start, end)
    

def sliceCPULog(resultDir, start, end):
    global _brokerLogs
    logfilePath=_brokerLogs+os.sep+BROKER_CPU_DATED
    cpuSliceFile=resultDir+os.sep+BROKER_CPU
    
    # Process the CPU log file and make a file of format:
    # datetime <CPU% usage> <MEM% usage>
    #
    # Open log CPU file for reading
    logFile = open(logfilePath, "r")
    
    #
    # Create outputfile
    #
    cpuslice = open(cpuSliceFile,"w")
    
    for line in logFile:
    	data = line.split()
	#
	# //fixme remove tz addition.
	#
	lineTime = lineToDate(" ".join(data[0:2])+" +0000")
	
	if lineTime > start:
	    if lineTime < end:
   	        cpuslice.writelines(line)

    logFile.close()
    cpuslice.close()
    log("Sliced CPU log")

def sliceGCLog(resultDir, start, end):
    global _brokerLogs
    logfilePath=_brokerLogs+os.sep+BROKER_GC
    sliceFile=resultDir+os.sep+BROKER_GC
    
    gcstart = extractTime(ACCESS, logfilePath+".stat")        
    
    # Open log GC file for reading
    logFile = open(logfilePath, "r")

    # Open the output file, erasing any existing version
    output= open(sliceFile, "w")
    
    # Use a regular expression to pull out the Seconds.Millis values from the
    # Start of the gc log line.
    pattern = re.compile(r'(?P<seconds>^[0-9]+)\.(?P<millis>[0-9]+):')
    
    for line in logFile:
        m = pattern.match(line)
    
        if m:
	    seconds = m.group('seconds');
	    millis = m.group('millis');
    		
	    offset = timedelta(seconds=int(seconds),milliseconds=int(millis))     

            lineTime = gcstart + offset
	    
	    if lineTime > start:
	        if lineTime < end:
   	            output.writelines(line)
				
    # Close the files
    logFile.close
    output.close()
    log("Sliced gc log")

	    
def sliceLog4j(resultDir, start, end):    
    global _brokerLogs
    logfilePath=_brokerLogs+os.sep+BROKER_LOG
    log4jSliceFile=resultDir+os.sep+BROKER_LOG
    
    log4jstart = extractTime(ACCESS, logfilePath+".stat")        
    
    #
    # Say that first line is the start of the file,
    # This value will give a time value to the initial
    # logging before Log4j kicks in.
    #
    lineTime = log4jstart
    
    # Process the broker log4j file
    # Open log CPU file for reading
    logFile = open(logfilePath, "r")
    
    #
    # Create outputfile
    #
    log4jslice = open(log4jSliceFile,"w")
    
    for line in logFile:
    	data = line.split()
	
	#
	# If the line has a time at the start then process it
	# otherwise use the previous time. This means if there is
	# a stack trace in the middle of the log file then it will
	# be copied over to the split file as long as it is in the 	
	# split time.
	#
	if (hasTime(data)):
	    #
	    # //fixme remove tz addition.
   	    #
	    lineTime = lineToDate(" ".join(data[0:2])+" +0000")
	    	
	if lineTime > start:	    
	    if lineTime < end:
	        print line
   	        log4jslice.writelines(line)

    logFile.close()
    log4jslice.close()
    log("Sliced broker log")


#
# Check the first two entries of data can make a datetime object
#
def hasTime(data):
    date = data[0]
    time = data[1]
    
    # Examples:
    # 2009-06-22 17:04:44,246
    # 2009-06-22 17:04:44.2464
    # 2009-06-22 17:04:44
    
    # ISO-8601 '-' format date
    dateRE = re.compile('[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]')
    
    #
    # Check for times with/out millis    
    # e.g.
    # 10:00:00,000    - log4j
    # 10:00:00.0000   - generated in script for cpu time
    #
    timeRE = re.compile('[0-9][0-9]:[0-9][0-9]:[0-9][0-9]?[0-9]*')

    return dateRE.match(date) and timeRE.match(time)
 
    
def createGraphData(resultDir, testName):
    # Create graph.data file for process.sh
    # Format two lines : Title and filename
    #   $version $type : $volume% volume
    #   $version-$brokerState-$type-$volume
    version=getBrokerVersion()
    
    test= extractTestValue("n",resultDir, testName)    
    volume = int(float(extractTestResult("Test * Size Throughput", resultDir, testName)) * 1000)
    messageSize = extractTestValue("messageSize",resultDir, testName)
    ackMode = ackModeToString(extractTestValue("consAckMode",resultDir, testName))
    
    graphDataFile=resultDir+os.sep+GRAPH_DATA

    graphData = open(graphDataFile, "w")
    
    #
    # Write Title
    graphData.write(version+":"+test+":"+str(messageSize)+"kb x "+str(volume)+" msg/sec using "+ackMode)
    graphData.write('\n')
    
    #
    # Write FileName
    graphData.writelines(version+"-"+testName)
    graphData.write('\n')    
    graphData.close
    log("Created graph.data")
    
  
def getBrokerVersion():
    global _brokerLogs
    READY = "Qpid Broker Ready"
    brokerLogFile = _brokerLogs + os.sep + BROKER_LOG
    
    log = open(brokerLogFile, "r")
    
    dataLine = ""
    for line in log:
    	if line.find(READY) != -1:
	    dataLine = line
	    break	
	    
    # Log Entry
    #2009-06-19 17:04:02,493 INFO  [main] server.Main (Main.java:456) - Qpid Broker Ready :2.3.0.1 build: 727403M	    
    # Split on READY
    data = dataLine.split(READY)
    
    # So [1] should be 
    # :2.3.0.1 build: 727403M	        
    readyEntries = data[1].split()
    
    # so spliting on white space should give us ':version'
    # and a quick split on ':' will give us the version
    version = readyEntries[0].split(':')[1]
        
    # Strip to ensure we have no whitespace
    return version.strip()


def extractTestValue(property,resultDir,testName):
    return extractTestData(property,resultDir,testName," =")
    
def extractTestResult(property,resultDir,testName):  
    return extractTestData(property,resultDir,testName,":")
    
def extractTestData(property,resultDir,testName,type):
    resultFile = resultDir + os.sep + testName+".csv"
    
    results = open(resultFile, "r")

    dataLine = ""
    for line in results:
        if line.find("Total Tests:") == 0:
	    dataLine = line 

    results.close()
    
    # Data is CSV
    data = dataLine.split(',')
    
    found = False
    result = ""
    searchProperty = property+type

    for entry in data:
        if found:
	    result = entry
	    break
        if entry.strip() == searchProperty:
	    found=True
	
    return result.strip()    
    

def ackModeToString(ackMode):
     if ackMode == '0':
         return "Transacted"
     elif ackMode == '1':
         return "AutoAck"	 
     elif ackMode == '2':
         return "ClientAck"
     elif ackMode == '3':
         return "DupsOK" 
     elif ackMode == '257':
         return "NoAck"
     elif ackMode == '258':
         return "PreAck"
     else:
     	 return str(ackMode)

    

def debug(msg):
   global _debug
   if _debug:
       log(msg)
       
def log(msg):
   print msg
    
def mkdir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)
    
if __name__ == "__main__":
    main()
