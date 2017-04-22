#!/usr/bin/env python

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

XML_TEMPLATE = ""

import sys
import os.path
from os import path, access, R_OK
import shutil

class MainData:
        def __init__(self, file=None, prefix=None, descriptiveName=None, server='localhost:9581/services'):
                self._descriptiveName = descriptiveName
                self._prefix = prefix
                self._file = file
                self._server = server

        def setPrefix(self, prefix):
                self._prefix = prefix

        def setFile(self, file):
                self._file = file

        def setDescriptiveName(self, descriptiveName):
                self._descriptiveName = descriptiveName
                
        def setServer(self, server): 
                self._server = server


# set up our main data
mainData = MainData()

# set up argument map
argumentMap = {
                "-f" : mainData.setFile,
                "-n" : mainData.setDescriptiveName,
                "-p" : mainData.setPrefix,
                "-s" : mainData.setServer,
              }


# current function pointer
function = None

# parse arguments and populate MainData object
for arg in sys.argv[1:]:
    if function is None:
        function = argumentMap[arg]
    else:
        function(arg)
        function = None

# validate arguments passed in
file = mainData._file
exportFileName = mainData._prefix
name = mainData._descriptiveName
server = mainData._server

if mainData._file is None:
        print 'File not passed in, please pass in file using -f <path_to_file>'
        exit(1)

if not os.path.exists(file)  or not  os.path.isfile(file) :
        print 'File,', file, 'is not a valid file'
        exit(2)

if name is None:
        print "Give a descriptive name for the data using -n '<descriptive name>'"
        exit(3)

print 'Parsing file,', file
fileName = os.path.split(file)[1]

if fileName == "spotters.dat":
    workFile = "/tmp/spotters.dat"
    if path.exists(workFile) and path.isfile(workFile) and access(workFile, R_OK):
        print "Attempting to cleanup work directory"
        os.system("rm /tmp/spotters.dat")
    else:
        print "No preliminary cleanup needed - continuing"

    shutil.copy(file, workFile)
    os.system("sed -i -e 's/spotterName/spottersName/g' /tmp/spotters.dat")
    os.system("sed -i -e 's/spotterAddr/spottersAddress/g' /tmp/spotters.dat")
    os.system("sed -i -e 's/spotterCity/spottersCity/g' /tmp/spotters.dat")
    os.system("sed -i -e 's/spotterPhone/spottersPhone/g' /tmp/spotters.dat")
    file = workFile

if exportFileName is None:
    exportFileName = fileName
    idx = exportFileName.rfind('.')
    if idx > -1:
        exportFileName = exportFileName[0:idx]

from awips.ThriftClient import * 
client = ThriftClient(server)

from dynamicserialize.dstypes.com.raytheon.uf.common.pointdata.requests import *

request = NewAdaptivePlotRequest()
request.setFileName(fileName)
request.setBundleName(exportFileName)
request.setDescription(name)
request.setFileContents(open(file,"r").read())

client.sendRequest(request)

if fileName == "spotters.dat":
    os.system("rm /tmp/spotters.dat")

print 'plots uploaded to server'
