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
from optparse import OptionParser
from optparse import OptionGroup
import subprocess
import re
from os.path import isfile
import sys

from FileFilter import FileFilter
import HeaderUpdater

# version is derived from the last modified data y.y.m.d
version = "1.0.3.4"



def main():
	
	optionParser = OptionParser(usage="usage: %prog [OPTIONS] DIRECTORY",
		version="%prog " + version + "\n+ HeaderUpdater " + HeaderUpdater.version)
	
	HeaderUpdater.addOptions(optionParser)
	
	optionParser.add_option("--filters", dest="filterFile",
		help="filter the files in DIRECTORY according to the given"
		 + " FILTERFILE.")
		
	optionParser.add_option("--file-log", dest="foundFilesLog",
		help="keep a log of (filtered) files found and fed to headup.")
		 
	optionParser.add_option_group(OptionGroup(optionParser,
		"DIRECTORY","Specify a directory containing files to update."+
		"  DIRECTORY is recursively searched for files to update."))
		
	(option, args) = optionParser.parse_args()
	
	if len(args) != 1:
		optionParser.error("missing DIRECTORY")
		
	DIRECTORY = args[0]

	fileFilter = FileFilter(option.filterFile)

	fileListProcess = subprocess.Popen(["find", DIRECTORY, "-type", "f"],
		stdout=subprocess.PIPE)
	fileList = fileListProcess.communicate()[0]

	fileList = fileList.split("\n")
	fileList = "\n".join(fileFilter.getFilteredFiles(fileList))
	
	foundFilesLog = option.foundFilesLog
	if foundFilesLog != None:
		fflogObject = open(foundFilesLog, 'w')
		fflogObject.write(fileList)
		fflogObject.close()
		
	for fileName in fileList.strip().split("\n"):
		HeaderUpdater.main(option,fileName)
	
if __name__ == "__main__":
	main()
