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

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2 Mar 2010      #3771         jelkins        Initial Creation.

# version format is y.y.mm.dd
version = "1.0.3.2"

from optparse import OptionParser
from optparse import OptionGroup
import HeaderUpdater
import sys

def main():
	commandParser = OptionParser(usage="usage: %prog [OPTIONS] [FILE]",
		version="%prog "+version+ "\n+ HeaderUpdater " + HeaderUpdater.version)
	
	commandParser.add_option_group(OptionGroup(commandParser,
		"FILE","Specify a file containing a list of files to process."+
		"  Each line in the file contains a single Filename to process."+
		"  If no FILE is provided or FILE is `-' filenames are read"+
		" from stdin."))
		
	HeaderUpdater.addOptions(commandParser)
		
	(commandOption,args) = commandParser.parse_args()
	
	if len(args) == 1:
		FILE = args[0]
	else:
		FILE = "-"
		
	inputFile = sys.stdin
	if FILE != "-":
		inputFile = open(FILE,'r')
	
	fileList = inputFile.read()
	inputFile.close()
	
	for fileName in fileList.strip().split("\n"):
		HeaderUpdater.main(commandOption,fileName)
	
if __name__ == "__main__":
	main()
