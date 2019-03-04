#!/usr/bin/env python

##
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
