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
#    3 Mar 2010      #3771         jelkins        Initial Creation.

from __future__ import with_statement

# the version is derived from the date last updated y.y.m.d
version = "1.0.3.12"

from optparse import OptionParser
from optparse import OptionGroup
from os import pathsep
from os import rename
from os.path import basename
from os.path import splitext
import sys
import re
import logging

from FileTypeConfig import FileTypeConfig
import OptionCallback

_regexCache = {}

def getRegex(fileType, regexKey, value=None):
	global _regexCache
	
	fileTypeNode = {}
	
	if fileType in _regexCache:
		fileTypeNode = _regexCache[fileType]
	else:
		 _regexCache[fileType] = fileTypeNode
		 
	if not(regexKey in fileTypeNode):
		fileTypeNode[regexKey] = None
		if value != None:
			fileTypeNode[regexKey] = re.compile(value,re.DOTALL)
		
	return fileTypeNode[regexKey]

def getLastMatch(matches, matchSplit, endOffset=None, splitGroup= - 1):
	result = re.split(matchSplit, matches)
	result = result[splitGroup]
	if endOffset != None:
		result = result[:endOffset]
	return re.escape(result)

def getHeader(headerFileName, fileConfig):
	headerText = ""
		
	with open(headerFileName, 'r') as headerInput:
		for line in headerInput:
			searchText = fileConfig.getConfig("textSearch")
			replaceText = fileConfig.getConfig("textReplace")
			if searchText != None and replaceText != None:
				line = re.sub(re.escape(searchText), replaceText, line)
			headerText += fileConfig.getConfig("lineTemplate", {"lineText":line})
	result = fileConfig.getConfig("headerTemplate", {"headerText":headerText[: - 1]})
	return result
	
def addOptions(commandLineParser):
	commandLineParser.add_option("-a", "--disable-addmissing", dest="addMissing",
		default=True, action="store_false",
		help="do not add a header if an existing header is not found.")
		
	commandLineParser.add_option("-v", "--verbose", dest="verbose",
		action="callback", callback=OptionCallback.flagWithOption,
		help="output what's happening to stderr. -v [DEBUG] enable "
		 + "debug output")
	
	commandLineParser.add_option("-i", "--in-place", dest="backupSuffix",
		action="callback", callback=OptionCallback.flagWithOption,
		help="update FILE in place. -i [BACKUPSUFFIX] create a backup "
		 + "of the original file.")
		
	commandLineParser.add_option("-r", "--revert-backup", dest="revertSuffix",
		help="revert FILE to FILEREVERTSUFFIX and remove backup")
		
	commandLineParser.add_option("-t", "--textheader", dest="headerFile",
		 help="read header text from HEADERFILE")
		 
	commandLineParser.add_option("-s", "--search", dest="searchString",
		default="",
		help="look for an existing header with a matching SEARCHSTRING.")
		
	commandLineParser.add_option("-S", "--search-regex", dest="searchPattern",
		help="look for an existing header with a matching SEARCHPATTERN.")
		
	commandLineParser.add_option_group(OptionGroup(commandLineParser,
		"SEARCHPATTERN|SEARCHSTRING", "Without specifying a SEARCHPATTERN"
		 + " or SEARCHSTRING a search will only be made for an existing"
		 + " header that matches the template.  Specify a SEARCHSTRING or"
		 + " SEARCHPATTERN to enable block and line block header searching."
		 + "  If both a SEARCHSTRING and SEARCHPATTERN are given, The"
		 + " SEARCHPATTERN will override the SEARCHSTRING."))
		
	commandLineParser.add_option("-l", "--search-limit", dest="searchLimit",
		default=3000, type=int,
		help="look for an existing header within the first SEARCHLIMIT "
		 + "bytes.  Recommend setting this to about 200% the size of the current"
		 + " header. default %default")
		
	commandLineParser.add_option("-f", "--filetypes", dest="fileTypesDir",
 		help="include the filetype configurations from FILETYPESDIR. "
 		 + "Multiple directories may be specified using the `" + pathsep
 		 + "' path separater character")
 		
	commandLineParser.add_option("-e", "--ext", dest="fileExtension",
		help="specifiy the FILEEXTENSION to use")
	

def main(commandOption=None, FILE=None):
	""" Execute HeaderUpdater from the command line
	"""
	
	# define the command line options
	commandLineParser = OptionParser(usage="usage: %prog [OPTIONS] [FILE]",
		version="%prog " + version)
	
	commandLineParser.add_option_group(OptionGroup(commandLineParser,
		"FILE", "Specify an input FILE.  If no FILE is given or if"
		 + " FILE is `-' read input from stdin.  When reading from stdin"
		 + " the -e option is required."))
		
	addOptions(commandLineParser)

	# parse the arguments	

	commandLineOption = None
	args = None
	
	if commandOption != None:
		commandLineOption = commandOption
	else:
		(commandLineOption, args) = commandLineParser.parse_args()		
		
	if FILE != None:
		args = [FILE]
	
	if len(args) == 1:
		inputFileName = args[0]
	elif commandLineOption.fileExtension != None:
		inputFileName = "-"
	else:
		commandLineParser.error("stdin requires -e option")
	
	# setup the logger
	logging.basicConfig(stream=sys.stderr,
						format='%(name)-12s: %(levelname)-8s %(message)s')
						
	logger = logging.getLogger(basename(inputFileName))
	
	logLevel = logging.WARNING
	verbose = commandLineOption.verbose
	if verbose != None:
		logLevel = logging.INFO
		if verbose != "":
			if verbose == "DEBUG":
				logLevel = logging.DEBUG
	
	logger.setLevel(logLevel)
	
	# quickly restore a file from backup
	revertSuffix = commandLineOption.revertSuffix
	if revertSuffix != None:
		try:
			rename(inputFileName + revertSuffix, inputFileName)
		except OSError, v:
			logger.error(v)
		return
		
	# load the filetype configurations
	fileTypeConfig = FileTypeConfig()
	
	fileTypeConfig.fileType = splitext(inputFileName)[1]
	
	if commandLineOption.fileExtension != None:
		fileTypeConfig.fileType = commandLineOption.fileExtension
		
	if commandLineOption.fileTypesDir != None:	
		fileTypeConfig.loadConfig(commandLineOption.fileTypesDir)
		logger.debug("Loaded fileType configs from: " + commandLineOption.fileTypesDir)
					
	# check for a configuration for the input file	
	if not(fileTypeConfig.isAvailable()):
		logger.error("no " + fileTypeConfig.fileType + " configuration exists")
		return 10
			
	# read the inputfile
	inputFile = sys.stdin
	if inputFileName != "-":
		inputFile = open(inputFileName, 'r')
	
	inputHeader = inputFile.read(commandLineOption.searchLimit)
	inputFooter = inputFile.read()
	inputFile.close()
	
	logger.info("Ready to process " + inputFileName)

	searchOption = re.escape(commandLineOption.searchString)
	
	if commandLineOption.searchPattern != None:
		searchOption = commandLineOption.searchPattern
		
	searchString = ".*?" + searchOption + ".*?"
	
	# these offsets provide an easy way to handle line returns caught
	# by the match
	headerStartOffset = 0
	headerEndOffset = 0
	
	# create the newHeader
	newHeader = None
	
	if commandLineOption.headerFile != None:
		newHeader = getHeader(commandLineOption.headerFile, fileTypeConfig)
	
	# check that we don't already have the new header in the inputFile
	notUpdated = False
	logger.info("Checking if file already contains updated header")
	headerMatch = None if newHeader == None else re.search(re.escape(newHeader), inputHeader, re.DOTALL)
	if headerMatch != None:
		notUpdated = True
		logger.info("File already contains the updated header")
	else:
		# check if we can find a header matching the template
		searchHeader = "\n*" + re.escape(fileTypeConfig.getConfig("headerTemplate", {"headerText":"searchStringPlaceholder"})) + "\n"
		searchHeader = re.sub("searchStringPlaceholder", searchString, searchHeader)
		logger.info("Checking if file contains a header matching the template")
		headerMatch = re.search(searchHeader, inputHeader, re.DOTALL)
	
		if headerMatch != None:
			headerEndOffset = - 1
			logger.info("Searching for the start of the header")
			headerStartOffset = len(re.search("\n*", headerMatch.group()).group())
			
			# we must check that each line starts with the lineTemplate
			validTemplateMatch = True
			header = headerMatch.group()[headerStartOffset:headerEndOffset]
			logger.info("Ensuring each line in the header starts with the lineTemplate")
			for line in header.split("\n")[1: - 1]:
				lineSearch = fileTypeConfig.getConfig("lineTemplate", {"lineText":""})
				lineMatch = re.search(re.escape(lineSearch), line)
				if lineMatch == None:
					validTemplateMatch = False
					headerMatch = None
					break
			
			if validTemplateMatch == True:
				logger.info("Found existing header matching template")
	
	if headerMatch == None and searchString != ".*?.*?" and fileTypeConfig.getConfig("blockBegin") != None:
		# try and find a header located inside a block comment
		searchBlock = re.escape(fileTypeConfig.getConfig("blockBegin"))
		searchBlock += searchString
		searchBlock += re.escape(fileTypeConfig.getConfig("blockEnd"))
		
		logger.info("Searching for header inside block comment")
		headerMatch = re.search(searchBlock, inputHeader, re.DOTALL)
		
		if headerMatch != None:
			blockBegin = re.escape(fileTypeConfig.getConfig("blockBegin")) 
			isAmbiguousBlock = fileTypeConfig.getConfig("blockBegin") == fileTypeConfig.getConfig("blockEnd")
			
			splitGroup = - 1
			if isAmbiguousBlock == True:
				splitGroup = - 2
		
			headerSubGroup = getLastMatch(headerMatch.group(), blockBegin, splitGroup=splitGroup)
			headerSubGroup = blockBegin + headerSubGroup
			
			if isAmbiguousBlock == True:
				headerSubGroup += blockBegin
			
			logger.info("Searching last header inside block comment")
			headerMatch = re.search(headerSubGroup, inputHeader, re.DOTALL)
		
		if headerMatch != None:
			logger.info("Found existing header inside block section")
			
	if headerMatch == None and searchString != ".*?.*?" and fileTypeConfig.getConfig("lineComment") != None:
		# try and find a header offset by line comments
		# this is only done if the searchRegEx isn't the default,
		# otherwise we will probably match something that isn't a header
		
		lineComment = fileTypeConfig.getConfig("lineComment")
		
		searchLine = re.escape(lineComment) + ".*?"
		searchLine += searchString + "\n"
		
		# lookahead assertions are AWESOME!
		searchLine += "(?!" + re.escape(lineComment) + ")"
		
		lineHeaderRegex = getRegex(fileTypeConfig.fileType, "lineHeader", searchLine)
		
		logger.info("Searching for a header in a block of line comments")
		headerMatch = lineHeaderRegex.match(inputHeader)
		
		if headerMatch != None:
			logger.info("Splitting the header into its line comment groups")
			headerSubGroup = getLastMatch(headerMatch.group(),
				"\n(?!" + re.escape(lineComment) + ").*?\n", - 1)
			
			logger.info("Searching for the last header in a block of line comments")
			headerMatch = re.search(headerSubGroup, inputHeader, re.DOTALL)
			
		# handle situations where the header and placeAfter portion
		# are not split by a a line
		placeAfter = fileTypeConfig.getConfig("placeAfter")
		if headerMatch != None and placeAfter != None:
			placeAfterSearch = placeAfter + "(.*)"
			logger.info("Searching to see if the header is directly after a placeAfter")				
			headerMinusPlaceAfter = re.search(placeAfterSearch, headerMatch.group(), re.DOTALL)
			if headerMinusPlaceAfter != None:
				logger.info("Extracting the header from the placeAfter")
				headerMatch = re.search(re.escape(
					headerMinusPlaceAfter.group(1)), inputHeader, re.DOTALL)
						
		# we must check that each line starts with the lineComment
		if headerMatch != None:
			header = headerMatch.group()
			logger.info("Verifying all lines in the header begin with a lineComment")
			for line in header.split("\n"):
				lineMatch = re.search("^" + re.escape(lineComment) + ".*", line)
				if lineMatch == None:
					headerMatch = None
					break
			
		if headerMatch != None:
			logger.info("Found existing header in line comment section")
	
	if (headerMatch != None
		and commandLineOption.headerFile != None 
		and notUpdated == False):
		# an existing header was found, we will need to replace it		
		outputHeader = (inputHeader[:headerMatch.start() + headerStartOffset] + 
			newHeader + inputHeader[headerMatch.end() + headerEndOffset:])
		
		logger.info("Updated existing header")
		logger.debug("\n" + headerMatch.group() + "\nwith: \n" + newHeader)
	elif ((commandLineOption.addMissing and fileTypeConfig.getBooleanConfig("addMissing") != False)
		and notUpdated == False
		and commandLineOption.headerFile != None):
		# an existing header was not found, we need to add a new one
		
		placementSearch = fileTypeConfig.getConfig("placeAfter")
		if placementSearch != None:
			logger.info("Searching for the placeAfter")
			placementMatch = re.search(placementSearch, inputHeader)
			
			if placementMatch != None:
				
				outputHeader = inputHeader[:placementMatch.end()]
				
				if outputHeader[ - 1] != "\n":
					outputHeader += "\n"
				
				outputHeader += newHeader
				
				if inputHeader[placementMatch.end()] != "\n":
					outputHeader += "\n"
					
				outputHeader += inputHeader[placementMatch.end():]
				
				logger.info("Added new header after placement match")
				logger.debug("\n" + newHeader + "\nplacement match:\n" + 
							placementMatch.group())
			else:
				# we didn't find the placement match
				info = "Failed to find placement match, "
				
				requirePlaceAfter = fileTypeConfig.getBooleanConfig("requirePlaceAfter")
				
				if requirePlaceAfter == None:
					requirePlaceAfter = True
				
				if requirePlaceAfter == True:
					outputHeader = inputHeader
					logger.info(info + "no file modifications were made")
					notUpdated = True
				else:
					outputHeader = newHeader
					
					if len(inputHeader) != 0 and inputHeader[0] != "\n":
						outputHeader += "\n"
						
					outputHeader += inputHeader
					
					logger.info(info + "but placement matching is not required")
					logger.info("Added new header")
					logger.debug("\n" + newHeader)
				
		else:		
			outputHeader = newHeader
					
			if inputHeader[0] != "\n":
				outputHeader += "\n"
						
			outputHeader += inputHeader
			
			logger.info("Added new header")
			logger.debug("\n" + newHeader)
	else:
		# don't do anything
		outputHeader = inputHeader
		logInfo = ""
		if newHeader == None:
			logInfo = "No header file provided, "
		elif notUpdated == False:
			logInfo = "Failed to find existing header, "
		logger.info(logInfo + "no file modifications were made")
		notUpdated = True
	
	outputStream = sys.stdout

	if commandLineOption.backupSuffix != None:
		if commandLineOption.backupSuffix != "" and notUpdated == False:
			# create a backup of the original file
			backupFileName = inputFileName + commandLineOption.backupSuffix
			backupFile = open(backupFileName, 'w')
			backupFile.write(inputHeader)
			backupFile.write(inputFooter)
			backupFile.close()
			logger.info("Created backup file: " + backupFileName)
		outputStream = open(inputFileName, 'w')

	outputStream.write(outputHeader)
	outputStream.write(inputFooter)
	
	outputStream.flush()
	
	if outputStream != sys.stdout:
		outputStream.close()
		if notUpdated == False:
			logger.info("Performed in-place update")

if __name__ == "__main__":
    main()