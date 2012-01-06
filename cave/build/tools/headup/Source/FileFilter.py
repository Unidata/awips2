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
import csv
import re

class FileFilter:

	def __init__(self,filterFile=None):
		self.excludes = []
		self.includes = []
		self.fileTypeIncludes = []
		
		if filterFile != None:
			self.loadFilters(filterFile)

	def loadFilters(self,filterFile):
		filterObject = open(filterFile,"r")
		filterDB = csv.DictReader(filterObject,
			["filterType","pattern","comment"])
		for filterLine in filterDB:
			filterType = filterLine["filterType"]
			if filterType == "exclude":
				self.excludes.append(re.escape(filterLine["pattern"]))
			elif filterType == "include":
				self.includes.append(re.escape(filterLine["pattern"]))
			elif filterType == "excludeRegex":
				self.excludes.append(filterLine["pattern"])
			elif filterType == "includeRegex":
				self.includes.append(filterLine["pattern"])
			elif filterType == "includeFiletype":
				self.fileTypeIncludes.append(re.escape(filterLine["pattern"]))
		filterObject.close()
	
	def getExcludes(self):
		return self.excludes
	
	def getIncludes(self):
		return self.includes
		
	def getExcludeRegex(self):
		return self._getRegex(self.excludes)
	
	def getIncludeRegex(self):
		return self._getRegex(self.includes)
		
	def getFileTypeIncludeRegex(self):
		regexList = []
		for pattern in self.fileTypeIncludes:
			regexList.append(re.compile(".*?"+pattern+"$"))
		return regexList
	
	def _getRegex(self,patternList):
		regexList = []
		for pattern in patternList:
			regexList.append(re.compile(".*?"+pattern+".*?"))
		return regexList
	
	def _isExcludedFile(self,fileName):
		foundMatch = False
		for exclude in self.getExcludeRegex():
			if exclude.match(fileName) != None:
				foundMatch = True
		return foundMatch
	
	def getFilteredFiles(self,fileList):
		if len(self.includes) == 0 and len(self.excludes) == 0:
			return fileList
		
		result = fileList

		fileTypeRegex = self.getFileTypeIncludeRegex()
		includeRegex = self.getIncludeRegex()
		excludeRegex = self.getExcludeRegex()

		filteredFileList = []
		if len(fileTypeRegex) > 0:
			for fileName in result:
				for finclude in fileTypeRegex:
					if finclude.match(fileName):
						filteredFileList.append(fileName)
			result = filteredFileList

		filteredFileList = []
		if len(includeRegex) > 0:
			for fileName in result:
				for include in includeRegex:
					if include.match(fileName) != None:
						if len(excludeRegex) > 0:
							if not(self._isExcludedFile(fileName)):
								filteredFileList.append(fileName)							
						else:
							filteredFileList.append(fileName)
			result = filteredFileList
		else:
			for fileName in result:
				if not(self._isExcludedFile(fileName)):
					filteredFileList.append(fileName)
			result = filteredFileList
		
		return result
			
			
