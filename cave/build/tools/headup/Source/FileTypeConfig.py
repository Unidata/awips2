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

from ConfigParser import ConfigParser
from ConfigParser import NoOptionError
from os import pathsep
from os import listdir
from os.path import join

class FileTypeConfig(ConfigParser):
	""" Handles file type configurations
	"""
	
	def __init__(self,defaultConfig = None,configDirectories = None,
				fileType = None):
		self.fileType = fileType
	
		dConf = {"space":" "}
		if defaultConfig != None:
			dConf.update(defaultConfig)
	
		ConfigParser.__init__(self,dConf)
		
		if configDirectories != None:
			self.loadConfig(configDirectories)
	
	def isAvailable(self,fileType = None):
		if fileType == None:
			fileType = self.fileType
		return self.has_section(fileType)
	
	def loadConfig(self,configDirectories):
		for path in configDirectories.split(pathsep):
			for file in listdir(path):
				if ".cfg" in file:
					self.read(join(path,file))

	def _getConfig(self,configKey,getterFunction,varDict=None):
		result = None
		try:
			if varDict != None:
				result = getterFunction(self.fileType,configKey,vars=varDict)
			else:
				result = getterFunction(self.fileType,configKey)
		except NoOptionError:
			pass
		return result	

	def getConfig(self,configKey,varDict=None):
		return self._getConfig(configKey,self.get,varDict)

	def getBooleanConfig(self,configKey):
		return self._getConfig(configKey,self.getboolean)
	
