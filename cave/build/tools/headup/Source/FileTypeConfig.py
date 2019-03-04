##
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
	
