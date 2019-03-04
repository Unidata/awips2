# #
# #

#
# Class with abstract methods for use with localization files.  
# One implementation of this will be a pure python implementation 
# for use outside of the AWIPS II architecture, and one will follow 
# the AWIPS II architecture.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash        Initial Creation.
#    
# 
#

import abc

class IPathManager(object):
    
    __metaclass__ = abc.ABCMeta
    
    def __init__(self):
        return
    
    @abc.abstractmethod
    def getLocalizationFile(self, name, context=None):
        pass
    
    @abc.abstractmethod
    def getTieredLocalizationFile(self, loctype, name):
        pass
    
    @abc.abstractmethod
    def listFiles(self, name, extensions, recursive, filesOnly, loctype=None, loclevel=None, locname=None):
        pass
    
    @abc.abstractmethod
    def getAvailableLevels(self):
        pass