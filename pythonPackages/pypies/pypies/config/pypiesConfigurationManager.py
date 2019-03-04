##
##


#
# Configuration for pypies logging
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/10/13                      bkowal         Initial Creation.
#    01/17/13        #1490         bkowal         The location of pypies.cfg is
#                                                 now retrieved from the environment.
#    05/20/16                      mjames         Remove print on finding pypies.cfg
#                                                 which was incorrectly logged as an error.
# 

import os, ConfigParser

class PypiesConfigurationManager:
    
    def __init__(self):
        self.__configLoaded = False
        
        self.__initConfigLocation()
        if (not self.__configLoc):
            raise RuntimeError("No pypies.cfg found")
        
        self.__loadConfig()

    def __initConfigLocation(self):        
        self.__configLoc = os.environ["PYPIES_CFG"]
        if not os.path.exists(self.__configLoc):
            print "Unable to find pypies.cfg at ", self.__configLoc
            self.__configLoc = None
        
    def __loadConfig(self):
        self.__scp = ConfigParser.SafeConfigParser()
        self.__scp.read(self.__configLoc)
        self.__configLoaded = True

    def getConfigurationLocation(self):
        return self.__configLoc
        
    def hasConfigurationBeenLoaded(self):
        return self.__configLoaded
        
    def getConfiguration(self):
        return self.__scp
