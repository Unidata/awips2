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
        else:
            print "Found pypies.cfg at ", self.__configLoc        
        
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
