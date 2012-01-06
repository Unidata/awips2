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
#    05/13/11                      njensen       Initial Creation.
#    
# 
#


import logging, os, ConfigParser
import logging.handlers, logging.config

class LogConfig:
    
    def __init__(self):
        cfgLoc = self.__getConfigLocation()        
        if cfgLoc:
            scp = self.__loadConfig(cfgLoc)
            self.pypiesLogger = logging.getLogger('root')
            self.minutesLogger = logging.getLogger('minute')
            self.hoursLogger = logging.getLogger('hourly')
        else:
            self.pypiesLogger = self.__getDefaultLogger()
            self.minutesLogger = self.pypiesLogger
            self.hoursLogger = self.pypiesLogger

    def __getConfigLocation(self):        
        configLoc = '/awips2/pypies/conf/pypies.cfg'
        if not os.path.exists(configLoc):
            print "Unable to find pypies.cfg at ", configLoc
            configLoc = None
        else:
            print "Found pypies.cfg at ", configLoc        
        return configLoc
    
    def __loadConfig(self, configLoc):
        scp = ConfigParser.SafeConfigParser()
        if not configLoc:
            raise RuntimeError("No pypies.cfg found")
        else:
            print "using", configLoc, "for logging config"
        scp.read(configLoc)
        logFileDir = scp.get('handler_pypiesHandler', 'logFileDir')
        if not os.path.exists(logFileDir):
            os.makedirs(logFileDir)
        logging.config.fileConfig(configLoc)    
    
    def __getDefaultLogger(self):
        import logging, logging.handlers
        log_filename = '/tmp/pypies.log'
        logger = logging.getLogger('pypies')
        handler = logging.handlers.TimedRotatingFileHandler(log_filename, 'midnight', backupCount=7, utc=True)
        handler.setLevel(logging.INFO)
        logger.setLevel(logging.INFO)
        formatter = logging.Formatter('%(levelname)s %(asctime)s %(message)s')
        handler.setFormatter(formatter)
        logger.addHandler(handler)
        return logger                            
    
    def getPypiesLogger(self):
        return self.pypiesLogger
    
    def getMinutesLogger(self):
        return self.minutesLogger
    
    def getHoursLogger(self):
        return self.hoursLogger


