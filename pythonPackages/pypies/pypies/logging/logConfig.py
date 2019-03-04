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
#    05/13/11                      njensen       Initial Creation.
#    01/17/13        1490          bkowal        The logging tcp port is now configurable
# 
#


import logging, os, ConfigParser
import logging.handlers, logging.config
import pypies.config.pypiesConfigurationManager

class LogConfig:
    
    def __init__(self):
	pypiesConfigurationManager = pypies.config.pypiesConfigurationManager.PypiesConfigurationManager()

        if pypiesConfigurationManager.hasConfigurationBeenLoaded():
            self.__configure(pypiesConfigurationManager)
            self.pypiesLogger = logging.getLogger('root')
            self.minutesLogger = logging.getLogger('minute')
            self.hoursLogger = logging.getLogger('hourly')
        else:
            self.pypiesLogger = self.__getDefaultLogger()
            self.minutesLogger = self.pypiesLogger
            self.hoursLogger = self.pypiesLogger
            self.loggingPort = logging.handlers.DEFAULT_TCP_LOGGING_PORT
    
    def __configure(self, configurationManager):
        scp = configurationManager.getConfiguration()
        print "using", configurationManager.getConfigurationLocation(), "for logging config"

        logFileDir = scp.get('handler_pypiesHandler', 'logFileDir')
        if not os.path.exists(logFileDir):
            os.makedirs(logFileDir)
        logging.config.fileConfig(configurationManager.getConfigurationLocation())
        
        self.loggingPort = int(scp.get('tcp_logger', 'logging_port'))    
    
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
    
    def getLoggingPort(self):
        return self.loggingPort

