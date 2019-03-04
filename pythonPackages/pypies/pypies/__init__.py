##
##


#
# __init__.py for pypies package
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/27/10                      njensen       Initial Creation.
#    01/17/13        1490          bkowal        Retrieve the hdf5 root directory
#                                                and the logging tcp port from
#                                                configuration.
# 
#


import IDataStore
import sys, os
import pypies.config.pypiesConfigurationManager

def configure():
    pypiesConfigurationManager = pypies.config.pypiesConfigurationManager.PypiesConfigurationManager()
    if (not pypiesConfigurationManager.hasConfigurationBeenLoaded()):
        # this case is unlikely
        print 'Failed to load the pypies configuration!'
        sys.exit(-1)
    
    return pypiesConfigurationManager

def getLogger(scp):
    import logging, logging.handlers
    
    loggingPort = int(scp.get('tcp_logger', 'logging_port'))
    
    logger = logging.getLogger('pypies')
    logger.setLevel(logging.INFO)
    socketHandler = logging.handlers.SocketHandler('localhost', loggingPort)
    # don't bother with a formatter, since a socket handler sends the event as
    # an unformatted pickle
    logger.addHandler(socketHandler)
    return logger

def getHdf5Dir(scp):
    # determine the edex hdf5 root
    hdf5Dir = scp.get('edex_data', 'hdf5dir')
    # add a trailing directory separator (when necessary)
    if (not hdf5Dir.endswith('/')):
        hdf5Dir = hdf5Dir + '/'
        
    if not os.path.exists(hdf5Dir):
       os.makedirs(hdf5Dir)
    infoMessage = 'using hdf5 directory: ' + hdf5Dir
    logger.info(infoMessage)
    
    return hdf5Dir

pypiesCM = configure()
scp = pypiesCM.getConfiguration()
logger = getLogger(scp)
timeMap = {}
hdf5Dir = getHdf5Dir(scp)


def pypiesWrapper(request):
    import handlers
    return lambda request: handlers.PypiesHandler(request)
    

class NotImplementedException(Exception):
    
    def __init__(self, message=None):
        self.message = message
    
    def __str__(self):
        if self.message:
            return self.message 
        else:
            return ""

class StorageException(Exception):
    def __init__(self, message=None):
        self.message = message
    
    def __str__(self):
        if self.message:
            return self.message 
        else:
            return ""

            