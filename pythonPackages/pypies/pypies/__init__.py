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
# __init__.py for pypies package
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/27/10                      njensen       Initial Creation.
#    
# 
#


import IDataStore

def getLogger():
    import logging, logging.handlers
    logger = logging.getLogger('pypies')
    logger.setLevel(logging.INFO)
    socketHandler = logging.handlers.SocketHandler('localhost', logging.handlers.DEFAULT_TCP_LOGGING_PORT)
    # don't bother with a formatter, since a socket handler sends the event as
    # an unformatted pickle
    logger.addHandler(socketHandler)
    return logger

logger = getLogger()


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

            