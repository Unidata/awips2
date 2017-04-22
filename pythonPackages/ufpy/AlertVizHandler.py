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
# Pure python logging mechanism for logging to AlertViz from
# pure python (ie not JEP).  DO NOT USE IN PYTHON CALLED
# FROM JAVA.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/18/10                      njensen       Initial Creation.
#    
# 
#

import logging
import NotificationMessage
    
class AlertVizHandler(logging.Handler):

    def __init__(self, host='localhost', port=61999, category='LOCAL', source='ANNOUNCER', level=logging.NOTSET):
        logging.Handler.__init__(self, level)        
        self._category = category
        self._host = host
        self._port = port
        self._source = source
        
    
    def emit(self, record):        
        "Implements logging.Handler's interface.  Record argument is a logging.LogRecord."
        priority = None
        if record.levelno >= 50:
            priority = 'CRITICAL'
        elif record.levelno >= 40:
            priority = 'SIGNIFICANT'
        elif record.levelno >= 30:
            priority = 'PROBLEM'
        elif record.levelno >= 20:
            priority = 'EVENTA'
        elif record.levelno >= 10:
            priority = 'EVENTB'
        else:
            priority = 'VERBOSE'
        
        msg = self.format(record)
                    
        notify = NotificationMessage.NotificationMessage(self._host, self._port, msg, priority, self._category, self._source)
        notify.send()
         