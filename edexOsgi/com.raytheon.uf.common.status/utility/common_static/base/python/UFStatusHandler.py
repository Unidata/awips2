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

from com.raytheon.uf.common.status import UFStatus
from com.raytheon.uf.common.status import UFStatus_Priority as Priority
import logging

#
# Python logging mechanism for logging through Java to UFStatus
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/19/09                      njensen       Initial Creation.
#    
# 
#
    
class UFStatusHandler(logging.Handler):

    def __init__(self, pluginName, category, source=None, level=logging.NOTSET):
        logging.Handler.__init__(self, level)
        self._pluginName = pluginName
        self._category = category
        self._source = source
        self._handler = UFStatus.getHandler(self._pluginName, self._source)
        
    
    def emit(self, record):        
        "Implements logging.Handler's interface.  Record argument is a logging.LogRecord."
        priority = None
        if record.levelno >= 50:
            priority = Priority.CRITICAL
        elif record.levelno >= 40:
            priority = Priority.SIGNIFICANT
        elif record.levelno >= 30:
            priority = Priority.PROBLEM
        elif record.levelno >= 20:
            priority = Priority.EVENTA
        elif record.levelno >= 10:
            priority = Priority.EVENTB
        else:
            priority = Priority.VERBOSE
        
        msg = self.format(record)
                    
        self._handler.handle(priority, msg)
