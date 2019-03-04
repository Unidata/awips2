##
#

#
# Used to separate out climate logging for no confusion of UFStatusHandler 
# and AlertVizHandler
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/17/10                      mnash       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

import logging
from awips import AlertVizHandler

PLUGIN_NAME = 'com.raytheon.viz.aviation'
CLIMATE_CATEGORY = "CLIMATE"
_ClimateLogger = logging.getLogger(CLIMATE_CATEGORY)

try:
    _ClimateLogger.addHandler(AlertVizHandler.AlertVizHandler('localhost', 61999, PLUGIN_NAME, CLIMATE_CATEGORY, level=logging.INFO))
except Exception, e:
    logging.basicConfig(filename='/tmp/avnfps.log',level=logging.DEBUG)
    _ClimateLogger = logging.getLogger()