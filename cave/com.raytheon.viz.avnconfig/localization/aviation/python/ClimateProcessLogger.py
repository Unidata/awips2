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