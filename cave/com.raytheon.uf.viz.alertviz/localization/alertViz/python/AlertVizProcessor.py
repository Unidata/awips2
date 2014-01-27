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

from com.raytheon.uf.viz.alertviz import AlertVizPythonCallback
from com.raytheon.uf.viz.alertviz import AlertVizClient

import jep

#
# Provides a base class to implement alertviz message processing capability
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/09/09                      chammack       Initial Creation.
#    
# 
#
class AlertVizProcessor:
        
    def process(self, statusMessage, alertMetadata, globalConfiguration):
        raise Exception("Method should be implemented: %s" % _functionId(obj, 1))
    
    def createRunnableCallback(self, callback):
        proxy = jep.jproxy(callback, ['java.lang.Runnable'])
        return AlertVizPythonCallback(proxy)
    
    def sendCallbackMessage(self, str):
        AlertVizClient.sendCallbackMessage(str)
    