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

import AlertVizProcessor

from com.raytheon.uf.viz.python.swt.widgets import LabelWidget
from com.raytheon.uf.viz.python.swt.widgets import PushButtonWidget
from com.raytheon.uf.viz.python.swt import Window
from java.util import ArrayList

#
# A debug processor that opens a window with a button callback
#
# This demonstrates how it is possible to create a SWT GUI that has
# buttons with Python Callbacks
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/15/09                      chammack       Initial Creation.
#    
# 
#
class DebugUIProcessor(AlertVizProcessor.AlertVizProcessor):
      
    class Callback:
        def run(self):
           al = ArrayList()
         
           Window.open("Callback Successful", al, False)
        
    def process(self, statusMessage, alertMetadata, globalConfiguration):
         al = ArrayList()
         pb = PushButtonWidget.withText("Test Button")
         cb = DebugUIProcessor.Callback()
         pb.setCallback(self.createRunnableCallback(cb))
         al.add(pb)
         
         Window.open("Testing", al, False)