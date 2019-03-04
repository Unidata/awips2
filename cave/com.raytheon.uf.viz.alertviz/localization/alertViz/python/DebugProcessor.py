##
##

import AlertVizProcessor

#
# A debug processor that sends messages to standard out
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/09/09                      chammack       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

class DebugProcessor(AlertVizProcessor.AlertVizProcessor):

    def process(self, statusMessage, alertMetadata, globalConfiguration):
         print "%s %s %s" % (statusMessage.getPriority(), statusMessage.getCategory(), statusMessage.getMessage())
