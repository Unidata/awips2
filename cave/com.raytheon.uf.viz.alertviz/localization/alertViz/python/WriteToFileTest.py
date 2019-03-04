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

class WriteToFileTest(AlertVizProcessor.AlertVizProcessor):

    def process(self, statusMessage, alertMetadata, globalConfiguration):
        testFile = open('/tmp/AlertVizPyTest.txt', 'a')
        testFile.write("AV Python Test -- %s %s %s \n" % (statusMessage.getPriority(), statusMessage.getCategory(), statusMessage.getMessage()))
        testFile.close()
