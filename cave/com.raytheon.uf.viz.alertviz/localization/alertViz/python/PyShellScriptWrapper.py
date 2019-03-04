##
##

import AlertVizProcessor
import subprocess

#
# A debug processor that sends messages to standard out
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/03/11        9101          cjeanbap       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

class PyShellScriptWrapper(AlertVizProcessor.AlertVizProcessor):

    def process(self, statusMessage, alertMetadata, globalConfiguration):
        scriptName = str(statusMessage.getDetails())
        #print "script name: " + scriptName
        if (not (scriptName.endswith(".tcl"))):
            #print "/bin/sh"
            subprocess.Popen(['/bin/sh', scriptName])
        else:
            #print "/bin/tclsh"
            subprocess.Popen(['/usr/bin/tclsh', scriptName])
