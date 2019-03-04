##
##

#
# Sample script to show capability of running batch list of afos commands
# through localization
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/18/09                      rjpeter        Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

import BaseTool

class ExecuteAFOSCommands(BaseTool.BaseTool):
    
     def process(self, pythonTextEditorToolCallback, arg):
         rval = str()
         list = arg.split(',')
         for s in list:
             products = pythonTextEditorToolCallback.executeAFOSCommand(s)
             for prod in products:
                 if len(prod.getProduct()) > 0:
                     rval += prod.getProduct() + "\n"

         if len(rval) <= 0:
             pythonTextEditorToolCallback.displayMessage('No product in the database matches your request.');

         return rval
         