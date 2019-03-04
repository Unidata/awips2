##
##

#
# Provides a base class to implement text editor tool localization capability
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

class BaseTool:
    
     def process(self, pythonTextEditorToolCallback, args):
        raise Exception("Method should be implemented: %s" % _functionId(obj, 1))
         