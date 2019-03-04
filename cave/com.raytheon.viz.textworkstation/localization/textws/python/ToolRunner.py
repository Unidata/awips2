##
##


#
# Provides a way to run a tool script
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/13/09                      rjpeter        Initial Creation.
#    
# 
#
        
##
# This is a base file that is not intended to be overridden.
##

def process(script, pythonTextEditorToolCallback, args):
    module = __import__(script)
    clazz = module.__dict__.get(script)
    return clazz().process(pythonTextEditorToolCallback, args)