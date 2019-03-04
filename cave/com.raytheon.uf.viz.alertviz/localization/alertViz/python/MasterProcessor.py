##
##


#
# Provides a controller to fire off processors
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

##
# This is a base file that is not intended to be overridden.
##

def process(statusMessage, alertMetadata, globalConfiguration):
    script = str(alertMetadata.getPythonScript())[:-3]    
    module = __import__(script)
    clazz = module.__dict__.get(script)
    clazz().process(statusMessage, alertMetadata, globalConfiguration)
