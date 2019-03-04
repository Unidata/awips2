##
##

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
#    03/24/17        DR 16895      D. Friedman    Removed callbacks
#    
#

##
# This is a base file that is not intended to be overridden.
##

class AlertVizProcessor:

    def process(self, statusMessage, alertMetadata, globalConfiguration):
        raise Exception("Method should be implemented: %s" % _functionId(obj, 1))
