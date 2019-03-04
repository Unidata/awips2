##
##

##
# This is a base file that is not intended to be overridden.
##

##
# uengine is deprecated and will be removed from the system soon. Migrate your
# apps to using the Data Access Framework (DAF).
##

#
# XML request of data
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/02/08                      M. Duff        Initial Creation.
#    
# 



from java.util import ArrayList
from com.raytheon.edex.uengine.tasks.query import ShefQuery


class ShefLocationQuery(BaseRequest.BaseRequest):
    
    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "")
        self.queryResults = None        
        self.query = ShefQuery()
        
        
    