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
# Latest time query wrapper 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/14/08                      njensen       Initial Creation.
#    
# 



from com.raytheon.edex.uengine.tasks.query import LatestTimeQuery


class LatestTime():
    def __init__(self, uriList):
        self.__ltq = LatestTimeQuery(uriList)
    
    def execute(self):
        return self.__ltq.execute()
        