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
# Retrieves the class associated with a given plugin
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2/6/2008        1990          bphillip       Initial Creation.
#
#



from com.raytheon.uf.edex.database.plugin import PluginFactory
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList


class GetPluginRecordClass():
    
    def __init__(self, pluginName):
        self.__plugin = pluginName
    
    def execute(self):
        response = ArrayList()
        response.add(ResponseMessageGeneric(PluginFactory.getInstance().getPluginRecordClassName(self.__plugin)))
        return response 