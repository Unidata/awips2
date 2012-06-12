#
# GempakRadarCatalogQuery
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a MetadataCatalogQuery for data from GEMPAK and returns
# the requested returned field.
#
#    Usage:
#    import GempakRadarCatalogQuery
#    query = GempakRadarCatalogQuery.GempakRadarCatalogQuery("radar")
#    query.addConstraint("...","...")
#    query.addConstraint("...","...")
#    query.setDistinctField("...");
#    return query.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    12/22/09        173_partB     mgamazaychikov       Initial Creation
#
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.uengine.tasks.query import MetadataCatalogQuery
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakRadarCatalogQuery():
    
    def __init__(self, pluginName):
        self.queryResults = ArrayList()
        self.pluginName = pluginName
        self.query = MetadataCatalogQuery(pluginName)
        
    def setDistinctField(self,field):
        self.distinctField = field
        self.query.setDistinctField(field)
    
    def addConstraint(self, name, value, operand="="):   
        self.query.addParameter(name, value, operand)
                
#
# Generates a list of responses in XML format
#
    def makeResponse(self):
        convert = GempakConvert(self.pluginName, self.queryResults)
        return convert.ConvertRadarRMC(self.distinctField)

#
# Returns a string with null response  
#
    def makeNullResponse(self):
        response = ArrayList()
        response.add("Database Query returned no results")
        return response
    
#
# Executes the query and calls appropriate response functions
#    
    def execute(self):
        name = "productCode"
        value = "2"
        operand = "!="
        self.query.addParameter(name, value, operand)
        self.queryResults = self.query.execute()
        if self.queryResults is None:
            self.makeNullResponse()
        else:
            return self.makeResponse()
