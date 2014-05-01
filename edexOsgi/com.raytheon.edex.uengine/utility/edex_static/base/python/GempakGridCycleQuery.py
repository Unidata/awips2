#
# GempakGridCycleQuery
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a GempakSqlQuery for Grid data from GEMPAK and returns
# all the available unique forecast times for the model's cycle.
#
#     Usage:
#    import GempakGridCycleQuery
#    query = GempakGridCycleQuery.GempakGridCycleQuery()
#    query.setParameters("ruc|1300")
#    return query.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    07/02/10        92            mgamazaychikov       Initial Creation
#
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.uengine.tasks.query import MetadataCatalogQuery
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert
import GempakSqlQuery

class GempakGridCycleQuery():
    
    def __init__(self, aPlugin):
        self.plugin = aPlugin
        self.queryResults = ArrayList()
        self.GSQ = GempakSqlQuery.GempakSqlQuery()
        self.GSQ.setReturnText()
        self.convert = GempakConvert()
    
#
# Set parameters of the metadata catalog query
#
    def setParameters (self, aParms):
        parms = aParms.split("|")
        self.model=parms[0].upper()
        self.cycle = self.convert.dattimToDbtime(parms[1].upper()).replace (' ', '_')
        #print "model=", self.model
        #print "cycle=", self.cycle
        
#
# Generates a list of responses in XML format
#
    def makeResponse(self):
        retStr = self.convert.dbtimeToDattim((self.queryResult).split("/")[2])
        #print "returning time=", retStr
        return ResponseMessageGeneric(retStr)
    
#
# Executes the query and calls appropriate response functions
#    
    def execute(self):
        #
        # construct the SQL query to execute
        #
        myQuery = "SELECT datauri FROM " + self.plugin + " WHERE datauri LIKE '%" + \
                   self.cycle + "%' AND datauri ~* '/" + self.model + "/' LIMIT 1"
        #print "myQuery:", myQuery
        
        #
        # set the SQL query
        # 
        self.GSQ.setQuery(myQuery)
        self.queryResult = self.GSQ.execute()
        #print "self.queryResult=", self.queryResult
        #
        # process the results of the query
        #
        if self.queryResult is None:
            return self.makeNullResponse("Query returned no results")
        else:
            return self.makeResponse()