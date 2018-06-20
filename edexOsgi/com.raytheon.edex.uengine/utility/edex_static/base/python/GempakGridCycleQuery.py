##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

##
# This is a base file that is not intended to be overridden.
##

##
# uengine is deprecated and will be removed from the system soon. Migrate your
# apps to using the Data Access Framework (DAF).
##

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