#
# GempakEnsMemberRequest
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a query of a database table ncgrib and returns ensemble members string.
# Could be re-factored for other tables. 
#
#     Usage:
#    import GempakEnsMemberRequest
#    query = GempakEnsMemberRequest.GempakEnsMemberRequest("ncgrib")
#    query.setDataParms("modelName|eventName|DATTIM")
#    return query.execute();
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    08/03/11        173_partC     mgamazaychikov       Initial Creation
#
#
import GempakSqlQuery
import GempakParmDict
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakEnsMemberRequest(GempakSqlQuery.GempakSqlQuery):
    
    def __init__(self, pluginName):
        self.eventName = None
        #
        # create the GempakSqlQuery instance
        #
        self.GSQ = GempakSqlQuery.GempakSqlQuery()
        
        #
        # set the return type to text(string)
        # not a ResponseMessageGEneric object 
        #
        self.GSQ.setReturnText()
        self.GSQ.setSeparator("|")
        self.pluginName = pluginName
    
    def setDataParms(self, inpStr):
        
        #
        # list of values in inpStr delineated by by '|'
        #
        parmsList = inpStr.split("|")
        
        modelName = parmsList[0]
        eventName = parmsList[1]
        dattim = parmsList[2]
        dbTime = self.__getDbTime(dattim)
        
        #
        # construct the SQL query to execute
        #

        myQuery = "SELECT distinct eventname FROM " + \
               self.pluginName + " WHERE " + \
              "modelname='" + modelName + \
              "' AND eventname LIKE '%" + eventName + \
              "%' AND datauri LIKE '%/" + dbTime + "/%'"          
        print "myQuery====", myQuery
        #
        # set the SQL query
        # 
        self.GSQ.setQuery(myQuery)
        
          
    def __getDbTime (self, aDattim):
        if self.pluginName == 'grib' or self.pluginName == 'ncgrib':
            convert = GempakConvert()
            return convert.dattimToDbtime(aDattim)           
        else:
            return None
        
    def execute(self):
        if self.GSQ is None:
            return self.makeNullResponse("Accessing a non-existent dictionary key")
        else:
            #
            # execute the set query
            #
            self.queryResult = self.GSQ.execute()
            print "self.queryResult=", self.queryResult
            
            #
            # process the results of the query
            #
            if self.queryResult is None:
                return self.__makeNullResponse("Query returned no results")
            else:
                return self.__makeResponse()
    
    def __makeResponse(self):        
        response = ArrayList()
            
        response.add(ResponseMessageGeneric(self.queryResult))
        return response
    
    def __makeNullResponse(self, aMessage=None):
        return ResponseMessageGeneric(aMessage)