#
# GempakDataURIRequest
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a query of a database table ncgrib and returns datauri string.
# Could be re-factored for other tables. 
#
#     Usage:
#    import GempakDataURIRequest
#    query = GempakDataURIRequest.GempakDataURIRequest("grib")
#    query.setDataParms("datauri!GDFILE|DATTIM|GVCORD|GLEVEL|GPARM")
#    return query.execute();
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    04/30/10        173_partC     mgamazaychikov       Initial Creation
#    05/05/11                      mgamazaychikov       Allowed the separator to be '!' or ':'
#    08/17/11                      mgamazaychikov       Re-wrote to use TableQuery class
#

from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.uf.edex.database.plugin import PluginFactory
from com.raytheon.edex.uengine.tasks.query import TableQuery        
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakDataURIRequest():
    
    def __init__(self, pluginName):       
        #
        # create the TableQuery instance
        #
        try:
            className = PluginFactory.getInstance().getPluginRecordClassName(pluginName)
        except:
            #
            # handle the exception if the plugin record class cannot be found
            #
            message = "RunTimeError getting the PluginRecordClassName for " + \
                pluginName + " "
            import sys
            if sys.exc_info()[1] is not None:               
                str = "%s" % sys.exc_info()[1]
                indx =  str.find("\n")           
                if indx > 0:
                    message = message + str[:indx]
            print "Unexpected error:" + message
            return self.__makeNullResponse(message)
        databaseName = "metadata"        
        #
        # create the TableQuery instance for specified database
        # and plugin record class
        #
        self.query = TableQuery(databaseName,className)
        
    def setDataParms(self, inpStr):
        #
        # find out which separator is used - "!" or ":"
        #
        if inpStr.find("!") < 0:
            sepChar = ":"
        else:
            sepChar = "!"
        #
        # list of field names in inpStr delineated by by '|'
        #
        nameList = self.__getNamelist()   
        #
        # list of values in inpStr delineated by by '|'
        #
        parmsList = (inpStr.split(sepChar)[1]).split("|")       
        #
        # dictionary of name-value based on inpStr
        #
        inpDict = dict(zip(nameList, parmsList))        
        #
        # modify the dictionary items to make them db centric
        #
        refTime, fcstTime = self.__getDbTime(inpDict["dattim"])
        print "refTime2=", refTime
        print "fcstTime2=", fcstTime
        inpDict["dataTime.refTime"] = refTime[0:-2]
        inpDict["dataTime.fcstTime"]= fcstTime
        #inpDict["dataTime.refTime"], inpDict["dataTime.fcstTime"] = self.__getDbTime(inpDict["dattim"])
        del inpDict["dattim"]       
        inpDict["modelName"] = self.__getDbModelInfo(inpDict["modelName"])
        #
        # set to return dataURI field
        #
        self.query.addReturnedField("dataURI", None)
        #
        # set the query parameters
        #
        print "inpDict=", inpDict
        for key, value in inpDict.items():
            self.query.addParameter (key, value)
    
    def execute(self):
        #
        # execute the set query
        #
        try:
            queryResult = ArrayList()
            queryResult = self.query.execute()
            size = queryResult.size()
            print "queryResult = ", queryResult
            print "queryResult size = ", size
        except:
            message = "RunTimeError executing TableQuery "
            import sys
            if sys.exc_info()[1] is not None:
                str = "%s" % sys.exc_info()[1]
                indx =  str.find("\n")           
                if indx > 0:
                    message = message + str[:indx]
            print "Unexpected error:" + message
            return self.__makeNullResponse(message)
        #
        # process the results of the query
        #
        if queryResult is None or size==0:
            return self.__makeNullResponse("Error query returned no results")
        else:
            return self.__makeResponse(queryResult)
    
    def __isMixed(self, s):
        seen_upper = seen_lower = False
        for c in s:
            if c.isupper(): seen_upper = True
            if c.islower(): seen_lower = True
            if seen_upper and seen_lower:
                return True
        return False
    
    def __getNamelist (self):
        return ['modelName', 'dattim', 'vcord', 'glevel1', 'parm', 'eventName']  

    def __getDbTime (self, aDattim):
        refTime = aDattim.upper().split("F")[0]
        fcstTime = "%s" % (int(aDattim.upper().split("F")[1]) * 3600)
        convert = GempakConvert()
        refTime = convert.dattimToDbtime(refTime)
        print "refTime=", refTime
        print "fcstTime=", fcstTime
        return refTime, fcstTime
    
    def __getDbModelInfo (self, aMdl):
        if self.__isMixed( aMdl ):
            return aMdl
        else:
            return aMdl.upper()
    
    def __makeResponse(self, aResponse):        
        response = ArrayList()
        aResponse0 = "%s" % aResponse.get(0)
        print "aResponse=", aResponse
        print "aResponse0=", aResponse0
        response.add(ResponseMessageGeneric(aResponse0))
        return response
    
    def __makeNullResponse(self, aMessage=None):
        return ResponseMessageGeneric(aMessage)