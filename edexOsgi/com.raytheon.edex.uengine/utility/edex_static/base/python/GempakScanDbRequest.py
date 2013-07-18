#
# GempakScanDbRequest
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a query of a database table ncgrib and returns ensemble members string.
# Could be re-factored for other tables. 
#
#     Usage:
#    import GempakScanDbRequest
#    query = GempakScanDbRequest.GempakScanDbRequest("ncgrib")
#    query.setParameters("modelname|dbtag|eventtag|timetmplt")
#    return query.execute();
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    08/03/11        173_partC     mgamazaychikov       Initial Creation
#
#
from string import Template
import re
import GempakSqlQuery
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakScanDbRequest():
    
    def __init__(self, pluginName):
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
    
    def setParameters(self, inpStr):
        
        #
        # list of values in inpStr delineated by by '|'
        #
        parmsList = inpStr.split("|")
        
        modelName = parmsList[0]
        dbTag     = parmsList[1]
        eventTag  = parmsList[2]
        if eventTag.find("*") > 0:
            eventTag = eventTag.split("*")[0]

        gempakTimeStr = parmsList[3]
        self.prefix = modelName + "_" + dbTag + "_"
        duriTimeSearchString = self.__constructDuriTimeString(gempakTimeStr)
       #print "duriTimeSearchString=", duriTimeSearchString
        
        #
        # construct the SQL query to execute
        #

        myQuery = "SELECT distinct reftime, forecasttime, eventname FROM " + \
               self.pluginName + " WHERE " + \
              "modelname='" + modelName + \
              "' AND eventname LIKE '" + eventTag + \
              "%' AND datauri LIKE '%" + duriTimeSearchString + "%'"          
       #print "myQuery====", myQuery
        #
        # set the SQL query
        # 
        self.GSQ.setQuery(myQuery)
    
    def __constructDuriTimeString(self,gempakTimeStr):
        cycleKeys = 'YYYY|MM|DD|HH'
        
        gempakTimeStrCycle   = gempakTimeStr.split("f")[0]
        gempakTimeStrCycle   = gempakTimeStrCycle.replace("[0-9]", "x")
        gempakTimeStrFFF     = gempakTimeStr.split("f")[1]
        gempakTimeStrFFF     = gempakTimeStrFFF.replace("[0-9]", "%")
        
        gempakCyclePattern = re.compile(r'^(\w{4})(\w{2})(\w{2})(\w{2})$')
        gempakCycleValues  = gempakCyclePattern.search(gempakTimeStrCycle).groups()
        
        duriCycleFormat = '$YYYY-$MM-$DD $HH%'
        duriCycleTmplt = Template(duriCycleFormat)
        duriCycleKeys = tuple(cycleKeys.split("|"))
              
        gempakToDuri = dict(zip(duriCycleKeys, gempakCycleValues))
        
        duriTimeStrCycle = duriCycleTmplt.safe_substitute(gempakToDuri)
        duriTimeStrCycle = duriTimeStrCycle.replace("x","%")
        duriTimeStrCycle = duriTimeStrCycle.replace(" ","_")
        
        duriTimeStrFFF = "_(" + gempakTimeStrFFF
        duriTimeStrFFF     = re.sub(r'\([0]{1,2}','(', duriTimeStrFFF)
        
        return duriTimeStrCycle + duriTimeStrFFF
    
    def __constructReturnString(self,gsqResults):
        cycleKeys = 'YYYY|MM|DD|HH'
        
        gempakCycleKeys = tuple(cycleKeys.split("|"))
        
        duriCyclePattern = re.compile(r'^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2}\.0)$')
        
        gempakCycleFormat = '$YYYY$MM$DD$HH'
        gempakCycleTmplt = Template(gempakCycleFormat)
        
        returnString=""
        
        gsqResultsList = gsqResults.split("|")
        
        for str in gsqResultsList:
            lst = str.split(",")
            cycle = lst[0]
            forecastTime = lst[1]
            eventName = lst[2].strip(" ")
            duriCycleValues = duriCyclePattern.search(cycle).groups()
            
            duriToGempak = dict(zip(gempakCycleKeys, duriCycleValues))
            
            gempakTimeStrCycle = gempakCycleTmplt.safe_substitute(duriToGempak)
            
            gempakFFF = 'f%03d|' % ( int(forecastTime) / 3600 )
            
            returnString = returnString + self.prefix + eventName + "_" + \
                           gempakTimeStrCycle + gempakFFF
         
       #print "returnString=", returnString[0:-1]  
        return returnString[0:-1]
        
    def execute(self):
        if self.GSQ is None:
            return self.makeNullResponse("Accessing a non-existent dictionary key")
        else:
            #
            # execute the set query
            #
            gsqResults = self.GSQ.execute()
           #print "gsqResults=", gsqResults
            if gsqResults is None:
                return self.__makeNullResponse("Query returned no results")
            else:                
                self.returnString = self.__constructReturnString(gsqResults)
               #print "self.returnString=", self.returnString
            
                #
                # process the results of the query
                #
                if self.returnString is None:
                    return self.__makeNullResponse("Query returned no results")
                else:
                   #print "calling self.__makeResponse"
                    return self.__makeResponse()
    
    def __makeResponse(self):        
        response = ArrayList()
            
        response.add(ResponseMessageGeneric(self.returnString))
        return response
    
    def __makeNullResponse(self, aMessage=None):
        return ResponseMessageGeneric(aMessage)