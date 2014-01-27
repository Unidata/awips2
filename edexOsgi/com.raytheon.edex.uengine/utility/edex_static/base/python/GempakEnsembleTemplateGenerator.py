

from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.uf.edex.database.plugin import PluginFactory
from com.raytheon.edex.uengine.tasks.query import TableQuery        
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakEnsembleTemplateGenerator():
    
    def __init__(self, pluginName):
        self.pluginName = pluginName
        self.eventName = None 
        self.perturbationNum = 0     
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
        self.query.setDistinctField("eventName")
        
    def setEnsembleName(self, modelName):
        self.modelName = modelName
        self.query.addParameter ("modelName", modelName)

    def setEnsembleEventName(self, eventName):
        self.eventName = eventName
        self.query.addParameter ("eventName", eventName)

    def setEnsemblePerturbationNumber(self, perturbationNum):
        self.perturbationNum = perturbationNum
        self.query.addParameter ("modelInfo.perturbationNumber", perturbationNum)
                        
    def execute(self):
        #
        # execute the set query
        #
        try:
            queryResult = ArrayList()
            queryResult = self.query.execute()
#            print "queryResult = ", queryResult
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
        if queryResult is None:
            return self.__makeNullResponse("Query returned no results")
        else:
            return self.__makeResponse(queryResult)
        
    def __makeResponse(self, ensArrayList):
        size = ensArrayList.size()
        ensList = []
        for i in range(size):
            ensList.append("%s" % ensArrayList.get(i))
        commonStart = self.__findCommonStart(ensList)
#        print "commonStart= ", commonStart
#        if self.perturbationNum == 0:
#            ensTemplate = self.modelName + "_db_" + commonStart + "*_YYYYMMDDHHfFFF"
#        else:
        ensTemplate = self.modelName + "_db_" + commonStart + "_YYYYMMDDHHfFFF"
        response = ArrayList()
        response.add(ResponseMessageGeneric(ensTemplate))
        return response
    
    def __makeNullResponse(self, aMessage=None):
        return ResponseMessageGeneric(aMessage)
    
    def __findCommonStart (self, strlist):
        strlist = strlist[:]
        prev = None
        while True:
            common = self.__getCommonLetters(strlist)
            if common == prev:
                break
            strlist.append(common)
            prev = common
        return self.__getCommonLetters(strlist)
    
    def __getCommonLetters(self, strlist):
        return ''.join([x[0] for x in zip(*strlist) \
                if reduce(lambda a,b:(a == b) and a or None,x)])