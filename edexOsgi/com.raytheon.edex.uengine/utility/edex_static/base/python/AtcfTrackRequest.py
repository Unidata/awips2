
#
# AtcfTrackRequest
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Request of ATCF track data.
#
#     Usage:
#
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    09/07/10        284            mgamazaychikov       Initial Creation
#    10/10/28        307            mgamazaychikov       Updated to handle time-matching 
import BaseRequest
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class AtcfTrackRequest(BaseRequest.BaseRequest):
    
    def __init__(self):
        BaseRequest.BaseRequest.__init__(self, "atcf")
        self.isTechniqueSet = False
        self.time = None
        self.StartTime = None
        self.EndTime = None
        self.isTimeList= False
        self.AllTimesList = []
        self.TimesList = []
        self.Convert = GempakConvert()
        
    def setTime(self, aTime=None):
        self.time = aTime

    def setStartTime(self, aTime=None):
        if aTime :
            self.StartTime = aTime
        else:
            # set StartTime to the First Available Time
            self.StartTime = self.__getFirstTime()
            
    def setEndTime(self, aTime=None):
        if aTime :
            self.EndTime = aTime
        else:
            # set StartTime to the Last Available Time
            self.EndTime = self.__getLastTime() 
    
    def setTechnique(self, aTechnique):
        self.technique = aTechnique
        self.isTechniqueSet = True
    
    def execute(self):
        if self.time:
            self.query.addParameter("dataTime", self.time)
        else:
            if not self.StartTime:
                self.StartTime = self.__getFirstTime()
            if not self.EndTime:
                self.EndTime = self.__getLastTime()
            if self.StartTime and self.EndTime:
                self.timeRange = self.StartTime + "--" + self.EndTime
                self.query.addParameter("dataTime", self.timeRange, "between")
            else:
                return self.__makeNullResponse("Could not set the time range")
        # get the results from the database              
        self.queryResults = self.query.execute()
        if self.queryResults is None or self.queryResults.size() == 0:
            return self.__makeNullResponse()
        else:
            # process the results
            return self.__makeResponse()
    
    def __getFirstTime(self):
        # check if the list of available times has been calculated            
        if self.isTimeList:
            return self.Convert.dattimToDbtime(self.AllTimesList[0])
        else:
            # get the list of times and return the earliest available time
            self.__getTimeList()
            return self.Convert.dattimToDbtime(self.AllTimesList[0])
        
    def __getLastTime(self):
        # check if the list of available times has been calculated
        if self.isTimeList:
            return self.Convert.dattimToDbtime(self.AllTimesList[len(self.AllTimesList) -1])
        else:
            # get the list of times and return the last available time
            self.__getTimeList()
            return self.Convert.dattimToDbtime(self.AllTimesList[len(self.AllTimesList) -1])
 
              
    
    def __getTimePlus(self, aTime, aPlus="168"):
        return self.Convert.addHours(aTime, aPlus)

#    def __createTimes(self):
#        if self.time:
#            self.TimesList.append(self.time)
#        else:
#            startIndex = self.AllTimesList.index(self.StartTime)
#            endIndex = self.AllTimesList.index(self.EndTime)
#            self.TimesList = self.AllTimesList[startIndex:endIndex]
            
    def __getTimeList(self):
        import GempakCatalogTimeQuery
        tq = GempakCatalogTimeQuery.GempakCatalogTimeQuery("atcf")
        tq.setReturnText()
        self.AllTimesList =  tq.execute().split("|")
        self.isTimeList =True
        
    def __getDictionaries(self):
        import GempakSqlQuery
        # list of time cyclone ID tuples:
        # eg [('2010-08-28 06:00:00.0', 'EP9'), ('2010-08-28 12:00:00.0', 'EP8')]
        timeIdTupleList = []
        
        # timeIdTqDict - is the key-value dictionary of time-Id tuple Technique List dictionary
        # each time has a corresponding dictionary of ID-Technique
        # eg: {('2010-08-28 06:00:00.0','AL8'):['AEMI', 'AEMN', 'AP01', 'AP02'], ('2010-08-28 12:00:00.0','AL9'):['AP11', 'AP14', 'AP18']}
        timeIdTqDict = {}
        
        # idNumberDict - is the key-value dictionary of cycloneID-basin, cycloneNumb tuple pairs
        # each cycloneId has a corresponding basin-cycloneNumb tuple
        # eg: {'AL8': ('AL', '8'), 'AL9': ('AL', '9')}       
        idNumberDict = {}
        
        for ii in range(self.queryResults.size()):
            
            currentRecord = self.queryResults.get(ii)

            # get the time 
            recordTime = str(currentRecord.getDataTime())
            
            # get basin
            basin = str(currentRecord.getBasin())
            # get cyclone number
            cycloneNumb = str(currentRecord.getCycloneNum())
        
            # construct cyclone ID
            # eg EP9
            cycloneId = basin + cycloneNumb
            
            # construct time - cyclone ID tuple
            # eg ('2010-08-21 06:00:00.0', 'EP9')
            timeIdTuple = (recordTime, cycloneId)
             
            # check time - cyclone ID tuple old or new
            if timeIdTuple in timeIdTupleList:
                # existing time - cyclone ID tuple - do not add to list
                pass
            else:
                timeIdTupleList.append(timeIdTuple)
                
                # add the basin,cycloneNumb tuple as value to the idNumberDict dictionary 
                # for the key cycloneId, if has not been added already
                if (basin, cycloneNumb) in idNumberDict.values():
                    pass
                else:
                    idNumberDict[cycloneId] = (basin, cycloneNumb)

                techniqueList = []
                # check if the request is for a single technique
                if self.isTechniqueSet:
                    techniqueList.append(self.technique)
                else:
                    # perform the db query for the records with that cyclone ID
                    techniqueQuery = GempakSqlQuery.GempakSqlQuery()
    
                    # set the return type to text(string)
                    # not a ResponseMessageGEneric object 
                    techniqueQuery.setReturnText()

                    # set the separator
                    techniqueQuery.setSeparator("|")

                    # set the query text
                    techniqueQueryText = "SELECT DISTINCT technique FROM atcf WHERE " + \
                            "basin ='" + idNumberDict[cycloneId][0] + "' AND " +  \
                            "cyclonenum ='" + idNumberDict[cycloneId][1] +  "' AND " + \
                            "reftime='" + recordTime + "'"
                    techniqueQuery.setQuery(techniqueQueryText)
                    
                    # execute the query
                    techniqueQueryResults = techniqueQuery.execute()
                    
                    # create a list of techniques
                    techniqueList = techniqueQueryResults.split("|")
                    
                    # add technique as value to the to the time,cyclone ID - technique dictionary 
                    timeIdTqDict[timeIdTuple] = techniqueList
        return idNumberDict, timeIdTqDict

    def __makeResponse(self):
        from gov.noaa.nws.ncep.edex.uengine.tasks.atcf import AtcfCyclone, AtcfTrack
        response = ArrayList()
        idNumberDict, timeIdTqDict = self.__getDictionaries()
               
        # traverse the time,cyclone ID - technique dictionary to obtain AtcfRecords and
        # construct AtcfCyclone objects
        for key, value in timeIdTqDict.items():                
            currentTime = key[0]
            cycId = key[1]
            techList = value
            aBasin = idNumberDict[cycId][0]
            aCycloneNumb = idNumberDict[cycId][1]

            # create new AtcfCyclone object
            aCyclone = AtcfCyclone(cycId)
            for techniqueName in techList:
                bsResults = None
                # CARQ case
                if techniqueName == "CARQ":
                    # first we need to create track for the past  
                    BaseRequest.BaseRequest.__init__(self, "atcf")
                    self.query.addParameter("dataTime", currentTime)
                    self.query.addParameter("basin",aBasin)
                    self.query.addParameter("cycloneNum",aCycloneNumb)
                    self.query.addParameter("technique",techniqueName)
                    self.query.addParameter("radWind", "34")
                    self.query.addParameter("fcstHour","0", "<")
                    order = True
                    self.query.setSortBy("fcstHour", order)
                    bsResultsPast = self.query.execute()
                    bsResults = bsResultsPast

                    # second we need to create track for the future  
                    BaseRequest.BaseRequest.__init__(self, "atcf")
                    timeRange = currentTime + "--" + self.__getTimePlus(currentTime)
                    self.query.addParameter("dataTime", timeRange, "between")
                    self.query.addParameter("basin",aBasin)
                    self.query.addParameter("cycloneNum",aCycloneNumb)
                    self.query.addParameter("technique",techniqueName)
                    self.query.addParameter("radWind", "34")
                    self.query.addParameter("fcstHour","0")
                    order = True
                    self.query.setSortBy("dataTime", order)                
                    bsResultsFuture = self.query.execute()
                    count = 0
                    for ii in range(bsResultsFuture.size()):
                        recordToChange = bsResultsFuture.get(ii)
                        recordToChange.setFcstHour(ii*6)                       
                        bsResults.add(recordToChange)
                # non-CARQ case
                else:
                    BaseRequest.BaseRequest.__init__(self, "atcf")
                    self.query.addParameter("dataTime", currentTime)
                    self.query.addParameter("basin",aBasin)
                    self.query.addParameter("cycloneNum",aCycloneNumb)
                    self.query.addParameter("technique",techniqueName)
                    order = True
                    self.query.setSortBy("fcstHour", order)
                    bsResults = self.query.execute()
                if bsResults is None or bsResults.size() == 0:
                    pass
                else:                    
                    # create new AtcfTrack object
                    aTrack = AtcfTrack(techniqueName, bsResults)
                    
                    # add AtcfTrack object to AtcfCyclone
                    aCyclone.addTrack(aTrack)
                # add AtcfCyclone object to the response ArrayList
                response.add(ResponseMessageGeneric(aCyclone))
        return response

#
# Returns a string with a response message
#
    def __makeNullResponse(self, aMessage="Database Query returned no results"):
        return ResponseMessageGeneric(aMessage)