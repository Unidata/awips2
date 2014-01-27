#
# NcModelAvailableTimesQuery
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Scans dataLocation directory and returns all the available cycles.
#
#     Usage:
#    import NcModelAvailableTimesQuery
#    query = NcModelAvailableTimesQuery.NcModelAvailableTimesQuery()
#    query.setDataLocation("$MODEL/gfs")
#    query.setCurrentCycle("2010-11-28 18:00:00.0")
#    return query.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    10/22/10        ??            mgamazaychikov       Initial Creation
#
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert
import os
import re


class NcModelAvailableTimesQuery():
    
    def __init__(self):
        self.convert = GempakConvert()
    
#
# Set parameters of the metadata catalog query
#
    def setDataLocation(self, aDataLocation):
        dirLevels = aDataLocation.split("/")
        numberDirLevel = len(dirLevels)
        if aDataLocation.find("$") == 0:
            envName = dirLevels[0].split("$")[1]
            self.location = os.environ [envName]
            for ii in range (1,numberDirLevel -1):
                self.location = self.location + "/" + dirLevels[ii]
            self.model = dirLevels[numberDirLevel-1].lower()
        else:
            self.location = "/".join(dirLevels[0:numberDirLevel-1])
            self.model = dirLevels[numberDirLevel-1].lower()
#
# Set parameters of the metadata catalog query
#
    def setCurrentCycle(self, aTime):
        fileDate = aTime.split(' ')[0]
        fileTime = aTime.split(' ')[1]
        self.fileTmplt = ''.join(fileDate.split('-')[0:3]) + \
                         ''.join(fileTime.split(':')[0]) + 'f'
    
#
# Executes the query and calls appropriate response functions
#    
    def execute(self):
        dataDir = self.location + "/" + self.model
        allFiles = os.listdir(dataDir)
        srchPattern =  self.model + '_' + self.fileTmplt + '*'
        test = re.compile(srchPattern, re.IGNORECASE)
        availTimes = [f[f.find('_')+1:] for f in allFiles if test.search(f)]
        availTimes.sort()
        retAvailTimes = [self.__fileNameTimeToDbtime(at) for at in availTimes]       
        if retAvailTimes:
            return self.__makeResponse(retAvailTimes)
        else:
            return self.__makeNullResponse("No times available")

#
# Converts YYYYMMDDHHfHHH file name time to DataTime format YYYY-MM-DD HH:MM:SS.S (H)
#
    def __fileNameTimeToDbtime (self, aFileNameTime):
        aList = aFileNameTime.upper().split("F")        
        return (aList[0][:4] + '-' + aList[0][4:6] + 
                '-' + aList[0][6:8] + ' ' + aList[0][8:10] + 
                ':00:00.0 (' + str(int(aList[1])) + ')')

#
# Generates a list of responses in XML format
#
    def __makeResponse(self, aTimes):
        retStr = "|".join(aTimes)
        return ResponseMessageGeneric(retStr)
    
#
# Returns a string with a response message
#
    def __makeNullResponse(self, aMessage="Query returned no results"):
        return ResponseMessageGeneric(aMessage)