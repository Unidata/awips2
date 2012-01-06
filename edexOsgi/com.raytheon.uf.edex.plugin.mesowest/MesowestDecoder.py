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
import sys, os, time, re, string, getopt
import copy
import LogStream
import pupynere
from com.raytheon.uf.common.pointdata.spatial import SurfaceObsLocation
from java.lang import Double
from java.lang import Integer

class MesowestDecoder():

    def __init__(self, text=None, filePath=None):
        
        self._parameters = ['altimeter', 'dewpoint', 'relHumidity', 'stationId', 'rawMessage', 'temperature', 'windDir', 'windGust', 'windSpeed', 'precipAccum']
        self._mappings = ['altimeter', 'dwpt', 'humidity', 'stationId', 'obsText', 'temp', 'windDirection', 'windGust', 'windSpeed', 'precip']

        
        #to ensure time calls are based on Zulu
        os.environ['TZ'] = "GMT0"
        
        self._deleteAfterProcessing = 0
        if filePath is None:
            self._incomingFilename = None
        else:
            self._incomingFilename = filePath

    def decode(self):
        fillValue = None 
        missing = None
        mesowestReports = list()
        obsList = dict()
        net = pupynere.netcdf_file(self._incomingFilename,"r")
        records = net._recs
        for x in range(records):
            stationId = ''.join(net.variables['stationId'][x])
            reportTime = net.variables['reportTime'][x] * 1000
            if obsList.has_key(reportTime):
                if stationId in obsList[reportTime]:
                    continue
                else:
                    obsList[reportTime].append(stationId)
            else:
                obsList[reportTime] = list()
                obsList[reportTime].append(stationId)
            mesowestReport = dict()
            for paramNum in range(len(self._parameters)):
                param = self._parameters[paramNum]
                mapping = self._mappings[paramNum]
                variable = net.variables[param]
                # Set the missing value if available
                try:
                    missing = variable.missing_value
                except AttributeError:
                    missing = None
                # Set the fill value if available
                try:
                    fillValue = variable._FillValue
                except AttributeError:
                    fillValue = None
                if len(variable.shape) == 1:
                    if fillValue != None and variable[x] == fillValue:
                        mesowestReport[mapping] = Double(-9999.0)
                        continue
                    if missing != None:
                        #  Odd case where certain values are -764 in the netcdf
                        if variable[x] == missing or variable[x] < -600:
                            mesowestReport[mapping] = Double(-9999.0)
                            continue
                    mesowestReport[mapping] = Double.valueOf(str(variable[x]))
                elif len(variable.shape) > 1 and variable.shape[1] > 3:
                    mesowestReport[mapping] = ''.join(variable[x])
                    
            location = SurfaceObsLocation()
            lat = Double.valueOf(str(net.variables['latitude'][x]))
            lon = Double.valueOf(str(net.variables['longitude'][x]))
            elv = Double.valueOf(str(net.variables['elevation'][x]))
            location.assignLocation(lat.doubleValue(),lon.doubleValue());
            location.setElevation(Integer(elv.intValue()));
            location.setStationId(stationId)
            mesowestReport['location'] = location
            mesowestReport['timeObs'] = long(reportTime)
            mesowestReport['dataTime'] = mesowestReport['timeObs']
            mesowestReport['networkType'] = ''.join(net.variables['stationType'][x])
            mesowestReports.append(mesowestReport)
        net.close()
        return mesowestReports


    def _usage(self):
        #Prints out usage information if started without sufficient command
        #line arguments.
        s =  "This tool is not meant to be run from the command line."
        print s
        LogStream.logProblem(s)



def main():
    try:
        LogStream.logEvent("MesowestDecoder Starting")        
        decoder = MesowestDecoder()
        decoder.decode()
        decoder = None
        LogStream.logEvent("MesowestDecoder Finished")
    except:
        LogStream.logProblem("Caught Exception: ", LogStream.exc())
        sys.exit(1)

if __name__ == "__main__":
    main()
    sys.exit(0)
