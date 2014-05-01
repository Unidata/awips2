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
from com.raytheon.edex.plugin.ldadprofiler.common import ProfilerLdadLevel

typeMap = { 'd' : 'DOUBLE', 'f' : 'FLOAT', 
            'i' : 'INT', 'c' : 'CHAR',
            's' : 'STRING', 'h' : 'INT' }

excludeVars = ('time', 'validTimeList', 'nStaticIds', 'inventory', 'globalInventory', 'firstInBin', 'lastInBin', 'lastRecord', 'staticIds','prevRecord', 'firstOverflow', 'isOverflow')

class LdadprofilerDecoder():

    def __init__(self, text=None, filePath=None):
        
        #self._parameters = ['height','wdir_tru','wspd_k','v','vconf','u','uconf','w','wconf']
        #self._mappings = ['levelHeight','windDir', 'windSpeed','vcWind','ucWind','wcWind']

        
        #to ensure time calls are based on Zulu
        os.environ['TZ'] = "GMT0"
        
        self._deleteAfterProcessing = 0
        if filePath is None:
            self._incomingFilename = None
        else:
            self._incomingFilename = filePath

    def decode(self):
        LogStream.logEvent("<<<<<<<<<<<<<<<<<<<<<<<<<<<<LdadProfilerDecoder is working>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
        fillValue = None 
        missing = None
        reportType = 3000
        ldadprofilerReports = list()
        net = pupynere.netcdf_file(self._incomingFilename,"r")
        records = net._recs
        location = SurfaceObsLocation()
        lat = Double.valueOf(str(net.variables['lat'].getValue()))
        lon = Double.valueOf(str(net.variables['lon'].getValue()))
        elv = Double.valueOf(str(net.variables['alt'].getValue()))
        location.assignLocation(lat.doubleValue(), lon.doubleValue());
        location.setElevation(Integer(elv.intValue()));
        location.setStationId(net._attributes['site_acronym'])
        for x in range(records):
            reportTime = net.variables['start_time_offset'][x] + net.variables['base_time'].getValue()
            #obsPeriod = net.variables['end_time_offset'][x]-net.variables['start_time_offset'][x]
            ldadprofilerReport = dict()
            levels = list()
            for y in range(net.dimensions['gate']):
                level = ProfilerLdadLevel()
                height = Double.valueOf(str(net.variables['height'][x][y]))
                level.setLevelHeight(Integer(height.intValue()))
                level.setWindSpeed(Double.valueOf(str(net.variables['wspd_k'][x][y])))
                level.setWindDir(Double.valueOf(str(net.variables['wdir_tru'][x][y])))
                level.setUcWind(Double.valueOf(str(net.variables['u'][x][y])))
                level.setVcWind(Double.valueOf(str(net.variables['v'][x][y])))
                level.setWcWind(Double.valueOf(str(net.variables['w'][x][y])))
                level.setUconf(Double.valueOf(str(net.variables['uconf'][x][y])))
                level.setVconf(Double.valueOf(str(net.variables['vconf'][x][y])))
                level.setWconf(Double.valueOf(str(net.variables['wconf'][x][y])))
                levels.append(level)     
            ldadprofilerReport['location'] = location
            ldadprofilerReport['base_time'] = int(net.variables['base_time'].getValue())
            ldadprofilerReport['start_time_offset']= Double.valueOf(str(net.variables['start_time_offset'][x]))
            ldadprofilerReport['end_time_offset']= Double.valueOf(str(net.variables['end_time_offset'][x]))
            ldadprofilerReport['timeObs'] = long(reportTime)
            ldadprofilerReport['dataTime'] = ldadprofilerReport['timeObs']
            ldadprofilerReport['nhts']= int(net.variables['nhts'][x])
            ldadprofilerReport['reportType']=reportType
            ldadprofilerReport['stationName']=net._attributes['sensor_site']
            ldadprofilerReport['levels'] = levels 
            ldadprofilerReports.append(ldadprofilerReport)
        net.close()
        return ldadprofilerReports


    def _usage(self):
        #Prints out usage information if started without sufficient command
        #line arguments.
        s =  "This tool is not meant to be run from the command line."
        print s
        LogStream.logProblem(s)



def main():
    try:
        LogStream.logEvent("LdadProfilerDecoder Starting")        
        decoder = LdadprofilerDecoder()
        decoder.decode()
        decoder = None
        LogStream.logEvent("LdadProfilerDecoder Finished")
    except:
        LogStream.logProblem("Caught Exception: ", LogStream.exc())
        sys.exit(1)

if __name__ == "__main__":
    main()
    sys.exit(0)