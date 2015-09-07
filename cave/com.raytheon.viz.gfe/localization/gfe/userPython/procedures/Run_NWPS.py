# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Run_NWPS
# Description:
#
# This runs a Procedure within the GFE that builds NWPS 
# forecast wind grids based on the operational wind forecast grids
# and then sends those Wind grids to the NWPS model.
#
# Authors: Pablo Santos and Alex Gibbs.
#
# Last Modified: 01/23/15 by AG/PS for AWIPS Baseline.
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear. Possible items are: Populate, Edit, Consistency,
# Verify, Hazards

MenuItems = ["Edit"]
import SmartScript, LogStream
import time, os, shutil, TimeRange, AbsTime
import ProcessVariableList

class Procedure (SmartScript.SmartScript):
        def __init__(self, dbss):
            SmartScript.SmartScript.__init__(self, dbss)

        def fileNameFromIntTime(self, floatTime):
            tupleTime = time.gmtime(floatTime)
            # print "TUPLETIME IS: ", tupleTime
            return time.strftime("%Y%m%d_%H00", tupleTime)
   
        def getButtonNames(self):
            
            #currentTime = int(time.time() / 3600) * 3600 # truncated to this hour
            currentTime = (self._gmtime().unixTime()/3600)*3600
            #print "currentTime: ", currentTime

            if time.gmtime(currentTime).tm_hour % 6 != 0:
                currentTime = currentTime + (6 * 3600)  # add three hours

            startTime = int(currentTime / (6 * 3600)) * (6 * 3600)
            #print "StartTime from GUI is: ", startTime

            timeStrs = []
            timeList = []
            
            for i in range(0, 7):
                currentTime = startTime + (6 * i) * 3600 - 108000 # 30 hrs    
                strTime = self.fileNameFromIntTime(currentTime)
                timeList.append(currentTime)
                timeStrs.append(strTime)

            return timeStrs,timeList

        def getModelTimeRange(self, modelID, param):
            #before = time.time() - (3000 * 24 * 3600) # 3000 days ago. Does not work with DRT
            #later = time.time() + 100 * 24 * 3600  # 100 days from now. Does not work with DRT
            before = self._gmtime().unixTime() - (7 * 24 * 3600) # 7 days ago
            later = self._gmtime().unixTime() + 8 * 24 * 3600  # 8 days from now
            timeRange = TimeRange.TimeRange(AbsTime.AbsTime(before), AbsTime.AbsTime(later))
            #self.deleteCmd(weNames, timeRange)
            gridInfo = self.getGridInfo(modelID, param, "SFC", timeRange)
            #print "GRIDINFO IS: ", modelID, gridInfo
            if len(gridInfo) == 0:
                self.statusBarMsg("No grids available for model:" + modelID, "S")
                return None

            minTime = later
            maxTime = before
            for g in gridInfo:
                start = g.gridTime().startTime().unixTime()
                end = g.gridTime().endTime().unixTime()
                minTime = min(minTime,start)
                maxTime = max(maxTime,end)

            modelTR = TimeRange.TimeRange(AbsTime.AbsTime(minTime), AbsTime.AbsTime(maxTime))
            #print "MODELTR", modelTR, minTime, maxTime
            return modelTR, minTime, maxTime

        def execute(self, editArea, timeRange, varDict):
                
                buttonList, timeList = self.getButtonNames()

                variableList = [
                                ("How Long Do You Want To Run NWPS:" , 102, "scale", [12, 102], 3),
                                #("NWPS Model Winds:", "ForecastWindGrids", "radio", ["ForecastWindGrids"]),
                                ("Model Start Time:", buttonList[4], "radio", buttonList),
                                ("Local, NCEP, or Both:", "Both", "radio", ["Local","NCEP","Both"]),
                                ("Model Core:", "SWAN", "radio", ["SWAN","NWW","UNSWAN"]),
                                ("Send Output to Web:", "Yes", "radio", ["Yes","No"]),
                                ("Plot Output Only (No Web):", "No", "radio", ["Yes","No"]),
                                ("Boundary Conditions:", "WNAWave", "radio", ["WNAWave", "TAFB-NWPS", "HURWave", "No"]),
                                ("**Boundary Conditions: OPC/TAFB-NWPS:   CHECK www.srh.noaa.gov/rtimages/nhc/wfo_boundary_conditions for up to date files for your SITE**\nNOTE: make sure there is a file time stamp online matching your selected Model Start Time","", "label"),
                                ("Run Hi Res NEST:", "Yes", "radio", ["Yes","No"]),
                                ("RTOFS Currents:", "Yes", "radio", ["Yes","No"]),
                                ("Model Time Step:", "600", "radio", ["1200","900","600","300"]),
                                ("Hotstart:", "True", "radio", ["True", "False"]),
                                ("Waterlevels:", "ESTOFS", "radio", ["ESTOFS","PSURGE", "No"]),
                                ("If PSURGE\n% Exceedance Hgt:", "10", "radio", ["10", "20", "30", "40", "50"]),
                ]
                
                varDict = {}
                processVarList = ProcessVariableList.ProcessVariableList("Run_NWPS", variableList, varDict, None)
                status = processVarList.status()
                if status != "OK":
                    return
                
                fcst_length = processVarList.varDict()["How Long Do You Want To Run NWPS:"]                
                wind="ForecastWindGrids"
                modelstarttime = processVarList.varDict()["Model Start Time:"]
                wheretorun = processVarList.varDict()["Local, NCEP, or Both:"]
                model = processVarList.varDict()["Model Core:"]
                web = processVarList.varDict()["Send Output to Web:"]
                plot = processVarList.varDict()["Plot Output Only (No Web):"]
                wna = processVarList.varDict()["Boundary Conditions:"]
                nests = processVarList.varDict()["Run Hi Res NEST:"]
                stream = processVarList.varDict()["RTOFS Currents:"]
                tstep = processVarList.varDict()["Model Time Step:"]
                hotstart = processVarList.varDict()["Hotstart:"]
                waterlevels = processVarList.varDict()["Waterlevels:"]
                excd = processVarList.varDict()["If PSURGE\n% Exceedance Hgt:"]
                            
                fcstlength = str(fcst_length)
                wind = str(wind)
                wna = str(wna)
                nest = str(nests)
                gstream = str(stream)
                waterlevels = str(waterlevels)
                excd = str(excd)
                 
                modelTR = self.getModelTimeRange("Fcst", "Wind")
                startHour = modelTR[1]
                endHour = modelTR[2]
                timeRange = modelTR[0]
  
                if (modelstarttime == buttonList[0]):
                    starttime=timeList[0]  
                elif (modelstarttime == buttonList[1]):
                    starttime=timeList[1]    
                elif (modelstarttime == buttonList[2]):
                    starttime=timeList[2]
                elif (modelstarttime == buttonList[3]):
                    starttime=timeList[3]
                elif (modelstarttime == buttonList[4]):
                    starttime=timeList[4]
                elif (modelstarttime == buttonList[5]):
                    starttime=timeList[5]
                elif (modelstarttime == buttonList[6]):
                    starttime=timeList[6]
                else:
                    starttime=startHour # Model start Hour if all others empty
                     
                if (startHour > starttime):
                    starttime = startHour
                     
                timeRange1 = TimeRange.TimeRange(AbsTime.AbsTime(starttime - 7*24*3600), AbsTime.AbsTime(starttime + 8*24*3600))
                timeRange2 = TimeRange.TimeRange(AbsTime.AbsTime(starttime), AbsTime.AbsTime(starttime + fcst_length*3600))               
                                
                self.deleteCmd(['NWPSwind'], timeRange1)
                databaseID = self.findDatabase("Fcst")
                self.copyToCmd([('Wind', 'NWPSwind')], databaseID, timeRange2) 
                self.fragmentCmd(['NWPSwind'], timeRange2)                
                self.saveElements(["NWPSwind"])
                 
                inp_args = fcstlength + ":" + wna + ":" + nest + ":" + gstream + ":" + wind + ":" + web + ":" + plot + ":" + tstep + ":" + hotstart + ":" + waterlevels + ":" + model + ":" + excd + ":" + wheretorun
                                 
                try:
                    os.stat('/tmp/nwps')
                except:
                    os.mkdir('/tmp/nwps')                
                os.chmod('/tmp/nwps',0o777) 
  
                f = open('/tmp/nwps/inp_args', 'w')
                f.write(inp_args)
                f.close()
                os.chmod('/tmp/nwps/inp_args',0o666)               
                             
                os.system('cp -r /tmp/nwps/inp_args /awips2/GFESuite/nwps/var/')                   
                shutil.rmtree('/tmp/nwps')
