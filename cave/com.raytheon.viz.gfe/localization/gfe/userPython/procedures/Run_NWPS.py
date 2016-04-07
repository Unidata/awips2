# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Run_NWPS
# Description: AWIPS 2 Version 16.2.1
#
# This runs a Procedure within the GFE that builds NWPS 
# forecast wind grids based on the operational wind forecast grids
# and then sends those Wind grids to the NWPS model.
#
# Authors: Pablo Santos, Alex Gibbs, and Joe Maloney.
#
# Last Modified: 01/23/15 by AG/PS for AWIPS Baseline.
# Last Modified: 09/10/15 by Joe Maloney/PS for mulitsite version of NWPS.
# Last Modified: 10/14/15 by Joe Maloney/PS to remove dependancy on cron. Totally on demand by user. 
# This means baseline cron entry for nwps will be removed with 16.2.1
# Last Modified: 10/30/15 by Joe Maloney, added -q flags to scp/ssh at end.
# Last Modified: 11/25/15 by Tom LeFebvre, added switch to run tool from a cron or interactively.
# Last Modified: 11/29/15 by P. Santos, completed adding code to enable running Run_NWPS interactively or from a cron.
# Last modified: 03/18/16 by Joe Maloney, a minor tweak to runManualNWPS_OutsideAWIPS call.
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
            
            for i in range(0, 8):
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
                GFEDomainname = self.getSiteID()   
                print "GFEDomain is: ", GFEDomainname
                cron = True
        
                if varDict is None:  # This means the tool is being run interactively, so make the GUI.
                
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
                    fcstlength = str(fcst_length)              
                    wind="ForecastWindGrids"
                    modelstarttime = processVarList.varDict()["Model Start Time:"]
                    wheretorun = processVarList.varDict()["Local, NCEP, or Both:"]
                    model = processVarList.varDict()["Model Core:"]
                    web = processVarList.varDict()["Send Output to Web:"]
                    plot = processVarList.varDict()["Plot Output Only (No Web):"]
                    wna = processVarList.varDict()["Boundary Conditions:"]
                    nest = processVarList.varDict()["Run Hi Res NEST:"]
                    gstream = processVarList.varDict()["RTOFS Currents:"]
                    tstep = processVarList.varDict()["Model Time Step:"]
                    hotstart = processVarList.varDict()["Hotstart:"]
                    waterlevels = processVarList.varDict()["Waterlevels:"]
                    excd = processVarList.varDict()["If PSURGE\n% Exceedance Hgt:"]
                    cron = False
                    # end interactive GUI portion
                
                else: 

# This part of if else statement assumes procedure is being run from command 
#line with variable list passed on using the -V option to runProcedure. This 
#allows to run procedure from a cron. Example default for runProcedure would be:
# All variables shown below passed with -V option are required for procedure to run properly.
# /awips2/GFESuite/bin/runProcedure -n Run_NWPS -c gfeConfig 
# -V '{"fcstlength":"102","wind":"ForecastWindGrids","wheretorun":"NCEP","model":"SWAN","web":"Yes","plot":"Yes","wna":"WNAWave","nest":"Yes","gstream":"Yes","tstep":"600","hotstart":"True","waterlevels":"ESTOFS","excd":"10"}'
# If running from a cron, you do not need to create a SITE level override of this baseline procedure if your input variables 
# are different because you pass that on from the command line.
                    
                    modelstarttime = buttonList[4]
                    fcstlength = varDict['fcstlength']
                    wind = varDict['wind']
                    wheretorun = varDict['wheretorun']
                    model = varDict['model']
                    web = varDict['web']
                    plot = varDict['plot']
                    wna = varDict['wna']
                    nest = varDict['nest']
                    gstream = varDict['gstream']
                    tstep = varDict['tstep']
                    hotstart = varDict['hotstart']
                    waterlevels = varDict['waterlevels']   
                    excd = varDict['excd']   
                                                                                                                                    
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
                timeRange2 = TimeRange.TimeRange(AbsTime.AbsTime(starttime), AbsTime.AbsTime(starttime + 8*24*3600))             
                                  
                self.deleteCmd(['NWPSwind'], timeRange1)
                databaseID = self.findDatabase("Fcst")
                self.copyToCmd([('Wind', 'NWPSwind')], databaseID, timeRange2) 
                self.fragmentCmd(['NWPSwind'], timeRange2)                
                self.saveElements(["NWPSwind"])
                   
                inp_args = fcstlength + ":" + wna + ":" + nest + ":" + gstream + ":" + wind + ":" + web + ":" + plot + ":" + tstep + ":" + hotstart + ":" + waterlevels + ":" + model + ":" + excd + ":" + wheretorun
                                   
                try:
                    os.stat('/tmp/nwps/'+GFEDomainname)
                except:
                    os.makedirs('/tmp/nwps/'+GFEDomainname)                
                os.chmod('/tmp/nwps/'+GFEDomainname,0o775) 
     
                with open('/tmp/nwps/'+GFEDomainname+'/inp_args', 'w') as f:
                    f.write(inp_args) 
                os.chmod('/tmp/nwps/'+GFEDomainname+'/inp_args',0o666)
                              
                os.system('ssh -q px2f mkdir -p /awips2/GFESuite/nwps/'+GFEDomainname+'_var')
                os.system('ssh -q px2f chmod 775 /awips2/GFESuite/nwps/'+GFEDomainname+'_var')              
                os.system('scp -rpq /tmp/nwps/'+GFEDomainname+'/inp_args px2f:/awips2/GFESuite/nwps/'+GFEDomainname+'_var/')   
                if cron:
                    os.system('ssh -q px2f /awips2/GFESuite/nwps/bin/runManualNWPS_OutsideAWIPS.sh '+GFEDomainname)                   
                else:
                    os.system('nohup xterm -iconic -e ssh -q px2f /awips2/GFESuite/nwps/bin/runManualNWPS_OutsideAWIPS.sh '+GFEDomainname+' &')
                shutil.rmtree('/tmp/nwps/'+GFEDomainname)                
