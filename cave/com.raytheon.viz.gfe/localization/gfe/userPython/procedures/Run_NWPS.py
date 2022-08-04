# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Run_NWPS
# Description: AWIPS 2 Version 18.1.1
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
# Last modified: 04/14/16 by T. LeFebvre/P. Santos, for wind inventory check.
# Last modified: 07/18/2016 by J. Maloney/P. Santos, post 16.4.1 code review.
# Last modified: 12/10/2017 by P. Stanko, remove archaic unused options and default RTOFS on in gulf stream, off elsewhere.
# Last modified: 12/10/2017 by P. Stanko, also added call to new baseline script for NWPS warning messages from WCOSS.
# Last modified: 12/12/2017 by P. Stanko, Differentiate between structured and unstructured sites, TAFB and non-TAFB, always try hotstart
# Last modified: 03/16/2020 by J. Maloney, modified /tmp folder creation b/c of IdM issues DCS21768
# ----------------------------------------------------------------------------
#
# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear. Possible items are: Populate, Edit, Consistency,
# Verify, Hazards
##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

MenuItems = ["Edit", "Populate"]
import SmartScript
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
            currentTime = (self._gmtime().unixTime() // 3600) * 3600

            if time.gmtime(currentTime).tm_hour % 6 != 0:
                currentTime = currentTime + (6 * 3600)  # add six hours

            startTime = int(currentTime / (6 * 3600)) * (6 * 3600)
            #print "StartTime from GUI is: ", startTime

            timeStrs = []
            timeList = []
            
            for i in range(0, 6):
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

        def getWEInventory(self, WEName, dbase="Fcst", level="SFC", 
                              timeRange=TimeRange.allTimes()):
            """Return a list of time ranges with available data for a field from
            a specific database and level.
            Args:
                string WEName: name of field to inventory
                string dbase: name of database to search (default = 'Fcst')
                string level: level of data to inventory (default = 'SFC')
            Returns:
                Python list of Python time range objects
            """
    
    #        print "Getting inventory of -> '%s' from '%s' at '%s'" % \
    #        (WEName, dbase, level)
    
            trList = []
            # getGridInfo will just die if the modelName or weName is not valid
            #  so wrap it in a try block and return [] if it fails
            try:
                gridInfo = self.getGridInfo(dbase, WEName, level, timeRange)
                trList = [g.gridTime() for g in gridInfo]
            except:
                self.statusBarMsg("Problems retrieving wind grids", "S")
               
            return trList
        
        def execute(self, varDict):

                gulfStreamSites=['KEY', 'MFL', 'MLB', 'JAX', 'CHS', 'ILM', 'MHX', 'AKQ', 'PHI', 'OKX', 'BOX', 'GYX', 'CAR']
                tafbSites=['BRO', 'CRP', 'HGX', 'LCH', 'LIX', 'MOB', 'TAE', 'TBW', 'MFL', 'KEY', 'MLB', 'JAX', 'CHS', 'ILM']
                buttonList, timeList = self.getButtonNames()
                GFEDomainname = self.getSiteID()   
                print("GFEDomain is: ", GFEDomainname)
                cron = True
        
                if varDict is None:  # This means the tool is being run interactively, so make the GUI.
                
                    variableList = [
                                    ("How Long Do You Want To Run NWPS:" , 144, "scale", [12, 144], 3),
                                    ("**NOTE: NCEP WCOSS Runs Always Go Out 144 Hours Regardless of Your Choice Here","", "label"),
                                    ("Model Start Time:", buttonList[4], "radio", buttonList),
                                    ("Local, NCEP, or Both:", "NCEP", "radio", ["Local","NCEP","Both"]),
                                    ("Waterlevels:", "ESTOFS", "radio", ["ESTOFS","PSURGE", "No"]),
                                    ("If PSURGE\n% Exceedance Hgt:", "10", "radio", ["10", "20", "30", "40", "50"]),
                    ]

                    if GFEDomainname in gulfStreamSites:
                        variableList.append(("RTOFS Currents:", "Yes", "radio", ["Yes","No"]))
                    else:
                        variableList.append(("RTOFS Currents:", "No", "radio", ["Yes","No"]))                

                    nest="Yes"

                    if GFEDomainname in tafbSites:
                        variableList.append(("Boundary Conditions:", "WAVEWATCH", "radio", ["WAVEWATCH", "TAFB-NWPS", "HURWave", "No"]))
                    else:
                        variableList.append(("Boundary Conditions:", "WAVEWATCH", "radio", ["WAVEWATCH", "HURWave", "No"]))
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
                    model = "SWAN"
                    web="Yes"
                    plot="Yes"
                    wna = processVarList.varDict()["Boundary Conditions:"]
                    gstream = processVarList.varDict()["RTOFS Currents:"]
                    tstep="600"
                    hotstart="True"
                    waterlevels = processVarList.varDict()["Waterlevels:"]
                    excd = processVarList.varDict()["If PSURGE\n% Exceedance Hgt:"]
                    cron = False
                    # label it WAVEWATCH in the GUI, but continue to call it WNAWave for WCOSS.
                    if wna == "WAVEWATCH":
                        wna="WNAWave"
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
                
                trList = self.getWEInventory("NWPSwind")
                if len(trList) < 144:
                    self.statusBarMsg("Not enough Wind grids. You need at least 144 hours.", "S")
                    return
                   
                inp_args = fcstlength + ":" + wna + ":" + nest + ":" + gstream + ":" + wind + ":" + web + ":" + plot + ":" + tstep + ":" + hotstart + ":" + waterlevels + ":" + model + ":" + excd + ":" + wheretorun
                                    
                try:
                    os.stat('/tmp/nwps'+GFEDomainname)
                except:
                    os.makedirs('/tmp/nwps'+GFEDomainname)                
                os.chmod('/tmp/nwps'+GFEDomainname,0o775) 
         
                with open('/tmp/nwps'+GFEDomainname+'/inp_args', 'w') as f:
                    f.write(inp_args) 
                os.chmod('/tmp/nwps'+GFEDomainname+'/inp_args',0o666)
                                  
                os.system('ssh -q pv2 mkdir -p /awips2/GFESuite/nwps/'+GFEDomainname+'_var')
                os.system('ssh -q pv2 chmod 775 /awips2/GFESuite/nwps/'+GFEDomainname+'_var')              
                os.system('scp -rpq /tmp/nwps'+GFEDomainname+'/inp_args pv2:/awips2/GFESuite/nwps/'+GFEDomainname+'_var/')   
                if cron:
                    os.system('ssh -q pv2 /awips2/GFESuite/nwps/bin/runManualNWPS_OutsideAWIPS.sh '+GFEDomainname)                   
                else:
                    os.system('nohup xterm -iconic -e ssh -q pv2 /awips2/GFESuite/nwps/bin/runManualNWPS_OutsideAWIPS.sh '+GFEDomainname+' &')
                    #ORIG#os.system('xterm -e ssh -q pv2 /awips2/GFESuite/nwps/bin/runManualNWPS_OutsideAWIPS.sh '+GFEDomainname)
                shutil.rmtree('/tmp/nwps'+GFEDomainname)    

# If you set up the ldad NWPS WCOSS Notification scripts from Paul Stanko, uncomment this line. Consult the 18.1.1 post install notes.  
#                 os.system('ssh ldad@ls1 /ldad/bin/NWPSmessage/NWPSmessage.sh '+modelstarttime+' '+GFEDomainname+' &') # attempt to call NWPSmessage script on LDAD
