# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Run_NWPS
# Description:
# This runs a Procedure within the GFE that builds
# Swan Fcst Wind grids based on the Operational Wind Fcst grids
# and then sends those Wind grids to the NWPS model.
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear. Possible items are: Populate, Edit, Consistency,
# Verify, Hazards
MenuItems = ["Edit"]
import LogStream, time
from math import *

VariableList = [
               ("How Long Do You Want To Run NWPS:" , 102, "scale", [12, 102], 3),
               ("NOTE:  Remember to remove all GFE wind grids up to the 00Z, 03Z, 06Z, 09Z, 12Z, 15Z, 18Z, or 21Z hour prior to the current time", "", "label"),
               ("  then save before running NWPS. However, if the previous NWPS run is old (> 24hrs),", "", "label"),
               ("up to 12 hours of winds BEFORE the current time must be accounted for (using LAPS,RTMA, etc.).", "", "label"),
               ("NWPS Model Winds:", "ForecastWindGrids", "radio", ["GFS40","NAM12","ForecastWindGrids"]),
               ("***Model Run Times***", "", "label"),
               ("NAM12 = 84 hours","", "label",),
               ("","", "label",),
               #("**Boundary Conditions: TAFB-NWPS:   CHECK www.srh.noaa.gov/rtimages/nhc/wfo_boundary_conditions for up to date files for your SITE**","", "label"),
               #("*NOTE: make sure there is a file time stamp online matches your first wind grid for your site","", "lable"),

               #("TAFB-NWPS boundary conditions can be used year round, but mainly for during tropical events","", "lable"),
               #("For TAFB-NWPS: check here for the timestamp that matches your first wind grid in GFE","", "lable"),

               ("Model Core:", "SWAN", "radio", ["SWAN","NWW","UNSWAN"]),
               ("Send Output to Web:", "Yes", "radio", ["Yes","No"]),
               ("Plot Output Only (No Web):", "No", "radio", ["Yes","No"]),
              # ("Initialize boundaries with:", [""], "radio", ["WNAWave", "HURWave", "No"]),
              # ("Initialize boundaries with:", "WNAWave", "radio", ["WNAWave", "TAFB-NWPS","HURWave", "No"]),
               ("Initialize boundaries with:", "WNAWave", "radio", ["WNAWave", "TAFB-NWPS", "HURWave", "No"]),
               ("Run Hi Res NEST?", "Yes", "radio", ["Yes","No"]),
               ("RTOFS Currents", "Yes", "radio", ["Yes","No"]),
               ("Int. Time Step?", "600", "radio", ["3600","1800","1200","900","600"]),
               ("Hotstart?", "True", "radio", ["True", "False"]),
               ("WATERLEVELS?", "ESTOFS", "radio", ["ESTOFS","PSURGE", "No"]),
               ("IF PSURGE\n% Exceedance Hgt?", "10", "radio", ["10", "20", "30", "40", "50"]),

]

import time
import AbsTime
import SmartScript
import os


class Procedure (SmartScript.SmartScript):
        def __init__(self, dbss):
                SmartScript.SmartScript.__init__(self, dbss)

        def execute(self, editArea, timeRange, varDict):
                fcst_length = varDict["How Long Do You Want To Run NWPS:"]
                wind = varDict["NWPS Model Winds:"]
                model = varDict["Model Core:"]
                web = varDict["Send Output to Web:"]
                plot = varDict["Plot Output Only (No Web):"]
                wna = varDict["Initialize boundaries with:"]
                nests = varDict["Run Hi Res NEST?"]
                stream = varDict["RTOFS Currents"]
                tstep = varDict["Int. Time Step?"]
                hotstart = varDict["Hotstart?"]
                waterlevels = varDict["WATERLEVELS?"]
                excd = varDict["IF PSURGE\n% Exceedance Hgt?"]            
                fcstlength = str(fcst_length)
                wind = str(wind)
                wna = str(wna)
                nest = str(nests)
                gstream = str(stream)
                waterlevels = str(waterlevels)
                excd = str(excd)
                os.system('echo '+fcstlength+' > /data/local/SWAN/tmp/runlen')
                os.system('chmod 666 /data/local/SWAN/tmp/runlen')
                os.system('echo '+wind+' > /data/local/SWAN/tmp/runwind')
                os.system('chmod 666 /data/local/SWAN/tmp/runwind')
                os.system('echo '+model+' > /data/local/SWAN/tmp/runcore')
                os.system('chmod 666 /data/local/SWAN/tmp/runcore')
                os.system('echo '+web+' > /data/local/SWAN/tmp/runweb')
                os.system('chmod 666 /data/local/SWAN/tmp/runweb')
                os.system('echo '+plot+' > /data/local/SWAN/tmp/runplot')
                os.system('chmod 666 /data/local/SWAN/tmp/runplot')
                os.system('echo '+wna+' > /data/local/SWAN/tmp/runwna')
                os.system('chmod 666 /data/local/SWAN/tmp/runwna')
                os.system('echo '+nest+' > /data/local/SWAN/tmp/runnests')
                os.system('chmod 666 /data/local/SWAN/tmp/runnests')
                os.system('echo '+gstream+' > /data/local/SWAN/tmp/rungstream')
                os.system('chmod 666 /data/local/SWAN/tmp/rungstream')
                os.system('echo '+tstep+' > /data/local/SWAN/tmp/runtstep')
                os.system('chmod 666 /data/local/SWAN/tmp/runtstep')
                os.system('echo '+hotstart+' > /data/local/SWAN/tmp/hotstart')
                os.system('chmod 666 /data/local/SWAN/tmp/hotstart')
                os.system('echo '+waterlevels+' > /data/local/SWAN/tmp/waterlevels')
                os.system('chmod 666 /data/local/SWAN/tmp/waterlevels')
                os.system('echo '+excd+' > /data/local/SWAN/tmp/excd')
                os.system('chmod 666 /data/local/SWAN/tmp/excd')                   
                os.system('touch /data/local/SWAN/tmp/run_swan.proc')
                os.system('chmod 666 /data/local/SWAN/tmp/run_swan.proc')
                self.statusBarMsg("WIND HAS BEEN SENT TO NWPS", "S")
