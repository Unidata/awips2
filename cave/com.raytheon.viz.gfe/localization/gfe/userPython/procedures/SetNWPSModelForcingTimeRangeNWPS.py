# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SetSwanModelForcingTimeRangeNWPS
#
# Description:
# This Procedure puts the Operational GFE Fcst Wind grids into the
# SWAN GFE Fcst Wind grids over the operational Domain.
# As background over the larger SWAN Domain it puts in the latest available
# Eta12 (NAM12/WRF) grids in.
# This Procedure DOES NOT Extend the grids to the appropriate time for
# the SWAN run. That is assumed to be already done once per day.
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Populate"]

# CONFIGURATIN ITEMS

# For operatio/awips/GFESwan/bin/runProcedurenal GFE
DB="MFL_GRID__Fcst_00000000_0000"
#DB_Isc="MFL_GRID__ISC_00000000_0000"
SWANTIMESTEP="3"

# The following is a WRK PIL to be used to store a notification in the text database via ldad

WRKSWN="/data/local/NWPS/SUAWRKNWP.dat"

# END CONFIGURATION

import SmartScript
import time, os, TimeRange, AbsTime
import numpy.oldnumeric


class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def getModelTimeRange(self, modelID, param):
        #before = time.time() - (3000 * 24 * 3600) # 3000 days ago
        #later = time.time() + 100 * 24 * 3600  # 100 days from now
        before = time.time() - (7 * 24 * 3600) # 7 days ago
        later = time.time() + 8 * 24 * 3600  # 8 days from now
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
            if start < minTime:
                minTime = start
            if end > maxTime:
                maxTime = end

        modelTR = TimeRange.TimeRange(AbsTime.AbsTime(minTime), AbsTime.AbsTime(maxTime))
        #print "MODELTR", modelTR, minTime, maxTime
        return modelTR, minTime, maxTime

    def execute(self, editArea, timeRange, varDict):
        # Calls each Smart Tool: T_Tool, PoP_Tool, Wind_Tool

        gmTime = time.gmtime(time.time())
        current = AbsTime.absTimeYMD(gmTime[0],gmTime[1],gmTime[2], gmTime[3], 0, 0)
        current_gmHour = gmTime[3]
        start = time.time() - 48*3600
        end = time.time() + (24-current_gmHour + 204)*3600
        timeRange = TimeRange.TimeRange(AbsTime.AbsTime(start), AbsTime.AbsTime(end))


        fmodel = open('/data/local/NWPS/swan.winds', 'r')
        line = fmodel.readline()

        if not line:
            print "ERROR - Swan input wind model not set"
            print "Defaulting to ForecastWindGrids"
            windmodel = "ForecastWindGrids"
        else:
            windmodel = line.strip()
            print "Requested wind model is: ", windmodel

        flen = open('/data/local/NWPS/swan.runlen', 'r')
        line = flen.readline()

        if not line:
            print "ERROR - Swan run length not set"
            print "Defaulting to 84 hours"
            Model_End_Hour = 84
        else:
            #Model_End_Hour = int(line.strip())
            Model_End_Hour = 84
            print "Requested runlen is: ", Model_End_Hour

        #print "I AM HERE"
        #modelTR = self.getModelTimeRange("Official", "Wind")
        modelTR = self.getModelTimeRange("Fcst", "Wind")
        timeRange = modelTR[0]
        startHour = modelTR[1]
        endHour = modelTR[2]
        print "Fcst MODELTR: ", modelTR

        Model_Start_Time = AbsTime.AbsTime(startHour)
        Model_End_Time = AbsTime.AbsTime(startHour + Model_End_Hour*3600)

        if windmodel == "NAM12" or windmodel == "GFS40" or windmodel == "ECMWFHiRes":

            modelTRModel = self.getModelTimeRange(windmodel, "Wind")
            timeRangeModel = modelTRModel[0]
            startHourModel = modelTRModel[1]
            endHourModel = modelTRModel[2]
            print windmodel, " MODELTR: ", modelTRModel

            wind_model_end_hour = (startHourModel - startHour)/3600
            print "wind_model_end_hour", wind_model_end_hour

            if wind_model_end_hour > 0:
                wind_model_end_hour = Model_End_Hour - wind_model_end_hour
            else:
                wind_model_end_hour = Model_End_Hour
                startHour = startHourModel
                Model_Start_Time = AbsTime.AbsTime(startHourModel)
                Model_End_Time = AbsTime.AbsTime(startHourModel + Model_End_Hour*3600)
                timeRange = TimeRange.TimeRange(AbsTime.AbsTime(startHourModel), AbsTime.AbsTime(end))

            print "wind_model_end_hour is now: ", wind_model_end_hour
            Fcst_Model_End_Offset = startHourModel + wind_model_end_hour*3600 - endHourModel
            windmodel_Start_Time = AbsTime.AbsTime(startHourModel)
            windmodel_End_Time = AbsTime.AbsTime(startHourModel + wind_model_end_hour*3600)

            if Fcst_Model_End_Offset > 0:

                wind_model_end_hour = wind_model_end_hour - Fcst_Model_End_Offset/3600
                windmodel_End_Time = AbsTime.AbsTime(startHourModel + wind_model_end_hour*3600)
                Model_End_Hour = Model_End_Hour - Fcst_Model_End_Offset/3600
                Model_End_Time = AbsTime.AbsTime(startHour + Model_End_Hour*3600)
                frunlen = open('/data/local/NWPS/swan.runlen', 'w')
                mod = Model_End_Hour%int(SWANTIMESTEP)
                while mod != 0:
                    Model_End_Hour = Model_End_Hour - 1
                    mod = Model_End_Hour%int(SWANTIMESTEP)
                frunlen.write(str(Model_End_Hour))


            #print "windmodel_End_Time is: ", windmodel_End_Time
            fmessg = open(WRKSWN, 'w')
            fmessg.write('NWPS Running for the ' + str(Model_Start_Time) + ' to ' + str(Model_End_Time) + ' period.\n')
            fmessg.write('The model is using the ' + windmodel + ' winds from\n')
            fmessg.write(str(windmodel_Start_Time) + ' to ' + str(windmodel_End_Time))
            print 'NWPS Running for the ', str(Model_Start_Time), ' to ', str(Model_End_Time), ' period.'
            print 'The model is using the ', windmodel, ' winds from'
            print str(windmodel_Start_Time), ' to ', str(windmodel_End_Time)

            if windmodel == "NAM12":
                wmid = self.findDatabase(windmodel)
                timerange = self.createTimeRange(0, wind_model_end_hour, "Database", wmid)
                self.copyCmd(['Wind'], wmid, timerange)
                # self.copy(['Wind'], 0, wind_model_end_hour, windmodel)
            elif windmodel == "ECMWFHiRes":
                wmid = self.findDatabase(windmodel)
                timerange = self.createTimeRange(0, wind_model_end_hour, "Database", wmid)
                self.copyCmd(['Wind'], wmid, timerange)
                # self.copy(['Wind'], 0, wind_model_end_hour, windmodel)
            elif windmodel == "GFS40":
                wmid = self.findDatabase(windmodel)
                timerange = self.createTimeRange(0, wind_model_end_hour, "Database", wmid)
                self.copyCmd(['Wind'], wmid, timerange)
                # self.copy(['Wind'], 0, wind_model_end_hour+6, windmodel)

            fmessg.write(str(timeRange))
            self.interpolateCmd(['Wind'], timerange)

        else:

            fmessg = open(WRKSWN, 'w')
            fmessg.write('NWPS Running for the ' + str(Model_Start_Time) + ' to ' + str(Model_End_Time) + ' period.\n')
            print "NWPS Running for the ", Model_Start_Time, " to ", Model_End_Time, " period."
        ######################################################################################
