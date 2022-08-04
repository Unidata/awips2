# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Haines.py
#
# Author: dtomalak
# ----------------------------------------------------------------------------

ToolType = "numeric"

WeatherElementEdited = "Haines"

HideTool = 0

#ScreenList = ["Haines"]


from numpy import *

import SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
    
    def execute(self, GridTimeRange):
        "Sets Haines according to selected model"

         
        fwModel = self.getObject("FireModel", "ModelType")

        if fwModel == "NAM12":
            modelSource = "_D2D_NAM12"
        elif fwModel == "NAM40":
            modelSource = "_D2D_NAM40"
        else:
            modelSource = "_D2D_GFS40"
            

        ############################################
        #
        # Valid Modes are 'Low', 'Mid', or 'High'
        #
        ############################################

        Mode = 'Low'

##        month = time.strftime('%b%d', time.gmtime())
##        monthrun = month + run
        site =  self.getSiteID()
##        model = site + modelSource + monthrun
        model = site + modelSource
        print("Using " + model + " for Haines Index Calculation")
        print("Haines time range is: \n" + repr(GridTimeRange))
            
        T950 = self.getGrids(model,"t","MB950",GridTimeRange,noDataError=0)
        if T950 is None:
            self.noData()

        T850 = self.getGrids(model,"t","MB850",GridTimeRange,noDataError=0)
        if T850 is None:
            self.noData()

        T700 = self.getGrids(model,"t","MB700",GridTimeRange,noDataError=0)
        if T850 is None:
            self.noData()

        T500 = self.getGrids(model,"t","MB500",GridTimeRange,noDataError=0)
        if T850 is None:
            self.noData()

        RH850 = self.getGrids(model,"rh","MB850",GridTimeRange,noDataError=0)
        if RH850 is None:
            self.noData()

        RH700 = self.getGrids(model,"rh","MB700",GridTimeRange,noDataError=0)
        if RH850 is None:
            self.noData()

        #
        # Calculate 850 MB Depressions
        #

        Dwpt_Factor_850 = (log(RH850/100.0) + ((17.27 * (T850 - 273.3))/(237.3 + (T850 - 273.31))))/ 17.27
        DD850 = T850 - ((Dwpt_Factor_850 * 237.3) / ( 1.0 - Dwpt_Factor_850 ) + 273.3)

        #
        # Calculate 700 MB Depressions
        #
        
        Dwpt_Factor_700 = (log(RH700/100.0) + ((17.27 * (T700 - 273.3))/(237.3 + (T700 - 273.31))))/ 17.27
        DD700 = T700 - ((Dwpt_Factor_700 * 237.3) / ( 1.0 - Dwpt_Factor_700 ) + 273.3)

        if Mode.upper() == 'LOW':

            # find T difference between levels
            Tdiff = (T950 - T850)

            # compute A & B terms
            Aterm = 2
            Bterm = 2
            Aterm = where(less_equal(Tdiff,3),1,Aterm)
            Aterm[greater_equal(Tdiff,8)] = 3
            Bterm = where(less_equal(DD850,5),1,Bterm)
            Bterm[greater_equal(DD850,10)] = 3

            # compute Haines
            Haines = Aterm + Bterm        

            return Haines

        if Mode.upper() == 'MID':

            # find T difference between levels
            Tdiff = (T850 - T700)

            # compute A & B terms
            Aterm = 2
            Bterm = 2
            Aterm = where(less_equal(Tdiff,5),1,Aterm)
            Aterm[greater_equal(Tdiff,11)] = 3
            Bterm = where(less_equal(DD850,5),1,Bterm)
            Bterm[greater_equal(DD850,13)] = 3

            # compute Haines
            Haines = Aterm + Bterm        

            return Haines 

        if Mode.upper() == 'HIGH':

            # find T difference between levels
            Tdiff = (T700 - T500)

            # compute A & B terms
            Aterm = 2
            Bterm = 2
            Aterm = where(less_equal(Tdiff,17),1,Aterm)
            Aterm[greater_equal(Tdiff,22)] = 3
            Bterm = where(less_equal(DD700,14),3,Bterm)
            Bterm[greater_equal(DD700,21)] = 1

            # compute Haines
            Haines = Aterm + Bterm        

            return Haines 
