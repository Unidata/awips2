# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MixHgt_FWF.py
#
# Author: dtomalak
# ----------------------------------------------------------------------------


ToolType = "numeric"
WeatherElementEdited = "MixHgt"
from numpy import *
HideTool = 0

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
#ScreenList = ["T","Td"]
#ScreenList = ["SCALAR","VECTOR","WEATHER","DISCRETE"]

# If desired, Set up variables to be solicited from the user:
# VariableList = [
#         ("Variable name1" , defaultValue1, "numeric"),
#         ("Variable name2" , "default value2", "alphaNumeric"),
#         ("Variable name3" , ["default value1", "default value2"], "check",
#                       ["value1", "value2", "value3"]),
#         ("Variable name4" , "default value4", "radio",
#                       ["value1", "value2", "value3"]),
#         ("Variable name5" , defaultValue, "scale",
#                       [minValue, maxValue], resolution),
#         ("Variable name6" , "", "model"),
#         ("Variable name7" , "", "D2D_model"),
#         ("Label contents" , "", "label"),
#         ("", dialogHeight, "scrollbar"),
#        ]

# Set up Class
import SmartScript
import string, time
# For available commands, see SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, T, Topo):
        "Calculates mixing height by looking for lapse rate changes above the surface"

        fwModel = self.getObject("FireModel", "ModelType")

        if fwModel == "GFS40":
            modelSource = "_D2D_GFS40"
        elif fwModel == "NAM40":
            modelSource = "_D2D_NAM40"
        else:
            modelSource = "_D2D_NAM12"
        
        # 
        # This determines the correct full model run to use.
        #
        
 ##       hour = int(time.strftime('%H', time.gmtime()))

 ##       if hour > 15 or hour < 4:
##            run = '_1200'
##        else:
##            run = '_0000'
##        month = time.strftime('%Y%m%d', time.gmtime())
##        day = time.strftime('%d', time.gmtime())
##        monthrun = month + run
        site =  self.getSiteID()
##        model = site + modelSource + monthrun
        model = site + modelSource
        sfc_model = model

        if fwModel == "NAM40":
            sfc_model = site + "_D2D_NAM12"
##            sfc_model = site + "_GRID_D2D_NAM12" + monthrun

        print "Using " + model + " for Mixing Height Calculation"
        print "MixHgt time range is: \n" + `GridTimeRange`

        #
        # Set the levels of the eta sounding to use.
        #

        self.__D2Dmodel = model
        soundingLevels = ["MB975", "MB950", "MB925", "MB900", "MB875","MB850",
                          "MB825","MB800","MB775","MB750","MB725","MB700",
                          "MB650","MB600", "MB550","MB500"]

        #
        # Get the surface pressure and temperature from the eta model.
        #
        
        
        self.__D2Dmodel = sfc_model
        sfc_pressure = self.getGrids(sfc_model, 'p', 'SFC', GridTimeRange)
        sfc_pressure = sfc_pressure / 100.0
        sfc_model_temp = self.getGrids(sfc_model, 't', 'FHAG2', GridTimeRange)
        self.__D2Dmodel = model

        # For working with sounding, convert topo field to meters and sfc temp to kelvin
        # There is a two degree offset to allow mixing when the model sounding is not truly
        # dry adiabatic in the low levels.

        sfcTempK = self.convertFtoK(T) + 0.00
        topoMeters = Topo/3.281

        #
        # Compute the difference between the model surface temp and the forecast surface temp.
        #

        thetaOffset = where(less(sfcTempK, sfc_model_temp), (sfc_model_temp - sfcTempK), 0.00)

        # Create Surface Theta
        sfcTheta =  sfcTempK * pow((1000 / sfc_pressure), 0.286)
        
        # Create the Height and Temperature Cubes
        sounding = self.makeNumericSounding(self.__D2Dmodel, 't', soundingLevels, GridTimeRange, noDataError=0)
        if sounding is None:
           self.noData()
        ghCube, tCube = sounding
        
        # Initialize Mixing Hgt Grid
        MixHgt = zeros(T.shape)

        # Initialize the mixDepth variable.
        mixDepth = zeros(T.shape)

        # Initialize the bottom of the sounding. 1.0 is a seed value to avoid divide by zero.
        lastTheta = sfcTheta
        lastghCube = topoMeters + 1.0

        # Climb through sounding, checking to see if the surface theta is less than the upper theta
        for level in xrange(ghCube.shape[0]):
            # Get pressure at the current level.
            pressure = string.atof(soundingLevels[level][2:])

            # Compute the potential temperature of the environment at the current level.
            potTemp = (tCube[level] - thetaOffset)* pow((1000 / pressure), 0.286)

            # The mixing height is ready to set when the environmental potential temperature
            # is greater than the parcel potential temperature, and the height at the current
            # level is above the surface.
            readyToSet = logical_and(less_equal(topoMeters, ghCube[level]), logical_and(equal(MixHgt,0), greater(potTemp, sfcTheta)))

            # This part allows the mixing height to be between levels...ie. the sounding could
            # have crossed before the current height. This will bring it back to an offset of
            # that level.


            # To turn off adjustment, comment out the next line, and uncomment the other.
            # Adjusted:
            trueMixHgt = lastghCube + (((sfcTheta - lastTheta)/(potTemp - lastTheta))*(ghCube[level] - lastghCube)) 
            # Unadjusted:
            #trueMixHgt = (ghCube[level] - topoMeters)

            # Apply a check that the adjusted mixing height can never be greater than the
            # unadjusted mixing height.
            trueMixHgtCheck = (ghCube[level] - topoMeters)
            trueMixHgt = where(greater(trueMixHgt, trueMixHgtCheck), trueMixHgtCheck, trueMixHgt)
            
            # If the surface temp is greater than the eta...mix to the height where the surface
            # theta is equal to the environment
            mixDepth = where(greater_equal(sfcTempK, sfc_model_temp), trueMixHgt, mixDepth)

            # If the surface temp is colder than the eta, use the eta surface temp to establish the unadjusted height,
            # then reduce it by a percentage of the surface temp/mix height temp difference.
            mixDepth = where(logical_and(less(sfcTempK, sfc_model_temp), greater_equal(sfcTempK, tCube[level])), ((sfcTempK - tCube[level])/(sfc_model_temp - tCube[level]) * trueMixHgt), mixDepth)            

            # If the surface is even colder than the mixing height temp...do not allow mixing.
            mixDepth[less(sfcTempK,tCube[level])] = 1.0

            # Set the mixing height for valid points
            MixHgt = where(readyToSet, mixDepth, MixHgt)

            # Set parameters for next iteration of loop
            lastTheta = potTemp
            lastghCube = (ghCube[level] - topoMeters)

        #
        # Final adjustments
        #

        # Change mixing height from meters to feet
        MixHgt = MixHgt * 3.281

        # Set a minimum value of 250 feet to account for plume mixing
        MixHgt[less(MixHgt, 250.0)] = 250.0
        
        # return the mixing height
        return MixHgt
