# migrated to awips2 PM/HG 9/19/14
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# LowCloudHeight   Last Update:  06 December 2018
#
# CloudBase_Fm_CCL_LCL - Version 20181206
#
# Version 1.2    Created 26 June 2014
#                Modified from the ConvectiveCondensationLevel tool
#
# Author:  Patrick D. Moore  WFO Greenville-Spartanburg (GSP)
#
# Low clouds may develop through mechanical lifting or diabatic heating of a
# surface layer or boundary layer parcel.  When the parcel is lifted adiabatically
# through mechanical means, a cloud will form at or slightly above the Lifting
# Condensation Level (LCL).  Ordinary cumulus clouds will develop through boundary
# layer heating if moisture is sufficient.  The base of these convective clouds
# will be the Convective Condensation Level (CCL).  When the boundary layer is
# well-mixed and clouds form purely through boundary layer heating, the LCL and CCL
# will be nearly identical.  In situations when some other forcing is contributing
# to the lifting of air parcels in the boundary layer, the LCL will be below the CCL.
#
# The CloudHeight tool is designed to compute the LCL and CCL from the values in
# T and Td grids and model data.  The LCL is computed using Espy's approximation,
# which is accurate to within 1% of the LCL height under normal conditions. To find
# the CCL, an average boundary layer mixing ratio is computed between the mixing
# ratio at the surface (using the Td grid) and the lowest two model boundary layers.
# The saturation mixing ratio is computed at each model level above the ground and
# is compared to the boundary layer mixing ratio. The CCL is found at the height
# where the saturation mixing ratio value on the temperature sounding becomes lower
# than the boundary layer mixing ratio.  Where no CCL is found, a value of 250 is
# returned.  The results of the LCL and CCL calculations are converted to hundreds
# of feet AGL and placed in # the PredHgt grid, depending on forecaster choice.  
#
# Last Modified:
#
#   04/25/13    Added a slider bar to eliminate mid level clouds from the
#               determination of the cloud height.
#
#   05/24/13    Fixed a bug where the CCL was incorrectly compared to the
#               topo height, resulting in no CCL over the higher terrain.  Doh!
#
#   08/14/14    Added LCL calculation and option to compute for a surface-based
#               parcel or a boundary layer parcel.  Also included option to leave
#               the existing PredHgt grid alone above a certain threshold.
#
#   09/19/14    Updated and migrated to Awips-2#
#
#   10/18/17    Adopted as EDAS supported tool. Modified to conform to coding conventions
#
#   11/14/18    Code changes to satisfy code review.
#
# ----------------------------------------------------------------------------
#
ToolType = "numeric"
WeatherElementEdited = "variableElement"
ScreenList = ["CloudBasePrimary", "CloudBaseConditional", "CloudBaseSecondary"]

#
# Set up variables to be solicited from the user:
VariableList = [("Find the height of low clouds using either the LCL or CCL\not a surface or boundary layer parcel and put the result\nin CloudBasePrimary. If no CCL is found, a value of 250 is used.", "", "label"),
                ("Create\nCloudBasePrimary\nWith:", "CCL", "radio", ["LCL", "CCL"]),
                ("Model:", ["NAM12"],"check", ['RAP13', "NAM12", "GFS"]),
                ("Run:", "Current", "radio", ["Current", "Previous"]),
                ("If more than one model is selected, the result will be an\nequal-parts blend of all the models selected.", "", "label"),
                ("Lift Parcel From:", "Boundary Layer", "radio", ["Surface", "Boundary Layer"]),
                ("Mid/High Clouds:", "Keep", "radio", ["Keep", "Eliminate"]),
                ("Upper Threshold For Low Clouds (100s of FT):", 60.0, "scale", [30.0, 100.0], 10.0),
                ("Above the threshold, mid and high clouds in CloudBasePrimary\nwill be kept or eliminated where no CCL is found.", "", "label")]
#
import SmartScript
import numpy as np
import GridManipulation


class Tool (SmartScript.SmartScript):

    def __init__(self, dbss):

        SmartScript.SmartScript.__init__(self, dbss)
        
        self._GM = GridManipulation.GridManipulation(dbss)
        
    # Returns a TimeRange that is closest in time to the specified timeRange 
    def getClosestTimeRange(self, dbID, timeRange):
        
        trList = self._GM.GM_getWEInventory("t", dbID, level="MB500") 
        
        minDiff = 24 * 3600  # init to big number 
        for tr in trList:
            diff = abs(tr.startTime().unixTime() - timeRange.startTime().unixTime())
            if diff < minDiff:
                minDiff = diff
                minTR = tr

        return minTR

    def execute(self, CloudBasePrimary, T, Td, Topo, GridTimeRange, varDict):
        "Finds the CloudBasePrimary according to either the CCL or LCL"

        # Set up variables from the varDict
        predHgtMethod = varDict["Create\nCloudBasePrimary\nWith:"]
        modelName = varDict["Model:"]
        modelRun = varDict["Run:"]
        parcelSource = varDict["Lift Parcel From:"]
        cloudHgtLimit = varDict["Upper Threshold For Low Clouds (100s of FT):"]
        choice = varDict["Mid/High Clouds:"]
       
        #   Set up dictionaries to hold the model layer and corresponding pressure
        layers = ["MB1000","MB975","MB950","MB925","MB900","MB875","MB850",
                   "MB825","MB800","MB775","MB750","MB725","MB700","MB675",
                   "MB650","MB625","MB600","MB575","MB550","MB525","MB500"]

        pLevels = [1000.0, 975.0, 950.0, 925.0, 900.0, 875.0, 850.0,
                    825.0, 800.0, 775.0, 750.0, 725.0, 700.0, 675.0,
                    650.0, 625.0, 600.0, 575.0, 550.0, 525.0, 500.0]

        #   initialize counters for averaging
        accum = 0
        counter = 0

        #   convert Topo to metres
        topoM = Topo / 3.2808

        #   initialize the starting value to the ground level
        startHgt = Topo

        #   initialize temporary grids
        heightLCL = self.empty()
        LCL  = self.empty()
        CCL  = self.empty()
        cloudMask = self.empty()

        #   for each model selected...
        for model in modelName:
            modelID = "D2D_" + model

            #       Set the database ID
            if modelRun == "Current":
                dataBaseID = self.findDatabase(modelID, 0)
            else:
                dataBaseID = self.findDatabase(modelID, -1)

            modelTimeRange = self.getClosestTimeRange(dataBaseID, GridTimeRange)

            #       Get the t Cube
            gh = self.makeNumericSounding(dataBaseID, 't', layers,
                                          modelTimeRange, noDataError=0)
            
            print("Make sounding t..........................................................")

            if gh is None:
                self.noData()

            ghCube, tCube = gh

            #       Get the rh Cube
            relh = self.makeNumericSounding(dataBaseID, 'rh', layers, modelTimeRange, noDataError=0)
            print("Make sounding rh..........................................................")

            if relh is None:
                self.noData()

            ghCube, rhCube = relh

            #       try to get surface pressure
            pMSL = self.getGrids(dataBaseID, "p", "SFC", modelTimeRange)
            print("Make sounding p..........................................................")

            #       convert sfc pressure from model (kPa) to hPa (mb)
            pSFC = pMSL / 100.0

            #       Initialize values for CCL iteration
            tempCCL = topoM
            lastCCL = topoM
            lastHgt = 0.0

            #       Convert the surface T and Td to Kelvin and Celsius
            td_SFC = self.convertFtoK(Td)
            t_SFC  = self.convertFtoK(T)

            tdC_SFC = td_SFC - 273.15
            tC_SFC  = t_SFC - 273.15

            #       try to get values for t and rh from lowest two boundary layers from the model.
            #       BL030 is surface to 30 mb AGL, BL3060 is a layer 30 mb to 60 mb AGL
            tBL030  = self.getGrids(dataBaseID, "t", "BL030", modelTimeRange)
            rhBL030 = self.getGrids(dataBaseID, "rh", "BL030", modelTimeRange)
            tBL3060  = self.getGrids(dataBaseID, "t", "BL3060", modelTimeRange)
            rhBL3060 = self.getGrids(dataBaseID, "rh", "BL3060", modelTimeRange)

#       calculate the model Td (in Celsius) for the mixing ratio calculation
            tdBL030  = self._calcModelDewpoint(tBL030, rhBL030)
            tdBL3060 = self._calcModelDewpoint(tBL3060, rhBL3060)

#       Find the boundary layer average mixing ratio (w), starting with
#       the surface mixing ratio calculated from the surface dewpoint
#
#       calculate vapor pressures (e) using the Bolton (1980)
#       form of the Clausius-Clapeyron Equation
            e_SFC = 6.112 * np.exp((17.67 * tdC_SFC) / (tdC_SFC + 243.5))
            eS_SFC = 6.112 * np.exp((17.67 * tC_SFC) / (tC_SFC + 243.5))

            #       calculate the surface mixing ratio and save the result
            #       so it can be displayed in a temporary grid if desired
            wSFC = (0.622 * e_SFC) / (pSFC - e_SFC)

            #       calculate the surface saturation mixing ratio as a starting
            #       point for CCL determination, and save the value in a placeholder
            sfcWs = (0.622 * eS_SFC) / (pSFC - eS_SFC)
            lastWs = sfcWs

            #       calculate the vapor pressure for each model boundary layer
            eBL030  = 6.112 * np.exp((17.67 * tdBL030) / (tdBL030 + 243.5))
            eBL3060 = 6.112 * np.exp((17.67 * tdBL3060) / (tdBL3060 + 243.5))

            #       calculate the mixing ratio for each model boundary layer
            wBL030 = (0.622 * eBL030) / (pSFC - eBL030)
            wBL3060 = (0.622 * eBL3060) / (pSFC - eBL3060)

            #       wBL is the average boundary layer mixing ratio
            wBL = (wSFC + wBL030 + wBL3060) / 3.0

            #       calculate a boundary layer average temp (in Kelvins)
            tBL = (t_SFC + tBL030 + tBL3060) / 3.0

            #       calculate a boundary layer average dewpt (in Kelvins)
            tdBL = ((tdC_SFC + tdBL030 + tdBL3060) / 3.0) + 273.15

            #       Set the parcel source for Temp, Dewpt, and Mixing Ratio (w)
            if parcelSource == "Surface":
                #           parcelSource is from Surface

                sfcTemp  = t_SFC
                sfcDewpt = td_SFC

                w = wSFC
                W = wSFC * 1000.0

            else:
                #           parcelSource is from Boundary Layer

                sfcTemp = tBL
                sfcDewpt = tdBL

                w = wBL
                W = wBL * 1000.0

            print("Creating grid Mixing Ratio")

            #       create a temporary grid for mixing ratio (W)
            self.createGrid("Temporary", "MixingRatio", "SCALAR", W, GridTimeRange,
                        precision=1, minAllowedValue=0.0, maxAllowedValue=20.1)

            ###     Find the LCL    ###

            #       Calculate the height of the LCL using Espy's equation
            heightLCL = 125.0 * (sfcTemp - sfcDewpt)

            #       Convert the LCL to hundreds of feet and then clip to 250
            LCL = (heightLCL * 3.2808) / 100.0 
            LCL = np.clip(LCL, 0.0, 250.0)
            print("Creating grid LCL")

            #       create a temporary grid for the LCL
            self.createGrid("Temporary", "LCL", "SCALAR", LCL, GridTimeRange,
                        precision=0, minAllowedValue=0.0, maxAllowedValue=250.0)

            ###     Find the CCL     ###

            #       At each model level
            for j in range(len(layers)):

                tK = tCube[j]
                hgt = ghCube[j]
                pres = pLevels[j]

                tC  = tK - 273.15

                #           set mask for grid boxes that are above the topography
                aboveGround = np.greater(hgt, topoM)

                #           calculate saturation vapor pressure (eS) at this level using the
                #           Bolton (1980) form of the Clausius-Clapeyron Equation
                eS = 6.112 * np.exp((17.67 * tC) / (tC + 243.5))

                #           calculate the saturation mixing ratio (wS) at this level
                wS = (0.622 * eS) / (pres - eS)

                #           find all grid boxes where the new Ws is lower than
                #           the boundary layer mixing ratio (w)
                newWsLower = np.less(wS, w)

                #           The CCL is ready to set when the saturation mixing ratio
                #           at the new level is less than the boundary layer mixing
                #           ratio, we are above ground level, and the CCL is still set
                #           at zero.  Set a mask for when this condition is true.
                readyToSet = np.logical_and(newWsLower, np.logical_and(aboveGround, np.less(lastCCL, topoM + 1.0)))

                #           perform a linear interpolation between model levels to make an
                #           approximation for the height where the saturation mixing ratio
                #           becomes less than the boundary layer mixing ratio
                hCross = self.linear(wS, lastWs, hgt, lastHgt, w)

                #           Find and store the level where temp goes above
                #           freezing, considering only layers above the ground
                tempCCL[readyToSet] = hCross[readyToSet]
            
                #           Set parameters for next iteration of loop
                lastWs  = wS
                lastHgt = hgt
                lastCCL = tempCCL

            #       accumulate values for averaging
            accum = accum + tempCCL
            counter = counter + 1.0
                
        #   compute the average CCL, then subtract Topo to convert to AGL.
        avgCCL = (accum / counter) - topoM

        #   find grid boxes where no CCL was found
        noCCL = np.less(avgCCL, 1.0)

        #   Convert to values used in CloudBasePrimary grid, which is expressed as hundreds of feet AGL,
        #   then clip to keep values between 0 and 250
        CCL = (avgCCL * 3.2808) / 100.00
        CCL = np.clip(CCL, 0.0, 250.0)
        print("Creating grid CCL")
#       create a temporary grid for CCL
        self.createGrid("Temporary", "CCL", "SCALAR", CCL, GridTimeRange, precision=1,
                        minAllowedValue=0.0, maxAllowedValue=250.0)

        #   find grid boxes where CCL is above threshold set by user
        cloudMask = np.greater(CCL, cloudHgtLimit)

        #   Set the CloudBasePrimary according to forecaster choice
        if predHgtMethod == "CCL":
            noCCLMask = np.logical_or(noCCL, cloudMask)

            #       Set the CloudBasePrimary to the CCL at all grid boxes below threshold, but...
            if choice == "Keep":

                #           eliminate zeroes where the previous CloudBasePrimary grid was created by scratch
                CloudBasePrimary[np.less(CloudBasePrimary, 0.01)] = 250.0

                #           keep the old CloudBasePrimary at all grid boxes above the threshold
                
                CloudBasePrimary[~noCCLMask] = CCL[~noCCLMask]

            else:

                #           eliminate the old CloudBasePrimary at all grid boxes above threshold
                CloudBasePrimary = np.where(np.logical_or(noCCL, cloudMask), 250, CCL)
                CloudBasePrimary[noCCLMask] = 250.0
                CloudBasePrimary[~noCCLMask] = CCL
                

        else:

            #       Set the CloudBasePrimary to the LCL
            CloudBasePrimary = LCL

        #   return the new value
        return CloudBasePrimary

    #
    def _calcModelDewpoint(self, temp, relhumidity):

        #   this version returns the dewpoint in deg C
        rh = np.clip(relhumidity, 0.5, 99.5)

        tempC = temp - 273.15

        #       calculate saturation vapor pressure using the
        #       Bolton (1980) form of the Clausius-Clapeyron Equation
        esBL = 6.112 * np.exp((17.67 * tempC) / (tempC + 243.5))

        #       calculate vapor pressure
        eBL = (rh / 100.0) * esBL

        #       calculate BL dewpoint
        dewpt = (243.5 * np.log(eBL / 6.112))/ (17.67 - np.log(eBL / 6.112))

        modelTd = dewpt

        return modelTd


