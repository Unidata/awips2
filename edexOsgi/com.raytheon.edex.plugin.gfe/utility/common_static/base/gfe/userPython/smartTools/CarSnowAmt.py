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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SnowAmt_TmpVV
#
# Author: Dan Cobb, Brian Meade, Dave Novak, Tom LeFebvre
# ----------------------------------------------------------------------------
# 29Nov2004 - Fixed logic that sets snow-ratio to zero if elevated warm layer
#             is encountered.  Also removed levels below 900MB due to erroneous
#             terrain effects.
#
#-----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "SnowAmt"
from numpy import *

# Set up Class
import SmartScript

# Set up dialog box
#import MyDialog

# For OB7
VariableList = [
         ("D2D Model" , "NAM12", "radio",
                       ["NAM12", "GFS40"]),
         ("Model Version" , "Latest", "radio",
                       ["Latest", "Previous"]),
         ("Thickness", "850-700", "radio",
                       ["850-700", "925-700", "850-650", "800-600", "750-550"]),
    ]


### This tool generates a grid of snowRatio based on the mean
### termperature, relative humidity, and vertical velocity
### in each layer from the surface to 500 millibars.
class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    ### Given a grid of temperature in Celcius, this method computes
    ### the base snowRatio based on the spline curve as defined by the
    ### coefficients.
    def baseSnowRatio(self, tGrid):
        #  set up the spline coefficients
        tThresh = [-30.0, -21.0, -18.0, -15.0, -12.0, -10.0, -8.0, -5.0, -3.0, 2.0]
        a = [9.0, 21.0, 31.0, 35.0, 26.0, 15.0, 9.0, 5.0, 4.0]
        b = [0.4441, 3.1119, 2.8870, -0.6599, -5.2475, -4.5685, -1.9786, -0.7544, -0.3329]
        c = [0.0, 0.2964, -0.3714, -0.8109, -0.7183, 1.0578, 0.2372, 0.1709, 0.0399]       
        d = [0.0110, -0.0742, -0.0488, 0.0103, 0.2960, -0.1368, -0.0074, -0.0218, -0.0027]

        # Initialize the coeficient grids
        aGrid = empty_like(tGrid)
        aGrid.fill(a[-1])   #last value in list
        bGrid = empty_like(tGrid)
        bGrid.fill(b[-1])
        cGrid = empty_like(tGrid)
        cGrid.fill(c[-1])
        dGrid = empty_like(tGrid)
        dGrid.fill(d[-1])
        tDiff = zeros(tGrid.shape,dtype=float32)

        # define grids of coefficients based on tGrid
        for i in xrange(len(tThresh) - 1):
            mask1 = greater_equal(tGrid, tThresh[i])
            mask2 = less(tGrid, tThresh[i+1])
            mask = logical_and(mask1, mask2) # area b/w threshold
            tDiff = where(mask, tGrid - tThresh[i], tDiff)
            aGrid[mask] = a[i]
            bGrid[mask] = b[i]
            cGrid[mask] = c[i]
            dGrid[mask] = d[i]

        # Do the calcuation using the grids of spline coefficients
        baseRatio = aGrid + bGrid * tDiff + cGrid * tDiff * tDiff \
                    + dGrid * pow(tDiff, 3)

        # Clip the snowRatio grid to 10.0 where tGrid is outside limits
        #baseRatio = where(greater(tGrid, 1.0), 0.0, baseRatio)
        #baseRatio = where(less(tGrid, tThresh[0]), 10.0, baseRatio)

        return baseRatio

    def execute(self, QPF, T, variableElement, varDict, GridTimeRange):


        #get the requested model
        modelName = varDict["D2D Model"]
        modelVersion = varDict["Model Version"]
        
        if modelVersion == "Previous":
            model = self.findDatabase("D2D_" + modelName, -1)
            print "model prev is ",model
            modelStr = self.getD2Dmodel("D2D_"+modelName)
            print "d2d is ",modelStr

        else:
            model = self.findDatabase("D2D_"+modelName, 0)
            print "model latest is ",model
            modelStr = self.getD2Dmodel(model)
            print "d2d is ",modelStr
        
        # define the levels for NAM40 and GFS40
        levels = ["MB900","MB875","MB850",
                     "MB825","MB800","MB775","MB750","MB725","MB700",
                     "MB675","MB650","MB625","MB600","MB575","MB550",
                     "MB525","MB500","MB450","MB400","MB350","MB300"]


        # Edit WSEta levels depending on vertical resolution
        WSEtaLevels = ["MB900","MB875","MB850","MB825",
                         "MB800","MB775","MB750","MB725","MB700","MB675",
                         "MB650","MB625","MB600","MB550","MB500"]
 
        if modelName == "WSEta":
            levels = WSEtaLevels
        

        # display a dialog box that tool is running
        #dialog = MyDialog.MyDialog(None,"Status","Computing SnowAmts. Stand by.")

        # get the temperature and RH cubes

        ghCube, tCube = self.makeNumericSounding(modelStr, 't', levels, GridTimeRange)
        ghCube, rhCube = self.makeNumericSounding(modelStr, 'rh', levels, GridTimeRange)
        ghCube, pvvCube = self.makeNumericSounding(modelStr, 'pvv', levels, GridTimeRange)

        # for thickness considerations, all we need for GRR area is 850 mb and 700 mb
        h85 = self.getGrids(modelStr, "gh", "MB850", GridTimeRange)
        h70 = self.getGrids(modelStr, "gh", "MB700", GridTimeRange)

        # but will also grab other heights for wrn sites thickness calculations
        h55 = self.getGrids(modelStr, "gh", "MB550", GridTimeRange)
        h60 = self.getGrids(modelStr, "gh", "MB600", GridTimeRange)
        h65 = self.getGrids(modelStr, "gh", "MB650", GridTimeRange)
        h75 = self.getGrids(modelStr, "gh", "MB750", GridTimeRange)
        h80 = self.getGrids(modelStr, "gh", "MB800", GridTimeRange)
        h92 = self.getGrids(modelStr, "gh", "MB925", GridTimeRange)


        print "Got", len(tCube), "t grids and", len(rhCube), "rh grids"

        # extract the shapes and make some variables
        #cubeShape = (len(tCube) - 1, tCube.shape[1], tCube.shape[2])
        cubeShape = (len(tCube), tCube.shape[1], tCube.shape[2])
        gridShape = (tCube.shape[1], tCube.shape[2])
        layerSR = zeros(cubeShape, 'int8')
        pvvAvg = zeros(cubeShape, 'int8')
        pvvSum = zeros(gridShape, 'int8')

        for i in range(len(ghCube) - 1):
        #for i in range(len(ghCube)):
            print "processing layer", levels[i]
            # calculate the average temp and rh in the layer
            avgTemp = tCube[i] - 273.15 # Convert to C
            avgRH = rhCube[i]

            # get the base snowRatio based on the avgTemp
            layerSR[i] = self.baseSnowRatio(avgTemp)

            # adjust snowRatio based on lapseRate
            #lr = -(tCube[i+1] - tCube[i])
            #lrAdj = where(greater_equal(lr,6.5), 1.0 + ((lr - lrMin) / (lrMax - lrMin)) * lrMaxAdj, 1.0)
            #layerSR[i] = layerSR[i] * lrAdj
            
            # Calc avg pressure vertical velocity, scale based on RH and sum
            # reverse the pvvAvg sign so up is positive
            pvvAvg[i] = -10 * (pvvCube[i])
            # clip downward vertical velocities
            pvvAvg[i][less(pvvAvg[i], 0.0)] = 0.0
            # Scale vertical velocity as a function of the square of RH.
            # This scaling will efectively negate a snowratio contribution in
            # layers that are dry.
            pvvAvg[i] = where(less(avgRH, 80.0), pvvAvg[i] * (( avgRH * avgRH ) / 6400.0 ), pvvAvg[i])
            pvvSum += pvvAvg[i]

        # Normalize the layerSnowRatio based on the pvv fraction of the total
        totalSnowRatio = zeros(gridShape,dtype=float32)
        
        #tweak the pvvSum grid to avoid division by zero
        pvvSum[less_equal(pvvSum, 0.0)] = .0001

        for i in range(len(layerSR)):
            srGrid = layerSR[i] * pvvAvg[i] / pvvSum
            totalSnowRatio += srGrid
        
        # Finally clip the snowRatio to zero under two conditions
        # cube where min colum temp > -8.0C and rh > 75%
        # This is basically Baumgardt - Top Down Approach - No ice No dice!
        #mask = logical_and(less(tCube, 265.15), greater_equal(rhCube, 50.0))
        #mask = sum(mask)  # reduce to single level by adding bits verically
        #totalSnowRatio = where(equal(mask, 0), 0.0, totalSnowRatio)

        #
        thicknessSnowRatio = zeros(gridShape,dtype=float32)
        myThickness = varDict["Thickness"]

        if myThickness == "850-700":
            thicknessSnowRatio = 20.0 - pow(((h70 - h85) - 1437.0)/29.0 , 2)
        elif myThickness == "925-700":
            thicknessSnowRatio = 20.0 - pow(((h70 - h92) - 2063.0)/41.0 , 2)
        elif myThickness == "850-650":
            thicknessSnowRatio = 20.0 - pow(((h65 - h85) - 1986.0)/39.0 , 2)
        elif myThickness == "800-600":
            thicknessSnowRatio = 20.0 - pow(((h60 - h80) - 2130.0)/42.0 , 2)
        else:  # "750-550"  
            thicknessSnowRatio = 20.0 - pow(((h55 - h75) - 2296.0)/45.0 , 2)

        #new from wes 3/20/08
        thicknessSnowRatio[less(thicknessSnowRatio, 0.0)] = 0.0
        totalSnowRatio = (totalSnowRatio * 0.50) + (thicknessSnowRatio * 0.50)
        totalSnowRatio = where(less_equal(pvvSum, 100.0), (totalSnowRatio * 0.01 * pvvSum) + (thicknessSnowRatio * (1.0 - pvvSum * 0.01)), totalSnowRatio)
        totalSnowRatio = where(less(pvvSum, 1.0), thicknessSnowRatio, totalSnowRatio)

        # If there's any layer above 0.0C, snowRatio gets 0
        mask = greater(tCube, 272.65)
        mask = sum(mask) # reduce to single level by adding bits vertically
        # if mask == 0, nowhere in the column is temp < 0.5C
        totalSnowRatio = where(equal(mask, 0), totalSnowRatio, 0.0)
        #Calculate Snowfall - taper to zero from 31 to 34 F.
        snowfall = QPF * totalSnowRatio
        snowfall = where(greater(T, 31.0), pow(35.0 - T,2)/16.0 * snowfall , snowfall)
        snowfall[greater(T, 35.0)] = 0.0
        #for now just create a new grid
        totalSnowRatio = totalSnowRatio.astype(float32)

        self.createGrid("Fcst", "snowRatio", "SCALAR", totalSnowRatio, GridTimeRange,
                         descriptiveName=None, timeConstraints=None,
                         precision=1, minAllowedValue=0.0,
                         maxAllowedValue=40.0)

        #for now just create a new grid
        self.createGrid("Fcst", "totalvv", "SCALAR", pvvSum.astype(float32), GridTimeRange,
                         descriptiveName=None, timeConstraints=None,
                         precision=0, minAllowedValue=0.0,
                         maxAllowedValue=400.0)

        # Return the new value
        return snowfall.astype(float32)
