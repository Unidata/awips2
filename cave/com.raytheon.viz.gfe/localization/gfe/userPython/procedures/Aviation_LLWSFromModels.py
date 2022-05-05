# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Aviation_LLWSFromModels - Version 20181203
#
# Author:  Matthew H. Belk                  Created: 10/29/2009
#          WFO Taunton MA             Last Modified: 10/24/2012
# ----------------------------------------------------------------------------
# Modified to work in both A1 and A2 environments by Tom LeFebvre - 6/20/14 
# Renamed to Aviation_ and removed A1 any code. - 2/29/16 - Tom LeFebvre
# Removed references to smart tools which are no longer needed - Tom LeFebvre 2/23/18
# Refactored code into common utility - Tom LeFebvre 11/14/18
#===============================================================================
#  Configuration section.

#  Configure the ModelList to include models available at your office.
#  These are the options for the background grids.
#
ModelList=["NAM12", "GFS", "RAP13"]

#  Define how often there should be a sounding
#  (will interpolate if needed)
InterpHours = 1

#  Define shear magnitude at which LLWS should be reported ( /s )
#  Official level for LLWS in TAFs is 0.169 /s for a 200 ft layer.
#  Typical shear values are < 0.080 /s
#
#  We need to use lower thresholds for deeper layers to account for
#  vertical resolution limitations in model data.
ReportLLWS_dict = {
                   "500": 0.138,           #  500 ft layers
                   "1000": 0.065,           #  1000 ft layers (was .865)
                   "1500": 0.035,           #  1500 ft layers
                   "2000": 0.025,            #  2000 ft AGL layer
}

#  Define a toggle to create some debug grids - can also be used for
#  training purposes
CreateDebugGrids = 0            #  1 = Yes, 0 = No
CreateVectorComponents = 0      #  1 = Yes, 0 = No

#===============================================================================
#  Set up variables to be solicited from the user

#  Start with an empty dialog
VariableList = []

#  Make a global list to hold all the names we add to the dialog
dialogNames = []

#  For each model we have configured
for model in ModelList:

    #  Add a slider bar to handle the last two versions this model
    VariableList.append((model, 0, "scale", [0,10], 1))
    VariableList.append(("Previous %s" % (model), 0, "scale", [0,10], 1))

    #  Add these names to the dialog list so we can get their data later on
    dialogNames.append(model)
    dialogNames.append("Previous %s" % (model))

## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

MenuItems = ["Populate"]

import time

import numpy as np

import Aviation_Utils
import GridManipulation
import ModelBlendUtility 
import SmartScript


class Procedure (ModelBlendUtility.ModelBlendUtility,
                 GridManipulation.GridManipulation,
                 SmartScript.SmartScript):

    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
        self._aviationUtils = Aviation_Utils.Aviation_Utils(dbss, None)

    #===========================================================================
    #  Method to set up Procedure to manipulate sounding data

    def _processVariableList(self, varDict):

        #-----------------------------------------------------------------------
        #  Get name of chosen model(s) - and fix it up so we can use it later 
        #  on.  This will grab the latest version of the chosen model.
        self._models, self._modelWeights = self._aviationUtils.getModelWeights(dialogNames,
                                                                               varDict)

        #-----------------------------------------------------------------------
        #  Determine model levels available for each model within 2000 ft of
        #  the ground.  We'll use the boundary layer levels since they are
        #  already AGL.  Levels must be listed in order of ascending height
        self._levels = ["BL030", "BL3060"]

    #===========================================================================
    #  Define a method to compute vertical wind shear between two levels

    def _computeShear(self, uBottom, vBottom, uTop, vTop, depth):

        duSquared = np.power((uTop - uBottom), 2)
        dvSquared = np.power((vTop - vBottom), 2)

        shear = np.sqrt(duSquared + dvSquared) / depth

        return shear


    ############################################################################
    #  Run this procedure
    ############################################################################
    
    def execute(self, timeRange, varDict):
        executeTR = timeRange

        #-----------------------------------------------------------------------
        #  Log the use of this procedure
        
        self.statusBarMsg("Aviation_LLWSFromModels started...", "R")
        self.GM_logToolUse("Aviation_LLWSFromModels started")

        #-----------------------------------------------------------------------
        #  Process the VariableList

        self._processVariableList(varDict)

        #-----------------------------------------------------------------------
        #  Indicate we want to use Python numpy to process grids

        self.setToolType("numeric")

        #-----------------------------------------------------------------------
        #  If we don't have any selected models we can use

        print(len(list(self._modelWeights.keys())), list(self._modelWeights.keys()))
        if not self._modelWeights.keys():
            #  Let user know we are aborting and why
            self.abort("\n\nNo available data for model soundings!\n" +
                       "Be sure to select at least one model.")

        #=======================================================================
        #  Make a list of all MixHgt grids locked by other users, so we can 
        #  detect the locks before trying to modify the grids

        llwsLocks = self.GM_getParmLocksByOthers("LLWS")
        llwsHgtLocks = self.GM_getParmLocksByOthers("LLWSHgt")
        
        #-----------------------------------------------------------------------
        #  Delete all the grids in the adjusted time range

        self.deleteCmd(["LLWS", "LLWSHgt"], executeTR)

        ########################################################################
        #  Now that we have the sounding data, cycle through each time

        mixHgtTRList = self.GM_makeTimeRangeList(executeTR, InterpHours)

        for tr in mixHgtTRList:

#            print "Computing for -> ", tr

            #-------------------------------------------------------------------
            # Check the tr in various ways to see if it's worth going on

            # Make sure we overlap the executeTR 
            if not tr.overlaps(executeTR):
                print("\tdoesn't overlap")
                continue

            # Make sure the tr is not locked
            if tr in llwsLocks:
                self.statusBarMsg("LLWS grid is locked at:" + str(tr), "S")
                continue
            
            if tr in llwsHgtLocks:
                self.statusBarMsg("LLWSHgt grid is locked at:" + str(tr), "S")
                continue

            # Restrict based on hour of the day
            # Shouln't need to do this if the wind grids are also defined
            # at this resolution.
            start = time.gmtime(tr.startTime().unixTime())
            if not start.tm_hour in range(0, 24, InterpHours):
                continue

            #===================================================================
            #  Get ready to compute weighted-average vector components at 1000
            #  and 2000 ft

            avgU1000 = self.empty()
            avgV1000 = self.empty()
            avgU2000 = self.empty()
            avgV2000 = self.empty()
            count = 0

            #-------------------------------------------------------------------
            #  Construct a list of all model data we have for this time

            #-------------------------------------------------------------------
            #  Look through all of the chosen models
            t1 = time.time()

            for model in self._models:

                modelInventory = self.GM_getWEInventory("wind", model, "BL030")

                #  Fetch the grids we need for thsi time step                 
                if tr not in modelInventory:
                    wind1000 = self.GM_interpolateGrid(model, "wind", "BL030", 
                                                       tr, modelInventory)
                    wind2000 = self.GM_interpolateGrid(model, "wind", "BL3060",
                                                       tr, modelInventory)

                else:
                    wind1000 = self.getGrids(model, "wind", "BL030", tr)
                    wind2000 = self.getGrids(model, "wind", "BL3060", tr)
                    
                if wind1000 is None or wind2000 is None:
                    self.statusBarMsg(model + " BL winds are missing at: " + 
                                      str(tr), "A")
                    continue
                #---------------------------------------------------------------
                #  Convert these winds into U and V vector components

                (u1000, v1000) = self.MagDirToUV(wind1000[0], wind1000[1])
                (u2000, v2000) = self.MagDirToUV(wind2000[0], wind2000[1])

                #---------------------------------------------------------------
                #  Adjust these components by the chosen model weight

                u1000 = u1000 * float(self._modelWeights[model])
                v1000 = v1000 * float(self._modelWeights[model])
                u2000 = u2000 * float(self._modelWeights[model])
                v2000 = v2000 * float(self._modelWeights[model])

                #---------------------------------------------------------------
                #  Add this data to the existing average components
                avgU1000 = avgU1000 + u1000
                avgV1000 = avgV1000 + v1000
                avgU2000 = avgU2000 + u2000
                avgV2000 = avgV2000 + v2000

                #---------------------------------------------------------------
                #  Count the weight for this model

                count = count + float(self._modelWeights[model])

            self.GM_logToolUse("%f seconds to fetch model grids." % 
                             (time.time() - t1))

            if count == 0.0:
                continue
            
            #===================================================================
            #  Compute the average vector components at 1000 and 2000 ft for
            #  this time
            t2 = time.time()
            
            avgU1000 /= float(count)
            avgV1000 /= float(count)
            avgU2000 /= float(count)              
            avgV2000 /= float(count)

            #-------------------------------------------------------------------
            #  Now get the weighted-average winds at 1000 ft and 2000 ft

            wind1000 = self.UVToMagDir(avgU1000, avgV1000)
            wind2000 = self.UVToMagDir(avgU2000, avgV2000)

            #-------------------------------------------------------------------
            #  Now get the surface wind from the Fcst database

            Wind = self.getGrids("Fcst", "Wind", "SFC", tr)

            #-------------------------------------------------------------------
            #  Break this vector into its components - be sure to convert speed
            #  to m/s from knots so the units are the same when we compare

            if Wind is not None:
                (uSfc, vSfc) = self.MagDirToUV(Wind[0]/self.convertMsecToKts(1),
                                               Wind[1])

            #  Otherwise
            else:
                uSfc = None
                vSfc = None
           
            #-------------------------------------------------------------------
            #  Display some debug grids - if we should

            if CreateVectorComponents:

                #  Display U and V components of all the winds we have so far
                self.createGrid("Fcst", "uSfc", "SCALAR", uSfc.asType(float32),
                                tr, precision=2)
                self.createGrid("Fcst", "vSfc", "SCALAR", vSfc.asType(float32),
                                tr, precision=2)

                self.createGrid("Fcst", "avgU1000", "SCALAR",
                                avgU1000.asType(float32), tr, precision=2)
                self.createGrid("Fcst", "avgV1000", "SCALAR",
                                avgV1000.asType(float32), tr, precision=2)

                self.createGrid("Fcst", "avgU2000", "SCALAR",
                                avgU2000.asType(float32), tr, precision=2)
                self.createGrid("Fcst", "avgV2000", "SCALAR",
                                avgV2000.asType(float32), tr, precision=2)
                self.createGrid("Fcst", "wind1000", "VECTOR", wind1000, tr,
                            units="m/s", defaultColorTable="Low Range Enhanced")
                self.createGrid("Fcst", "wind2000", "VECTOR", wind2000, tr,
                            units="m/s", defaultColorTable="Low Range Enhanced")
                self.createGrid("Fcst", "count", "SCALAR", self.empty() + count,
                                tr)

            #-------------------------------------------------------------------
            #  Compute a mean wind around 500 ft - if we can

            if avgU1000 is not None and avgV1000 is not None and \
               uSfc is not None and vSfc is not None:

                u500 = ((avgU1000 * 2.0) + uSfc) / 3.0
                v500 = ((avgV1000 * 2.0) + vSfc) / 3.0

                #  Compute a magnitude and direction for the mean wind (m/s)
                wind500 = self.UVToMagDir(u500, v500)

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateVectorComponents:

                    #  Display U and V components of average BL wind
                    self.createGrid("Fcst", "u500", "SCALAR",
                                    u500.asType(float32), tr, precision=2)
                    self.createGrid("Fcst", "v500", "SCALAR",
                                    v500.asType(float32), tr, precision=2)  
                    self.createGrid("Fcst", "wind500", "VECTOR", wind500, tr,
                                units="m/s", defaultColorTable="Low Range Enhanced")
                
            else:
                u500 = None
                v500 = None

                #  Indicate we don't have a mean BL wind
                wind500 = None

            #-------------------------------------------------------------------
            #  Compute a mean wind around 1500 ft - if we can

            if avgU1000 is not None and avgV1000 is not None and \
               avgU2000 is not None and avgV2000 is not None:

                u1500 = (avgU1000 + avgU2000) / 2.0
                v1500 = (avgV1000 + avgV2000) / 2.0

                #  Compute a magnitude and direction for the mean wind (m/s)
                wind1500 = self.UVToMagDir(u1500, v1500)

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateVectorComponents:

                    #  Display U and V components of average BL wind
                    self.createGrid("Fcst", "u1500", "SCALAR",
                                    u1500.asType(float32), tr, precision=2)
                    self.createGrid("Fcst", "v1500", "SCALAR",
                                    v1500.asType(float32), tr, precision=2)  
                    self.createGrid("Fcst", "wind1500", "VECTOR", wind1500, tr,
                                units="m/s", defaultColorTable="Low Range Enhanced")
                
            else:
                u1500 = None
                v1500 = None

                #  Indicate we don't have a mean BL wind
                wind1500 = None

            #===================================================================
            #  Initialize grids to store max wind shear and height

            #  Maximum shear computed from any layer in the lowest 2000 ft AGL
            maxShear = self.empty()

            #  Maximum surplus of shear above LLWS threshold for any layer
            maxDiffShear = self.empty()

            #  Height at top of layer with greatest surplus of shear
            maxHeight = self.empty()


            ####################################################################
            #  Determine shear in 500 ft layers from surface to 2000 ft AGL
            ####################################################################

            #  Get the shear threshold to use for these layers
            shearThreshold = ReportLLWS_dict["500"]

            #-------------------------------------------------------------------
            #  Compute wind shear from surface to 500 ft AGL

            if (uSfc is not None and vSfc is not None and
                u500 is not None and v500 is not None):

                shear = self._computeShear(uSfc, vSfc, u500, v500,
                                           self.convertFtToM(500))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear0005", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_500")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 5, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind500[0], 0)
                maxDir = np.where(llwsMask, wind500[1], 0)

            #-------------------------------------------------------------------
            #  Compute wind shear from 500 to 1000 ft AGL

            if (avgU1000 is not None and avgV1000 is not None and
                u500 is not None and v500 is not None):

                shear = self._computeShear(u500, v500, avgU1000, avgV1000,
                                           self.convertFtToM(500))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear0510", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_500")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 10, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind1000[0], maxMag)
                maxDir = np.where(llwsMask, wind1000[1], maxDir)

            #-------------------------------------------------------------------
            #  Compute wind shear from 1000 to 1500 ft AGL

            if (avgU1000 is not None and avgV1000 is not None and
                u1500 is not None and v1500 is not None):

                shear = self._computeShear(avgU1000, avgV1000, u1500, v1500,
                                           self.convertFtToM(500))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear1015", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_500")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 15, maxHeight)
                                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind1500[0], maxMag)
                maxDir = np.where(llwsMask, wind1500[1], maxDir)

            #-------------------------------------------------------------------
            #  Compute wind shear from 1500 to 2000 ft AGL

            if (u1500 is not None and v1500 is not None and
                avgU2000 is not None and avgV2000 is not None):

                shear = self._computeShear(u1500, v1500, avgU2000, avgV2000,
                                           self.convertFtToM(500))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear1520", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_500")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 20, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind2000[0], maxMag)
                maxDir = np.where(llwsMask, wind2000[1], maxDir)


            ####################################################################
            #  Determine shear in 1000 ft layers from surface to 2000 ft AGL
            ####################################################################

            #  Get the shear threshold to use for these layers
            shearThreshold = ReportLLWS_dict["1000"]

            #-------------------------------------------------------------------
            #  Compute wind shear from surface to 1000 ft AGL

            if (uSfc is not None and vSfc is not None and
                avgU1000 is not None and avgV1000 is not None):

                shear = self._computeShear(uSfc, vSfc, avgU1000, avgV1000,
                                           self.convertFtToM(1000))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear0010", "SCALAR",
                                    shear.astype(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_1000")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 10, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind1000[0], maxMag)
                maxDir = np.where(llwsMask, wind1000[1], maxDir)

            #-------------------------------------------------------------------
            #  Compute wind shear from 1000 to 2000 ft AGL

            if (avgU1000 is not None and avgV1000 is not None and
                avgU2000 is not None and avgV2000 is not None):

                shear = self._computeShear(avgU1000, avgV1000, avgU2000, avgV2000,
                                           self.convertFtToM(1000))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear1020", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_1000")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 20, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind2000[0], maxMag)
                maxDir = np.where(llwsMask, wind2000[1], maxDir)


            ####################################################################
            #  Determine shear in 1500 ft layers from surface to 2000 ft AGL
            ####################################################################

            #  Get the shear threshold to use for these layers
            shearThreshold = ReportLLWS_dict["1500"]

            #-------------------------------------------------------------------
            #  Compute wind shear from surface to 1500 ft

            if (uSfc is not None and vSfc is not None and
                u1500 is not None and v1500 is not None):

                shear = self._computeShear(uSfc, vSfc, u1500, v1500,
                                           self.convertFtToM(1500))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear0015", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_1500")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 15, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind1500[0], maxMag)
                maxDir = np.where(llwsMask, wind1500[1], maxDir)

            #-------------------------------------------------------------------
            #  Compute wind shear from 500 ft to 2000 ft

            if (u500 is not None and v500 is not None and
                avgU2000 is not None and avgV2000 is not None):

                shear = self._computeShear(u500, v500, avgU2000, avgV2000,
                                           self.convertFtToM(1500))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear0520", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_1500")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 20, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind2000[0], maxMag)
                maxDir = np.where(llwsMask, wind2000[1], maxDir)

 
            ####################################################################
            #  Determine shear from surface to 2000 ft AGL
            ####################################################################

            #  Get the shear threshold to use for these layers
            shearThreshold = ReportLLWS_dict["2000"]

            #-------------------------------------------------------------------
            #  Compute wind shear from surface to 2000 ft

            if (uSfc is not None and vSfc is not None and
                avgU2000 is not None and avgV2000 is not None):

                shear = self._computeShear(uSfc, vSfc, avgU2000, avgV2000,
                                           self.convertFtToM(2000))

                #---------------------------------------------------------------
                #  Display some debug grids - if we should

                if CreateDebugGrids:

                    #  Display this grid in GFE
                    self.createGrid("Fcst", "shear0020", "SCALAR",
                                    shear.asType(float32), tr,
                                    precision=3, minAllowedValue=0.0,
                                    maxAllowedValue=0.25, units="/s",
                                    defaultColorTable="LLWS_2000")

                #  Determine difference of shear and the LLWS threshold
                diffShear = np.where(np.greater_equal(shear, shearThreshold),
                                  (shear - shearThreshold), -1)

                #  Define a mask where shear is maximum shear computed so far
                maxShearMask = np.greater_equal(diffShear, maxDiffShear)

                #  Define a mask where shear meets LLWS criterion for this layer
                #  depth and this is the maximum shear computed so far
                llwsMask = np.logical_and(np.greater_equal(shear, shearThreshold),
                                       maxShearMask)

                #  Update the max shear and height grids
                maxShear = np.where(np.greater(shear, maxShear), shear, maxShear)
                maxDiffShear = np.where(maxShearMask, diffShear, maxDiffShear)
                maxHeight = np.where(llwsMask, 20, maxHeight)
                
                #  Update wind magnitude and direction to report in LLWS group
                maxMag = np.where(llwsMask, wind2000[0], maxMag)
                maxDir = np.where(llwsMask, wind2000[1], maxDir)

            #===================================================================
            #  Ensure our LLWS heights are within valid limits

            maxHeight = np.clip(maxHeight, 0, 20)

            #-------------------------------------------------------------------
            #  Display some debug grids - if we should

            if CreateDebugGrids:

                #  Display this grid in GFE
                self.createGrid("Fcst", "maxShear", "SCALAR",
                                maxShear.asType(float32), tr,
                                precision=3, minAllowedValue=0.0,
                                maxAllowedValue=0.25, units="/s",
                                defaultColorTable="LLWS")

                if CreateVectorComponents:

                    #  Display final low-level wind shear components
                    self.createGrid("Fcst", "maxMag", "SCALAR", maxMag, tr,
                                    precision=1, minAllowedValue=0,
                                    maxAllowedValue=50, units="m/s",
                                    defaultColorTable="Low Range Enhanced")

                    self.createGrid("Fcst", "maxDir", "SCALAR", maxDir, tr,
                                    precision=1, minAllowedValue=0,
                                    maxAllowedValue=360, units="(dir)",
                                    defaultColorTable="Gridded Data")

            #-------------------------------------------------------------------
            #  Put the wind data we may possibly report with LLWS together - 
            #  be sure to convert from m/s to kts

            maxWind = (self.convertMsecToKts(maxMag), maxDir)

            #-------------------------------------------------------------------
            #  Place these grids in the database

            self.createGrid("Fcst", "LLWS", "VECTOR", maxWind, tr)
            self.createGrid("Fcst", "LLWSHgt", "SCALAR", maxHeight, tr,
                            minAllowedValue=0, maxAllowedValue=20)

            self.GM_logToolUse("%f seconds " % (time.time() - t2) + 
                               "to calculate LLWS and LLWSHgt grids.")


        self.statusBarMsg("Aviation_LLWS completed...", "R")
        self.GM_logToolUse("Aviation_LLWS completed")


