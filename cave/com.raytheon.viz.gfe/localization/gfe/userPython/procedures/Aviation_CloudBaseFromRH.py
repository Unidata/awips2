# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Aviation_CloudBaseFromRH  - Version 20181114
#
# Author:  Matthew H. Belk                  Created: 01/22/2010
#          WFO Taunton MA
            
# ----------------------------------------------------------------------------
# Modified to work in both A1 and A2 environments by Tom LeFebvre - 6/20/14 
# Converted to use new weather element names - 6/14/15 - Tom LeFebvre
# Renamed to Aviation_ and removed A1 any code. - 2/29/16 - Tom LeFebvre
# Minor clean up and moved to new aviation tools. - 3/03/17 - Tom LeFebvre
# Minor changes from code review. - 11/14/18 - Tom LeFebvre
#===============================================================================
#  Configuration section.

#  Configure the ModelList to include models available at your office.
#  These are the options for the background grids.
#
ModelList=["NAM12", "GFS", "RAP13"]

#  Configure number of hours to adjust selected time range before and after
AdjustTR = 0

#  Define how often there should be a sounding
#  (will interpolate if needed)
InterpHours = 1

#  Define a maximum cloud base height in 100s of feet
MaxCloudBase = 250

#  Define a toggle to create some debug grids - can also be used for
#  training purposes
CreateDebugGrids = False            #  False = No, True = Yes

#  End configuration section.

#===============================================================================
#  Set up variables to be solicited from the user

#  Start with an empty dialog
VariableList = []

#  Make a global list to hold all the names we add to the dialog
dialogNames = []

#  Add a control to alter RH thresholds
VariableList.append(("Use AGL cloud heights:", "Yes", "radio", ["Yes", "No"]))

#  Add a control to alter RH thresholds
VariableList.append(("Populate:", "CloudBasePrimary", "radio", ["CloudBasePrimary",
                     "CloudBaseSecondary", "CloudBaseConditional"]))

#  For each model we have configured
for model in ModelList:

    #  Add a slider bar to handle the last two versions this model
    VariableList.append((model, 0, "scale", [0,10], 1))
    VariableList.append(("Previous %s" % (model), 0, "scale", [0,10], 1))

    #  Add these names to the dialog list so we can get their data later on
    dialogNames.append(model)
    dialogNames.append("Previous %s" % (model))

#  Add a label to explain the following option
VariableList.append(("Adjusting the slider bar below to the\n" +
                     "right will make it tougher to detect a\n" +
                     "cloud, which should raise cloud heights.\n" +
                     "Moving the slider bar to the left,\n" +
                     "will make it easier to detect a cloud.", "", "label"))

#  Add a control to alter RH thresholds
VariableList.append(("Alter RH thresholds for clouds by (%):", 0, "scale",
                     [-20,20], 1))


################################################################################
#  Import Python modules we will need

import time

import numpy as np

import Aviation_Utils
import GridManipulation


MenuItems = ["Populate"]

class Procedure (GridManipulation.GridManipulation):
    def __init__(self, dbss):
        GridManipulation.GridManipulation.__init__(self, dbss)

        self._aviationUtils = Aviation_Utils.Aviation_Utils(dbss, None)

    def _processVariableList(self, varDict):

        #  Set flag for using AGL heights

        self.useAglHeights = varDict["Use AGL cloud heights:"] == "Yes"
       
        #-----------------------------------------------------------------------
        #  Determine which cloud field to populate

        self._populateHeight = varDict["Populate:"]

        #-----------------------------------------------------------------------
        #  Make an empty list of models we can use

        self._models, self._modelWeights = self._aviationUtils.getModelWeights(dialogNames,
                                                                               varDict)

        #-----------------------------------------------------------------------
        #  Determine model levels and RH threshold which will result in a cloud
        #  at each. Levels must be listed in order of ascending height.

        self._levels = ["MB1000","MB975", "MB950", "MB925", "MB900",
                        "MB875", "MB850", "MB825", "MB800", "MB775",
                        "MB750", "MB725", "MB700", "MB675", "MB650",
                        "MB625", "MB600", "MB575", "MB550", "MB525",
                        "MB500", "MB450", "MB400", "MB350", "MB300"
                       ]
        #  Define RH threshold which indicates a cloud for each level
        #  We need to go from the top of the cube down since numpy requires
        #  increasing x-values (pressure) in order to compute 1-D splines  
        #  key = pressure level, value = RH threshold                      
        self._cloudRH = {1000:98.0, 975:96.0, 950:94.0, 925:92.0, 900:90.0,      
                          875:88.0, 850:85.0, 825:83.0, 800:80.0, 775:78.0,
                          750:75.0, 725:73.0, 700:70.0, 675:68.0, 650:65.0,
                          625:63.0, 600:60.0, 575:58.0, 550:55.0, 525:53.0,
                          500:50.0, 450:45.0, 400:40.0, 350:35.0, 300:30.0
                        }

        #  See if these thresholds are to be altered
        alterRHthresholds = varDict["Alter RH thresholds for clouds by (%):"]

        #=======================================================================
        #  Create a 1-D spline to determine cloud bases
        
        #  Sort dictionary keys and values so we can compute a 1-D spline
#          pKnots = np.array(sorted(self._cloudRH.keys()))
        rhKnots = []
#          presCube = []

        #  Construct the pressure cube we need, plus the RH knots for the spline
        #  function. These must be in ascending order to satisfy scipy. 
        #  Physically, this is opposite order we need.
        for key in sorted(list(self._cloudRH.keys()), reverse=True):
            rhKnots.append(self.newGrid(self._cloudRH[key] + alterRHthresholds))
#              presCube.append(self.newGrid(key))

        #  Finish construction of the numpy cubes
        self._rhThreshold = np.array(rhKnots)

        #  Identify the rows and columns of a single grid.
        #  It's important to remember that numpy defines "rows", "columns" and
        #  "depth" differently from how they are used in GFE. Thus we need to
        #  reverse the order here in constructing the meshgrid. We'll use them
        #  in the GFE-proper order later on.    
        empty = self.empty()
        self.BI_col, self.BI_row = np.meshgrid(np.arange(empty.shape[1]),
                                               np.arange(empty.shape[0]))

    ############################################################################
    #  Run this procedure
    ############################################################################
    
    def execute(self, timeRange, varDict):
        t0 = time.time()
        
        executeTR = timeRange
        print("ExecuteTR:", executeTR)

        #-----------------------------------------------------------------------
        #  Log the use of this procedure
        
        self.statusBarMsg("Aviation_CloudBaseFromRH started...", "R")

        #-----------------------------------------------------------------------
        #  Process the VariableList

        self._processVariableList(varDict)

        #-----------------------------------------------------------------------
        #  If we don't have any selected models we can use

        if len(list(self._modelWeights.keys())) == 0:
            self.statusBarMsg("No available data for model soundings!\n" +
                       "Be sure to select at least one model.", "S")
            return
        
        self.setToolType("numeric")

        #-----------------------------------------------------------------------
        #  Get the topography of our domain in feet

        topo = self.getTopo()

        #-----------------------------------------------------------------------
        #  Display these data if debugging
        
        if CreateDebugGrids:
            self.createGrid("Fcst", "topo", "SCALAR", topo, timeRange,
                            minAllowedValue=-3000.0, maxAllowedValue=25000.0)

        #=======================================================================
        #  Make a list of all CloudBasePrimary or ClloudBaseSecondary grids locked by other
        #  users, so we can detect the locks before trying to modify the grids

        predHgtLocks = self.GM_getParmLocksByOthers(self._populateHeight)

        #-----------------------------------------------------------------------
        #  Delete all the grids in the adjusted time range

        self.deleteCmd([self._populateHeight], executeTR)

        ########################################################################
        #  Now that we have the sounding data, cycle through each time

        predHgtTRList = self.GM_makeTimeRangeList(executeTR, InterpHours)

        for tr in predHgtTRList:

            #-------------------------------------------------------------------
            # Check the tr in various ways to see if it's worth going on

            # Make sure we overlap the executeTR 
            if not tr.overlaps(executeTR):
                print("\tdoesn't overlap")
                continue

            # Make sure the tr is not locked
            if tr in predHgtLocks:
                self.statusBarMsg("%s grid is locked at:" % 
                                  (self._populateHeight) + str(tr), "S")
                continue

            # Restrict based on hour of the day
            # Shouldn't need to do this if the wind grids are also defined
            # at this resolution.
            start = time.gmtime(tr.startTime().unixTime())
            if not start.tm_hour in range(0, 24, InterpHours):
                continue

            #===================================================================
            #  Get ready to compute weighted-average height and RH cubes, as
            #  well as cloud base height

            avgHgtCube = 0.0
            avgRhCube = 0.0
            totalWeight = 0.0
            cloudBaseHgt = self.newGrid(MaxCloudBase)

            #-------------------------------------------------------------------
            #  Construct a list of all model data we have for this time

            #-------------------------------------------------------------------
            #  Look through all of the chosen models
#              t1 = time.time()

            for model in self._models:

                modelInventory = self.GM_getWEInventory("rh", model,
                                                        self._levels[0])

                #Fetch the cubes                
                if tr not in modelInventory:
                    sounding = self.GM_interpolateSounding(model, "rh", 
                                           self._levels, tr, modelInventory)
                else:
                    sounding = self.makeNumericSounding(model, "rh", 
                                            self._levels, tr, noDataError=0)
                    
                if sounding is None:
                    self.statusBarMsg(model + " sounding is missing at: " + 
                                      str(tr), "A")
                    continue

                # Extract the height and RH cubes
                ghCube, rhCube = sounding

                #-----------------------------------------------------------
                #  Adjust these data by the chosen model weight

                ghCube *= float(self._modelWeights[model])
                rhCube *= float(self._modelWeights[model])

                #-----------------------------------------------------------
                #  Add up the weight for this model
                totalWeight += float(self._modelWeights[model])

                #-----------------------------------------------------------
                #  Add this data to the existing model cubes 

                avgHgtCube += ghCube
                avgRhCube += rhCube

            if totalWeight == 0.0:
                continue
            
            #===================================================================
            #  Compute the weighted-average values we need to compute the
            #  mixing height for this time
#              t2 = time.time()
            
            avgHgtCube /= totalWeight
            avgRhCube /= totalWeight
            
            #  Convert the height cube from meters to feet
            avgHgtCube /= 0.3048

            #  If we need cloud bases in AGL, convert all heights to AGL
            if self.useAglHeights:
               avgHgtCube -= topo

            #  Convert the heights into hundreds of feet
            avgHgtCube /= 100.0

            #-------------------------------------------------------------------
            #  Display these data if debugging
            
            if CreateDebugGrids:
                
                for level in range(avgRhCube.shape[0]):
                    
                    self.createGrid("Fcst", "rh%02d" % (level), "SCALAR", 
                                    avgRhCube[level], timeRange,
                                    minAllowedValue=0.0, maxAllowedValue=100.0)
                    self.createGrid("Fcst", "gh%02d" % (level), "SCALAR", 
                                    avgHgtCube[level], timeRange,
                                    minAllowedValue=-100.0, maxAllowedValue=500.0)

            #===================================================================
            #  Compute the cloud base for each point
            
            mask = (avgRhCube >= self._rhThreshold)

            #  Find the lowest level in the cube where cloud is indicated
            #  Remember, "masked" data in number is "bad".  So we need to
            #  reverse the logic in determining which index to use.
            index = mask.argmax(axis=0) #+ 1

            #  Now collapse the 3-D mask to a 2-D mask
            mask2D = np.any(mask, axis=0)

            #-------------------------------------------------------------------
            #  Display these data if debugging

            if CreateDebugGrids:

                self.createGrid("Fcst", "mask2D", "SCALAR", mask2D, timeRange,
                                minAllowedValue=0.0, maxAllowedValue=1.0)
                self.createGrid("Fcst", "index", "SCALAR", index, timeRange,
                                minAllowedValue=0.0, maxAllowedValue=50.0)

                for level in range(mask.shape[0]):
                    self.createGrid("Fcst", "mask%02d" % (level), "SCALAR", 
                                    mask[level], timeRange,
                                    minAllowedValue=0.0, maxAllowedValue=1.0)
            
    
            #  Now assign the lowest cloud base we found
            cloudBaseHgt[mask2D] = avgHgtCube[index, self.BI_row, self.BI_col][mask2D]

            #-------------------------------------------------------------------
            #  Place these grids in the database

            cloudBaseHgt = cloudBaseHgt.clip(1, 250)
            self.createGrid("Fcst", self._populateHeight, "SCALAR",
                            cloudBaseHgt, tr)
           
        self.GM_logToolUse("%f ms to complete Aviation_CloudBaseFromRH2" %
                           ((time.time() - t0) * 100.0))

        self.statusBarMsg("Aviation_CloudBaseFromRH completed...", "R")

