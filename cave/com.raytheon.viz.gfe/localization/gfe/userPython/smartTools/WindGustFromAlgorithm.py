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
# New_WindGust_Tool
#
# Authors: Tom Mazza            NWS Charleston, WV          Created: 04/25/03
#          Matthew H. Belk      NWS Taunton, MA       Last Modified: 06/16/03
#          Mathewson            FSL                   Modified: 3/30/04
#             -change in model names to OB3 names
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "WindGust"
from numpy import *
# without this, the builtin max() is used
from numpy import max
import LogStream

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:

#ScreenList = ["MixHgt","WindGust", "TransWind"]

# Set up variables to be solicited from the user:
VariableList = [
         ("Momentum algorithm:", "RUC", "radio", ["RUC", "Power"]),
         ("Use BL Winds:", "No", "radio", ["Yes", "No"]),
         ("Model:", "NAM12", "radio",
                    ["GFS80", "NAM12", "gfsLR", "NGM80", "RUC80"])
]


#Set up Class
import SmartScript
## For available commands, see SmartScript

toolName = 'WindGustFromAlgorithm'

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
    # Define your site ID
        self._SITEID = "BOX"

    # Required Method: Execute
    #  Called once for each grid
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, Wind, MixHgt, Topo, GridTimeRange):
        "Determines WindGust using one of two algorithms, one from the RUC or a power relationship.  This tool assumes your mixing height has already been adjusted for your surface temperatures."

        sounding =  self.makeNumericSounding(self._model, "wind",
                         self._modelCube, GridTimeRange,
                                             noDataError=0)
        
        ########################################################################
        #  If we don't have a model sounding at this point in time, or the
        #  size of the grids do not match
        if sounding is None:        # or sounding[0].shape != Topo.shape:
            LogStream.logProblem(toolName, ': cannot obtain a Wind sounding')
            return None              # leaves current WindGust grid alone
                    
        ########################################################################
        #  If we made it this far, split up the sounding into its component
        #  cubes of height and wind
        (gh_Cube, wind_Cube) = sounding

        if gh_Cube is None:
            LogStream.logProblem(toolName, 'gh_Cube is None')
            return None
        
        if wind_Cube is None:
            LogStream.logProblem(toolName, 'wind_Cube is None')
            return None
        
        ########################################################################
        #  Convert topography from feet to meters
        self._topo =  self.ftToM(Topo)

        ########################################################################
        #  Initialize a cube to hold BL wind grids
        bl_WindCube = {}

        ########################################################################
        #  Cycle through all the BL levels we have for this model
        for lvl in self._blCube:

            ####################################################################
            #  Initialize BL wind grid for this level
            grid = None
            
            ####################################################################
            #  If this is the NAM40/20 model
            if self._model.find('NAM40') != -1:

                ################################################################
                #  Get BL winds from other NAM40/NAM20 file
                tempModel = self._model.replace('NAM40', 'NAM20')

                ################################################################
                #  Try to get model BL winds for this time
                grid = self.getGrids(tempModel, "wind", lvl, GridTimeRange,
                                     noDataError=0)

            ####################################################################
            #  Otherwise
            else:
                
                ################################################################
                #  Try to get model BL winds for this time
                grid = self.getGrids(self._model, "Wind", lvl, GridTimeRange,
                                 noDataError=0)

            ####################################################################
            #  Add this grid to the BL wind cube - if it is valid
            if grid != None:

                ################################################################
                #  Store the wind speeds at this BL level
                bl_WindCube[lvl] = grid[0]

            ####################################################################
            #  Otherwise
            else:

                ################################################################
                #  Store a placeholder
                bl_WindCube[lvl] = None

        ########################################################################
        #  Convert mixing height from ft ASL to m ASL
        mixHgt_m = self.ftToM(MixHgt)

        ########################################################################
        #  Make a 3D mask where the model sounding level is ABOVE the ground,
        #  but below the Mixing Height
        self._mixedLayer = (gh_Cube >= self._topo) & (gh_Cube <= mixHgt_m)
        ########################################################################
        #  Method to compute WindGust using a version of the RUC technique
        #  adapted by Matthew H. Belk (BOX).

        ########################################################################
        #  Initialize WindGust using current 10m Wind speeds - (mag, dir)
        WindGust = Wind[0]

        ########################################################################
        #  Move vertically through the model BL cube
        for lvl in self._blCube:

            ####################################################################
            #  Make a mask where this BL surface is at or below the MixHgt
            blMask = MixHgt <= self._blHgt[lvl]
            
            ####################################################################
            #  If there are any points in the mixed layer at this surface, and
            #  there actually is a wind grid
            if any(blMask) and bl_WindCube[lvl] != None:

                ################################################################
                #  Get wind magnitude at current level - remember model winds
                #  are in m/s and need to be in kts for comparison
                curMag = self.mpsToKt(bl_WindCube[lvl])

                ################################################################
                #  Compute difference between wind at this level and SFC wind
                #  where points are in the mixed layer
                deltaSpd = curMag - Wind[0]

                ################################################################
                #  Get the depth of the mixed layer to this point (m AGL)
                deltaZ = self._blHgt[lvl]

                ################################################################
                #  Adjust change in wind speed by a coefficient - using the
                #  lesser of 0.5 or (deltaZ / 2000)
                #  First get the factor, which will range from 0.5 to 1.0,
                #  higher closer to the ground
                delta = max(1.0 - deltaZ/2000.0, 0.5)

                ################################################################
                #  Employ the power relationship if selected: it focuses in on
                #  how much lower than one this factor will be (it ranges from
                #  no less than 1 just above the surface to 0.5 lower than 1
                #  1000 or more feet from the surface).  The power relationship
                #  takes this small number (between 0 and 0.5) to the second
                #  power, which makes it smaller still.  It actually first
                #  doubles it, then squares it, then halves it again.  This
                #  causes a difference of 0 to stay 0, a difference of 0.5 to
                #  stay at 0.5, but a difference of 0.25 will become 0.125.
                #  This difference is then subtracted from one, to get a new,
                #  equal or larger factor by which to multiply the potential
                #  wind gust, to arrive at a gust potential that decreases more
                #  slowly at first with height, then more rapidly later on, to
                #  arrive at the same factor up at 1000 m and more above the
                #  surface.  The resulting wind gust is always equal to or
                #  greater than using the RUC algorthm straight up.
                
                if self._algorithm == 'Power':
                    delta = 1 - (pow((2 * (1 - delta)), 2)) / 2

                ################################################################
                #  Adjust wind speed difference by chosen coefficient
                deltaSpd *= delta

                gustV = Wind[0] + deltaSpd
                ################################################################
                #  Make a mask where this WindGust is > current WindGust
                newGust = gustV > WindGust

                ################################################################
                #  Assign new WindGust where new WindGust is greater and the
                #  surface is still within the mixed layer
                WindGustMask = newGust & blMask
                WindGust[WindGustMask] = gustV[WindGustMask]

        ########################################################################
        #  Move vertically through the model cube
        for i in xrange(gh_Cube.shape[0]):

            ####################################################################
            #  If there are any points in the mixed layer at this surface
            if any(self._mixedLayer[i]):

                ################################################################
                #  Get wind magnitude at current level - remember model winds
                #  are in m/s and need to be in kts for comparison
                curMag = self.mpsToKt(wind_Cube[0][i])

                ################################################################
                #  Compute difference between wind at this level and SFC wind
                #  where points are in the mixed layer
                deltaSpd = curMag - Wind[0]

                ################################################################
                #  Get the depth of the mixed layer to this point (m AGL)
                deltaZ = gh_Cube[i] - self._topo

                ################################################################
                #  Adjust change in wind speed by a coefficient - using the
                #  lesser of 0.5 or (deltaZ / 2000)
                #  First get the factor, which will range from 0.5 to 1.0,
                #  higher closer to the ground
                delta = max(1.0-deltaZ/2000.0,0.5)

                ################################################################
                #  Employ the power relationship if selected: it focuses in on
                #  how much lower than one this factor will be (it ranges from
                #  no less than 1 just above the surface to 0.5 lower than 1
                #  1000 or more feet from the surface).  The power relationship
                #  takes this small number (between 0 and 0.5) to the second
                #  power, which makes it smaller still.  It actually first
                #  doubles it, then squares it, then halves it again.  This
                #  causes a difference of 0 to stay 0, a difference of 0.5 to
                #  stay at 0.5, but a difference of 0.25 will become 0.125.
                #  This difference is then subtracted from one, to get a new,
                #  equal or larger factor by which to multiply the potential
                #  wind gust, to arrive at a gust potential that decreases more
                #  slowly at first with height, then more rapidly later on, to
                #  arrive at the same factor up at 1000 feet and more above the
                #  surface.  The resulting wind gust is always equal to or
                #  greater than using the RUC algorthm straight up.
                
                if self._algorithm == 'Power':
                    delta = 1 - (pow((2 * (1 - delta)), 2)) / 2

                ################################################################
                #  Adjust wind speed difference by chosen coefficient
                deltaSpd *= delta

                gustV = Wind[0] + deltaSpd
                ################################################################
                #  Make a mask where this WindGust is > current WindGust
                newGust = gustV > WindGust

                ################################################################
                #  Assign new WindGust where new WindGust is greater and the
                #  surface is still within the mixed layer
                WindGustMask = newGust & self._mixedLayer[i]
                WindGust[WindGustMask] = gustV[WindGustMask]

        ########################################################################
        #  Return the computed WindGust
        return WindGust
    


    # Optional Methods
        # These methods can have the additional argument:
        # ToolTimeRange -- selected time range over which we are running the tool
        
    def preProcessTool(self, varDict):
        # Called once at beginning of Tool
        # Cannot have WeatherElement or Grid arguments

        ########################################################################
        #  Get site ID
        try:
            siteID=self.mutableID().siteID()
        except:
            siteID=self._SITEID

        ########################################################################
        #  Get name of chosen model - and fix it up so we can use it later on.
        #  This will grab the latest version of the chosen model from the D2D
        #  netCDF files.
        self._model = "%s_D2D_%s" % (siteID, varDict["Model:"])

        ########################################################################
        #  Get chosen algorithm
        self._algorithm = varDict["Momentum algorithm:"]
        
        ########################################################################
        #  Get answer if we should use BL winds
        useBLwinds = varDict["Use BL Winds:"]
        
        ########################################################################
        #  Initialize a list of model levels
        self._modelCube = []

        ########################################################################
        #  Determine model levels available for each model
        if self._model.find( 'GFS80') != -1 or \
           self._model.find( 'GFS') != -1:
            self._modelCube = ["MB850", "MB700", "MB500", "MB400", "MB300"]
            self._blCube = []

        elif self._model.find( 'NAM12') != -1:
            self._modelCube = ["MB1000", "MB950", "MB900", "MB850", "MB800",
                               "MB750", "MB700", "MB650", "MB600", "MB550",
                               "MB500", "MB450", "MB400", "MB350"]
            self._blCube = ["BL030", "BL03060", "BL6090", "BL90120", "BL12015"]

        elif self._model.find( 'NAM40') != -1 or \
             self._model.find( 'NAM20') != -1:
            self._modelCube = ["MB975", "MB950", "MB925", "MB900", "MB875",
                               "MB850", "MB825", "MB800", "MB775", "MB750",
                               "MB725", "MB700", "MB675", "MB650", "MB625",
                               "MB600", "MB550", "MB500", "MB450", "MB400",
                               "MB350", "MB300"]
            self._blCube = ["BL030", "BL03060", "BL6090", "BL90120", "BL120150"]

        elif self._model.find( 'gfsLR') != -1:
            self._modelCube = ["MB1000", "MB850", "MB700", "MB500", "MB300"]
            self._blCube = []

        elif self._model.find( 'NGM80') != -1:
            self._modelCube = ["MB1000", "MB950", "MB850", "MB700", "MB500",
                               "MB400", "MB300"]
            self._blCube = ["FH1829", "FH2743", "FH3658"]

        elif self._model.find( 'RUC80') != -1:
            self._modelCube = ["MB1000", "MB950", "MB900", "MB850", "MB800",
                               "MB750", "MB700", "MB650", "MB600", "MB550",
                               "MB500", "MB450", "MB400", "MB350", "MB300"]
            self._blCube = ["BL030", "BL6090", "BL15018"]

        ########################################################################
        #  If we should not use the BL winds
        if useBLwinds is 'No':

            ####################################################################
            #  Reset the levels in the BL cube so we don't do anything
            self._blCube = []

        ########################################################################
        #  Determine height of all possible BL levels available for each model.
        #  If level is not at a fixed height AGL, use the hydrostatic equation.
        #  Assume the density of the air is 1 kg/m3 and gravity is 9.80 m/s^2.
        #  The height will be in m AGL at the center of the layer.  Remember
        #  there are 100 Pa per 1 mb.
        self._blHgt = {'BL030'   : (15.0 * 100.0/ 9.8),
                       'BL3060'  : (45.0 * 100.0 / 9.8),
                       'BL03060'  : (45.0 * 100.0 / 9.8),
                       'BL6090'  : (75.0 * 100.0 / 9.8),
                       'BL90120' : (105.0 * 100.0 / 9.8),
                       'BL12015' : (135.0 * 100.0 / 9.8),
                       'BL120150': (135.0 * 100.0 / 9.8),
                       'BL15018' : (165.0 * 100.0 / 9.8),
                       'FH1829'  : 1829.0,
                       'FH2743'  : 2743.0,
                       'FH3658'  : 3658.0
                      }
        
        LogStream.logDebug(toolName, ': preProcessTool complete.')
