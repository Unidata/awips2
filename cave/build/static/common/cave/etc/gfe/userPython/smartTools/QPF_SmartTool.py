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
# QPF_SmartTool.py
#  This is an example of a more complicated tool.
#  It determines a QPF value based on the current QPF value,
#  the Wind value, and surrounding Topography 
#  information.
#
#  It has sub-methods that are called by the main method.
#  These sub-methods calculate Vertical Motion and translate
#  it into a QPF term.
#
# Author: wier (translated to Python by hansen)
#         Updated by hansen 1/00 based on suggestions from Rusty Billingsley
#         Updated by njensen for AWIPS-II
# ----------------------------------------------------------------------------
WeatherElementEdited = "QPF"
ToolType = "numeric"

VariableList = [
         ("Vertical Motion Influence" , 50, "scale", [0,100]), 
        ]
    
# This allows us to use the sin and cos functions
from math import *

from numpy import *

AVG_GRID_SPACE = 10000
RAD_TO_DEG = 57.2958

####################
# QPF Smart Tool 
import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, QPF, Wind, Topo, varDict):
        "Sets QPF within Active Edit Area based on Wind and Topo." 

        VerticalMotion_Grid = self.VerticalMotionGrid(Wind, Topo)   
            
        scale = varDict["Vertical Motion Influence"] / 50.0
    
        value =  QPF * (1.0 + scale * VerticalMotion_Grid)
        return value.astype('float32')
    
    def VerticalMotionGrid(self, Wind_Grid, Topo_Grid):
        # Create Vertical Motion grid from Wind Grid and Topography
    
        # The Topo_Grid is a 2-D array where
        # each entry is a scalar elevation, for example:
        #   x = 0
        #   y = 0
        #   elevation = Topo_Grid[x][y]
    
        # The Wind_Grid is 2-D array where
        # each entry is a 2-tuple of magnitude and direction,
        # for example:
        #   wind_tuple = Wind_Grid[x][y]
        #   magnitude = wind_tuple[0]
        #   direction = wind_tuple[1]
    
        # Create a VerticalMotion_Grid that is
        # a 2-D array
    
        xGridSize = len(Topo_Grid)
        yGridSize = len(Topo_Grid[0])
        vmArray = []
    
        first = 1
        for x in range(xGridSize):
            # Add a new column
            vmArray = zeros(Topo_Grid.shape)
            
            for y in range(yGridSize):
                # Calculate the value for this point
                wind_tuple = (Wind_Grid[0][x][y], Wind_Grid[1][x][y])
                vmValue = self.VerticalMotion(wind_tuple,Topo_Grid,x,y)
    
                # Set the value
                vmArray[x][y] = vmValue
                
                # Keep track of min/max values
                if first:
                    first = 0
                    min = vmValue
                    max = vmValue
                else:
                    if vmValue < min:
                        min = vmValue
                    if vmValue > max:
                        max = vmValue
                
        # Now normalize the grid to values between -1 and 1
        factor1 = (max + min) / 2
        factor2 = (max-min) / 2
        for x in range(xGridSize):
            for y in range(yGridSize):
                vmArray[x][y] = (vmArray[x][y] - factor1) / factor2
    
        return vmArray
        
    
    def VerticalMotion(self, Wind, Topo_Grid, x,y):
        # wind is a 2-tuple: wind[0] is magnitude, wind[1] is direction
        magnitude = Wind[0]
        direction = Wind[1]
    
        # Determine wind u and v components.
        # First compute wind vector angle from north, in radians.
        rads = (direction - 180) / RAD_TO_DEG
    
        # u and v components
        # (convert from knots to meters per second 1.94384 knots / m/s )
        uw = sin(rads) * magnitude / 1.94384
        vw = cos(rads) * magnitude / 1.94384
    
        # find slope vector components (svx, svy) at this point (x, y). 
        # Direction is that of maximum slope and magnitude is the 
        # slope = rise/run, unitless.
        svx, svy = self.findSlopeVector(x, y, Topo_Grid)
    
        #  multiply (dot product) wind vector by slope vector
        # to get the value of the vertical air motion.
        vertAirSpeed = uw * svx + vw * svy
    
        return vertAirSpeed
    
    def findSlopeVector(self, x,y, Topo_Grid):
        # the Topo_Grid of the center grid point at x,y.
        # Topo_Grid is a tuple of tuples representing a 2-D grid.
    
        sumxcomp = sumycomp = count = 0
        centerh = Topo_Grid[x][y]
    
        gridSizeX = len(Topo_Grid)
        gridSizeY = len(Topo_Grid[0])
        
        for i in range(x-1, x+2):
            for j in range(y-1, y+2):
                # skip indices beyond limits of grid
                if i < 0 or j < 0 or i >= gridSizeX or j >= gridSizeY:
                    continue
                # components of vector pointing from the center xc,yc
                # to the grid point (i,j)
                xcomp = i-x
                ycomp = j-y
                
                # if at center point; distance is 0, do not compute
                if i == x and j == y:
                    continue 
    
                # distance between pair of grid points
                dist = AVG_GRID_SPACE * sqrt(xcomp*xcomp + ycomp*ycomp)
    
                # error trap to avoid 0 divide; should never occur
                if dist == 0.0:
                    continue
    
                # slope from center to the other grid point; + if up from center
                # (dist and _Topo_Grid values must be in same units)
                slope = (Topo_Grid[i][j] - centerh) / dist
    
                # multiply original components by slope to get the slope vector
                # components from (xc,yc) to (i,j), 
                # and add into summation of all x and y components
                sumxcomp += xcomp * slope
                sumycomp += ycomp * slope
                count += 1
    
        # average all slope vectors to neighbor points
        svx = sumxcomp / count
        svy = sumycomp / count
    
        # ensure "reasonable" values  - less than 45 degrees 
        if abs(svx) > 1.0:
            svx /= abs(svx)
        if abs(svy) > 1.0:
            svy /= abs(svy)
        
        return svx, svy

