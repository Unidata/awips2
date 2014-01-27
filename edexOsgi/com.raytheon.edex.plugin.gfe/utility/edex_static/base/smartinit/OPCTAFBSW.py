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
################################################################################
#  This file contains code to produce OPC wave height databases.
#
#  Author:  Matthew H. Belk                             Created: 07/26/2007
#           WFO BOX                               Last Modified: 07/26/2007
#

from Init import *
class OPCWAVEForecaster(Forecaster):

    def __init__(self):
        Forecaster.__init__(self, "OPCTAFBSW", "OPCTAFBSW")

#--------------------------------------------------------------------------
##  Calculates wave height from the WNAWAVE model.  Converts to feet from meters
##--------------------------------------------------------------------------
    def calcWaveHeight(self, htsgw_SFC, topo):
    
        #  Assign the value filtering out everything above 50 meters
        grid = where(greater(htsgw_SFC, 50), 0.0, htsgw_SFC * 3.28)

        #  Make a mask where topo is 0.0 m MSL
        topoMask = equal(topo, 0.0)
        
        #  Make a mask where data is missing
        dataMask = equal(grid, 0.0)
        
        #  Bring the data all the way into the coast
        grid = self.fillEditArea(grid, logical_and(dataMask, topoMask))
        
        #  Ensure grid values are between 0 ft and 100 ft
        grid = clip(grid, 0, 100)       
        
        #  Return completed grid - but only where topo is 0 ft MSL
        return where(topoMask, grid, 0.0)



################################################################################
##
##  Start utility methods needed for fill routine

##--------------------------------------------------------------------------
## Gets the indicies at o, l
##--------------------------------------------------------------------------
    def getindicies(self, o, l):
        if o > 0:
            a = slice(o, l); b = slice(0, l - o)
        elif o < 0:
            a = slice(0, l + o); b = slice(-o, l)
        else:
            a = slice(0, l); b = slice(0, l)
        return a, b


##--------------------------------------------------------------------------
## Gives an offset grid for array, a, by x and y points
##--------------------------------------------------------------------------
    def offset(self, a, x, y):
        sy1, sy2 = self.getindicies(y, a.shape[0])
        sx1, sx2 = self.getindicies(x, a.shape[1])
        b = zeros_like(a)
        b[sy1,sx1] = a[sy2,sx2]
        return b


##--------------------------------------------------------------------------
## Given a numeric array of 1s and 0s this method returns a similar
## array with cells set that emcompass the mask, unset everwhere else
##--------------------------------------------------------------------------
    def getMaskBorder(self, mask):
        border = zeros(mask.shape)
        for i in [-1, 0, 1]:
            for j in [-1, 0, 1]:
                border = logical_or(border, self.offset(mask, i, j))
        return logical_xor(border, mask)


##--------------------------------------------------------------------------
## Given a numeric array of 1s and 0s this method returns a list of
## the (x, y) indicies that are set to 1.
##--------------------------------------------------------------------------
    def getMaskIndicies(self, mask):
        flatIndicies = flatnonzero(mask)  # get the indicies of the set cells
        ysize = mask.shape[1]
        indexes = []
        # convert the flat indicies to the x, y indicies
        for i in flatIndicies:
            indexes.append((i / ysize, i % ysize))
        return indexes


##--------------------------------------------------------------------------
## Define a method to fill the specified edit area
##--------------------------------------------------------------------------
    def fillEditArea(self, grid, fillMask):
    
        #  Now fill in the rest of the grid
        borderMask = self.getMaskBorder(fillMask)
        editPoints  = self.getMaskIndicies(fillMask)
        borderPoints = self.getMaskIndicies(borderMask)

        for e in editPoints:  # for each point
            numSum = 0.0
            denomSum = 0.0
            for b in borderPoints:
                 if e[0] == b[0] or e[1] == b[1] or \
                    abs(e[0] - b[0]) == abs(e[1] - b[1]):
                    xdist = float(e[0] - b[0])
                    ydist = float(e[1] - b[1])
                    # calculate the distance to the border point
                    dist = sqrt(xdist * xdist + ydist * ydist)
                    value = grid[b[0], b[1]]
                    # Accumulate the distance-weighted average
                    if value > 0:
                        numSum = numSum + value / dist
                        denomSum = denomSum + 1 / dist
            if denomSum > 0:
                grid[e[0], e[1]] = numSum / denomSum
            else:
                grid[e[0], e[1]] = 0.0

        #  Return completed grid
        return grid


##--------------------------------------------------------------------------
## Smooths the grid in the "filled in" area...
##--------------------------------------------------------------------------
    def Smooth(self, grid, fillMask):
        # make a copy of the original grid
        a = grid
        # smooth a 3 times in succession to wipe out any noise
        for x in range(3):
            a = self.smoothGrid(a)
        # in the "filled-in" area, replace the current data with 
        # the smoothed data 
        grid = where(fillMask, a, grid)
        return grid


    def smoothGrid(self, grid):
        # This code is essentially the NumericSmooth example
        # smart tool customized for our purposes.
        # factors of less than 3 are useless or dangerous
        factor = 3
        if factor < 3:
            return grid

        half = int(factor)/ 2
        sg = zeros(grid.shape,Float64)
        count = zeros(grid.shape,Float64)
        gridOfOnes = ones(grid.shape,Float64)
        for y in range(-half, half + 1):
            for x in range(-half, half + 1):
                if y < 0:
                    yTargetSlice = slice(-y, None, None)
                    ySrcSlice = slice(0, y, None)
                if y == 0:
                    yTargetSlice = slice(0, None, None)
                    ySrcSlice = slice(0, None, None)
                if y > 0:
                    yTargetSlice = slice(0, -y, None)
                    ySrcSlice = slice(y, None, None)
                if x < 0:
                    xTargetSlice = slice(-x, None, None)
                    xSrcSlice = slice(0, x, None)
                if x == 0:
                    xTargetSlice = slice(0, None, None)
                    xSrcSlice = slice(0, None, None)
                if x > 0:
                    xTargetSlice = slice(0, -x, None)
                    xSrcSlice = slice(x, None, None)

                target = [yTargetSlice, xTargetSlice]
                src = [ySrcSlice, xSrcSlice]
                sg[target] = sg[target] + grid[src]
                count[target] = count[target] + gridOfOnes[src]
        return sg / count


##
##  End utility methods needed for fill routine
################################################################################


def main():
    OPCWAVEForecaster().run()
