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
from Init import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from GWW model
## output.
##
##--------------------------------------------------------------------------
class GWWForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "GWW", "GWW")

##--------------------------------------------------------------------------
##  Calculates wave height from the GWW model.  Converts to feet from meters
##--------------------------------------------------------------------------
    def calcWaveHeight(self, htsgw_SFC):
        #  Convert meters to feet
        grid = htsgw_SFC * 3.28
        grid[greater_equal(grid, 50.0)] = 0.0
        
        # points where we don't have real wave data
        editAreaMask = equal(grid, 0.0)
        dataMask = greater(htsgw_SFC, 0.0)
        # points where elevation = 0
        topoGrid = self.getTopo()
        topoMask = less_equal(topoGrid, 0.0)

        # points over water where we need more data
        editAreaMask = logical_and(editAreaMask, topoMask)
        borderMask = self.getMaskBorder(editAreaMask)
        borderMask = logical_and(borderMask, dataMask)
        editPoints  = self.getMaskIndicies(editAreaMask)
        borderPoints = self.getMaskIndicies(borderMask)
        for te in editPoints:  # for each point
            numSum = 0.0
            denomSum = 0.0
            for b in borderPoints:
                # points in the same row, column or diagonal
                if (abs(te[0] - b[0]) > 10) or (abs(te[1] - b[1]) > 10):
                    continue
                if te[0] == b[0] or te[1] == b[1] or \
                   abs(te[0] - b[0]) == abs(te[1] - b[1]):
                    xdist = float(te[0] - b[0])
                    ydist = float(te[1] - b[1])
                    # calculate the distance to the border point
                    dist = sqrt(xdist * xdist + ydist * ydist)
                    value = grid[b[0], b[1]]
                    # Accumulate the distance-weighted average
                    if value > 0:
                        numSum = numSum + value / dist
                        denomSum = denomSum + 1 / dist
            if denomSum > 0:
                grid[te[0], te[1]] = numSum / denomSum
            else:
                grid[te[0], te[1]] = 0.0

        # mask off wave data where topo is above zero
        grid[logical_not(topoMask)] = 0.0
        
        # Return the new value
        return grid

##--------------------------------------------------------------------------
##  Calculates wind from the GWW model.  COnvert from meters/sec to knots.
##--------------------------------------------------------------------------
    def calcWind(self, wind_SFC):
        # extract the wind speed and direction
        mag = where(greater(wind_SFC[0], 100), float32(0), wind_SFC[0]*1.94) # convert
        dir = where(greater(wind_SFC[0], 100), float32(0), wind_SFC[1])
        dir = clip(dir, 0, 359.5)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Primary Swell from GWW model.
# Note: have to use wave height for magnitude since that info not available.
##--------------------------------------------------------------------------
    def calcSwell(self, WaveHeight, dirpw_SFC):
        # extract the wind speed and direction
        mag = WaveHeight
        dir = clip(dirpw_SFC, 0, 359.5)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Secondary Swell from GWW model.
# Note: have to use wave height for magnitude since that info not available.
##--------------------------------------------------------------------------
    def calcSwell2(self, WaveHeight, dirsw_SFC):
        # extract the wind speed and direction
        mag = WaveHeight
        dir = clip(dirsw_SFC, 0, 359.5)
        return (mag, dir)

##--------------------------------------------------------------------------
##  Calculates Primary Period from GWW model.
##--------------------------------------------------------------------------
    def calcPeriod(self, perpw_SFC):
        return clip(perpw_SFC, 0, 60)

##--------------------------------------------------------------------------
##  Calculates Secondary Period from GWW model.
##--------------------------------------------------------------------------
    def calcPeriod2(self, persw_SFC):
        return clip(persw_SFC, 0, 60)

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
## array with cells set that encompass the mask, unset everywhere else
##--------------------------------------------------------------------------
    def getMaskBorder(self, mask):
        border = mask.copy()
        for i in [-1, 0, 1]:
            for j in [-1, 0, 1]:
                border[self.offset(mask, i, j)] = True
        return border

##--------------------------------------------------------------------------
## Given a numeric array of 1s and 0s this method returns a list of
## the (x, y) indices that are set to 1.
##--------------------------------------------------------------------------
    def getMaskIndicies(self, mask):
        flatIndicies = flatnonzero(mask)  # get the indicies of the set cells
        ysize = mask.shape[1]
        indexes = []
        # convert the flat indicies to the x, y indicies
        for i in flatIndicies:
            indexes.append((i / ysize, i % ysize))
        return indexes



def main():
    GWWForecaster().run()