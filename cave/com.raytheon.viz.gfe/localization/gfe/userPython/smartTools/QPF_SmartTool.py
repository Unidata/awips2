# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# QPF_SmartTool.py
#
# ----------------------------------------------------------------------------


ToolType = "numeric"
WeatherElementEdited = "QPF"
from numpy import *
import MetLib, time

HideTool = 0

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#

VariableList = [
         ("Vertical Motion Influence" , 50, "scale", [0,100]),
        ]

# Set up Class
import SmartScript
# For available commands, see SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Smooths the specified grid by the specified factor
    # With factor == 3, 3x3 smooth, factor == 5 5x5 smooth, etc.
    # Even factors (4, 6, 8,...) round up to the next odd value
    # If factors <3 are specified, the unmodified grid is returned.
    def smoothGrid(self, grid, factor):
        # factors of less than 3 are useless or dangerous
        if factor < 3:
            return grid
        st = time.time()
        half = int(factor)/ 2
        sg = zeros(grid.shape,float64)
        count = zeros(grid.shape,float64)
        gridOfOnes = ones(grid.shape,float64)
        for y in xrange(-half, half + 1):
            for x in xrange(-half, half + 1):
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
                sg[target] += grid[src]
                count[target] += gridOfOnes[src]
        return sg / count

    # Required Method: Execute
    #  %comment
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...
    def execute(self, QPF, Wind, varDict):

        # get the scale value
        scale = float(varDict["Vertical Motion Influence"]) / 50.0

        # Calculate the gridient of the topoGrid
        topoGrid = self.getTopo()

        d_dx, d_dy = MetLib.gradient(topoGrid)

        # Convert wind to u and v components
        u, v = self.MagDirToUV(Wind[0], Wind[1])

        # Calculate the dot product which is positive when wind blows
        # upslope and negative when it blows downslope
        dotGrid = MetLib.dot((d_dx, d_dy), (u, -v)) / 5000.0
        dotGrid = self.smoothGrid(dotGrid, 9)

        # adjust the existing QPF grid using the scale and dot product
        QPF = QPF * (1 + scale * dotGrid)

        return QPF
