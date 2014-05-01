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
###  MetLib - a library of meteorological methods for GFE
###  
###  This module contains a variety of python methods intended
###  for making meteorological calculations on gridded data.


from numpy import *
import copy

# CenteredDifference - This method performs a centered difference
# of the specificd grid.  Edges are calculated using a forward or
# backward difference so that the grid that is returned is the same
# size as the input grids.  In general this is a low-level method
# intended to be used by the derivative methods d_dx, d_dy, d_dz and
# d_dt.
#
# Note that this method returns the simple difference along one axis.
# Scaling to the correct grid size is the responsibility of the caller.
def centeredDifference(grid, axis):
    ## Make sure we have enough dimensions as the axis
    if axis >= 0 and axis >= len(grid.shape):
        print "Returning None: axis = ", axis, "grid.shape=", grid.shape
        return None
    elif axis < 0 and abs(axis) > len(grid.shape):
        print "Returning None: axis = ", axis, "grid.shape=", grid.shape
        return None
    # Make a slice list of the appropriate length
    sliceList= []
    for s in grid.shape:
        sliceList.append(slice(None, None, None))

    # Define the slices at the specified axis.  Terms labelled with
    # "1" refer to the middle of the grid, terms with 2 the first edge
    # of the grid and terms using "3' the last edge of the grid
    t1 = copy.copy(sliceList)
    t1[axis] = slice(1, -1, None)
    a1 = copy.copy(sliceList)
    a1[axis] = slice(2, None, None)
    b1 = copy.copy(sliceList)
    b1[axis] = slice(0, -2, None)
    t2 = copy.copy(sliceList)
    t2[axis] = slice(0, 1, None)
    a2 = copy.copy(sliceList)
    a2[axis] = slice(1, 2, None)
    b2 = copy.copy(sliceList)
    b2[axis] = t2[axis]
    t3 = copy.copy(sliceList)
    t3[axis] = slice(-1, None, None)
    a3 = copy.copy(sliceList)
    a3[axis] = t3[axis]
    b3 = copy.copy(sliceList)
    b3[axis] = slice(-2, -1, None)

    diff = zeros(grid.shape, float64)
    # Perform the centered difference
    diff[t1] = (grid[a1] - grid[b1]) / 2.0  # middle
    diff[t2] = grid[a2] - grid[b2]  # first edge
    diff[t3] = grid[a3] - grid[b3]  # last edge
    return diff

# Returns the forward difference derivative
def forwardDifference(grid, axis):
    ## Make sure we have enough dimensions as the axis
    if axis >= 0 and axis >= len(grid.shape):
        print "Returning None: axis = ", axis, "grid.shape=", grid.shape
        return None
    elif axis < 0 and abs(axis) > len(grid.shape):
        print "Returning None: axis = ", axis, "grid.shape=", grid.shape
        return None

    # make a list of "None' slices from which we will copy
    sliceList= []
    for s in grid.shape:
        sliceList.append(slice(None, None, None))
    
    a = copy.copy(sliceList)   # forward cell
    a[axis] = slice(1, None, None)
    
    b = copy.copy(sliceList)  # center cell
    b[axis] = slice(0, -1, None)

    t1 = copy.copy(sliceList)     # main grid target
    t1[axis] = slice(0, -1, None)

    t2 = copy.copy(sliceList)
    t2[axis] = slice(-1, None, None)  # last edge

    t3 = copy.copy(sliceList)   # second-to-last edge
    t3[axis] = slice(-2, -1, None)
    
    diff = zeros(grid.shape, float64)

    diff[t1] = grid[a] - grid[b]
    diff[t2] = diff[t3]  # copy second-to-last into last edge

    return diff

# Returns a backward difference derivative
def backwardDifference(grid, axis):
    ## Make sure we have enough dimensions as the axis
    if axis >= 0 and axis >= len(grid.shape):
        print "Returning None: axis = ", axis, "grid.shape=", grid.shape
        return None
    elif axis < 0 and abs(axis) > len(grid.shape):
        print "Returning None: axis = ", axis, "grid.shape=", grid.shape
        return None

    # make a list of "None' slices from which we will copy
    sliceList= []
    for s in grid.shape:
        sliceList.append(slice(None, None, None))
    
    a = copy.copy(sliceList)   # center cell
    a[axis] = slice(1, None, None)
    
    b = copy.copy(sliceList)  # backward cell
    b[axis] = slice(0, -1, None)

    t1 = copy.copy(sliceList)     # main grid target
    t1[axis] = slice(1, None, None)

    t2 = copy.copy(sliceList)
    t2[axis] = slice(0, 1, None)  # first edge

    t3 = copy.copy(sliceList)   # second edge
    t3[axis] = slice(1, 2, None)
    
    diff = zeros(grid.shape, float64)

    diff[t1] = grid[a] - grid[b]
    diff[t2] = diff[t3]  # copy second-to-last into last edge

    return diff

# Returns the derivative along the innermost axis.  By convention
# this is the x-axis.
def d_dx(grid):
    return centeredDifference(grid, -1)

# Returns the derivative along the second innermost axis.  By convention
# this is the y-axis.
def d_dy(grid):
    return -centeredDifference(grid, -2)

# Returns the derivative along the third innermost axis.  By convention
# this is the z-axis.  If a 2-dimensional grid is specified, an error
# will be returned from centeredDifference
def d_dz(grid):
    return centeredDifference(grid, -3)

# Returns the derivative along the outermost axis.  By convention
# this is the time-axis.  If a grid of less than 4 dimensions is
# specified, the centered difference method will report an error.
def d_dt(grid):
    return centeredDifference(grid, 0)

# Returns the dot product of the specified vectors.  Both vector grids
# are assumed to be specified in u, v components.
def dot(vectorGrid1, vectorGrid2):
    return vectorGrid1[0] * vectorGrid2[0] + vectorGrid1[1] * vectorGrid2[1]

# Returns the vector gradient of the specified scalar grid.
def gradient(grid):
    return (d_dx(grid), d_dy(grid))

# Returns the divergence of the specified Wind grid.  Wind is assumed
# to be a vector grid specified in u, v components.
def divergence(Wind):
    u, v = Wind
    return d_dx(u) + d_dy(v)

# Returns the vorticity of the specified Wind grid.  Wind is assumed
# to be a vector grid specified in u, v components.
def vorticity(Wind):
    u, v = Wind
    return d_dx(v) - d_dy(u)

# Returns the advection of the scalarGrid by the windGrid.  The
# windGrid is assumed to be a vector specified in u, v components.
def advection(windGrid, scalarGrid):
    u, v = windGrid
    return -dot(windGrid, gradient(scalarGrid))


### Utility methods

# Utility methods that uses the specified gridLocation 
# to generate and return a grid of latitude and a grid of longitude at
# each grid point.  The gridLoc can be obtained with a call to self.getGridLoc()
# in any SmartTool.
def getLatLonGrids(gridLoc):
    # Fetch the grids
    latLonGrid = gridLoc.getLatLonGrid().__numpy__[0];
    latLonGrid = reshape(latLonGrid, (2,gridLoc.getNy().intValue(),gridLoc.getNx().intValue()), order='F')
    return latLonGrid[1], latLonGrid[0]

# Returns a grid of gridSpacing or the distance from one grid cell to
# another in meters.  This scalar representation of grid spacing works
# well for conformal projections only.  Other projections should use a
# vector grid spacing or a different grid for x grid spacing and y
# grid spacing.
def makeSpacingGrid(gridLoc):
    DEG_TO_RAD = 0.017453292
    latGrid, lonGrid = getLatLonGrids(gridLoc)

    # x and y grid spacing must be calculated using the same direction
    # for both.
    deltaLon = centeredDifference(lonGrid, axis = -2)
    deltaLat = d_dy(latGrid)

    dxGrid = abs(cos(latGrid * DEG_TO_RAD) * deltaLon * 111111)
    dyGrid = deltaLat * 111111 # meters per degree

    # calc the total grid spacing using square root of the sum of the squares.
    spacing = sqrt(dxGrid * dxGrid + dyGrid * dyGrid)

    return spacing

# Returns a grid of coriolis acceleration based purely on latitude.
def makeCoriolisGrid(latGrid):
    DEG_TO_RAD = 0.017453292
    latGrid = latGrid * DEG_TO_RAD
    f = 2.0 * 0.00007292 * sin(latGrid)
    return f
