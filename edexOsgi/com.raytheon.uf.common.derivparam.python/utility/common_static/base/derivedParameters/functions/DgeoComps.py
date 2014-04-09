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
###

## @file DgeoComps.py

from numpy import isscalar
from numpy import copy as Copy
from numpy import NaN
from numpy import ones
from numpy import shape

##
# Compute the components of geostrophic wind for this level.
# @attention: result may contain NaN.
#
# @param height: Height (m)
# @type height: 2D numpy array, at least 3x3
# @param dx: Spacing between data points in the X direction
# @type dx: scalar or 2D numpy array at least 3x3
# @param dy: Spacing between data points in the Y direction
# @type dy: scalar or 2D numpy array at least 3x3
# @param coriolis: Coriolis force (kg-m/s^2)
# @type coriolis: scalar or 2D numpy array at least 3x3
# @return: d/dx of u component of geostrophic wind
#          d/dy of u component of geostrophic wind
#          d/dx of v component of geostrophic wind
#          d/dy of v component of geostrophic wind
# @rtype: tuple(dugdx, dugdy, dvgdx, dvgdy)
def execute(height, dx, dy, coriolis):
    "Compute the components of geostrophic wind for this level."
    # Generate output arrays as arrays of NaN,
    # the same size and dtype as Height.
    # dugdx = masked_where(True, Height) -- not needed
    dugdy = ones(shape(height), dtype=height.dtype) * NaN 
    dvgdx = Copy(dugdy)
    dvgdy = Copy(dugdy)
    
    gravAcc = 9.806
    
    # use cropped versions of dx, dy, and coriolis in calculations
    if not isscalar(dx):
        dx = dx[1:-1,1:-1]

    if not isscalar(dy):
        dy = dy[1:-1,1:-1]

    if not isscalar(coriolis):
        coriolis = coriolis[1:-1,1:-1]
    
    # intermediate values that are used repeatedly
    qqq = gravAcc / coriolis
    www = height[1:-1,1:-1] * 2.0

    # make calculations in arrays with outer edge cropped
    # then paste into full-sized array
    croppedDvgdy = height[2:,2:] + height[0:-2,0:-2]
    croppedDvgdy -= height[2:,0:-2] + height[0:-2,2:]
    croppedDvgdy *= qqq
    croppedDvgdy /= 4 * dx * dy
    dvgdy[1:-1,1:-1] = croppedDvgdy
    
    dugdx = -dvgdy#*0.0
    
    croppedDugdy = www - height[2:,1:-1] - height[0:-2,1:-1]
    croppedDugdy *= qqq
    croppedDugdy /= dy * dy
    dugdy[1:-1,1:-1] = croppedDugdy
    
    croppedDvgdx = height[1:-1,2:] + height[1:-1,0:-2] - www
    croppedDvgdx *= qqq
    croppedDvgdx /= dx * dx
    dvgdx[1:-1,1:-1] = croppedDvgdx
    
    return (dugdx, dugdy, dvgdx, dvgdy)