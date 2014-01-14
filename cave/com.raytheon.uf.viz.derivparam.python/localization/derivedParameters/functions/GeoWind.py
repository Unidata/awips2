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

## @file Geowind.py

from numpy import isscalar,NaN
import Vector

g = 9.806 # Gravitational acceleration (m/s^2)

##
# Calculate geo wind from height, dx, dy, and coriolis parameter.
#
# @param Height: Height (m)
# @type Height: numpy array
# @param dx: Spacing between data points in X direction (m).
# @type dx: numpy array or scalar
# @param dy: Spacing between data points in Y direction (m).
# @type dy: numpy array or scalar
# @param coriolis: coriolis effect
# @type coriolis: numpy array or scalar
# @return: geological wind
# @rtype: tuple(mag,dir,U,V) of numpy arrays of float
def execute(Height, dx, dy, coriolis):
    ""
    # assume dx, dy, and coriolis are OK

    # Because we're using the adjacent points to calculate the result, we can't
    # find values for points along the edges. Since we never use any points of
    # dx, dy, and coriolis except the middle block, redefine them as the slice
    # we actually use. This also allows us to deal with scalars.
    if not isscalar(dx):
        dx = dx[1:-1, 1:-1]
    
    if not isscalar(dy):
        dy = dy[1:-1, 1:-1]

    if not isscalar(coriolis):
        coriolis = coriolis[1:-1, 1:-1]

    # Create the cropped answer arrays.
    ans_V = Height[1:-1,2:] - Height[1:-1, 0:-2]
    ans_V *= g
    ans_V /= 2 * dy * coriolis
    
    ans_U = Height[2:, 1:-1] - Height[0:-2, 1:-1]
    ans_U *= g
    ans_U /= 2 * dx * coriolis

    # Create full-sized result arrays with all cells masked.
    result_U = Height + NaN
    result_V = Height + NaN
    
    # Paste the cropped arrays into the valid portion of the answer arrays.
    result_U[1:-1, 1:-1] = ans_U
    result_V[1:-1, 1:-1] = ans_V
    
    # Any masked cells become NaN
    # This includes the outer edges and any cells that used a masked
    # value from Height.
    result_U = result_U
    result_V = result_V
    
    result = Vector.componentsTo(result_U, result_V)
    return result
