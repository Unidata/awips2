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

## @file VorticityAdv.py

from numpy import empty, shape, NaN

##
# Calculate vorticity advection.
# This is currently only used in the calculation of PIVA; it was included here because it
# was originally part of g2gkinematics.f, and is similar to more commonly-used functions
# derived from that file like vorticity and divergence.
#
# @param Vector: A 2-tuple (U,V) of vector components. U and V must have the same shape,
#                be rank 2 or more, and have a shape of 3 or more in the first two dimensions. 
# @param coriolis: The coriolis adjustment of the grid accessor. This must be an array with
#                  the same dimensions as U and V.
# @param dx: The spacing between adjacent data points in the X direction. This can be an array
#            with the same dimensions as U or a scalar.
# @param dy: The spacing between adjacent data points in the Y direction. This can be an array
#            with the same dimensions as U or a scalar.
# @return: The vorticity advection array.
# @rtype: An array of at least rank 2 with a shape of 3 or more in the first two dimensions.
#         The outer edges of the returned cannot be calculated and are set to NaN. 
def execute(U, V, dx, dy, coriolis):
    """Calculate vorticity advection.
Parameters:
    Vector - a tuple(U,V) of arrays, at least 3x3.
    coriolis - coriolis factor: an array the same shape as U
    dx - X dimension spacing: an array the same shape as U or a scalar
    dy - Y dimension spacing: an array the same shape as U or a scalar
Returns:
    Vorticity advection: An array the same shape as U
"""
    # coriolis, dx, and dy are generated values, not data.
    # Assume they never have invalid values to mask away.
    
    # If dx and dy are matrices, we never use the outer edge,
    # so strip it off so we don't have to use slice notation in the math.
    # If they're actually scalars or 1-element matrices, we can't
    # slice them so don't try. 
    shapedx = shape(dx)
    if len(shapedx) < sum(shapedx):
        dx = dx[1:-1,1:-1]
        
    shapedy = shape(dy)
    if len(shapedy) < sum(shapedy):
        dy = dy[1:-1, 1:-1]

    # create an array to hold the result of calculation.
    result = empty(shape(U), dtype=U.dtype)
    result[0,:] = NaN
    result[-1,:] = NaN
    result[1:-1,0] = NaN
    result[1:-1,-1] = NaN
    
    # coriolis horizontal diff * U at pt / dx
    ans = coriolis[1:-1,0:-2] - coriolis[1:-1,2:]
    ans *= U[1:-1,1:-1]
    ans /= dx
    
    # coriolis vertical diff * V at pt / dy
    coriB = coriolis[0:-2,1:-1] - coriolis[2:,1:-1]
    coriB *= V[1:-1,1:-1]
    coriB /= dy
    
    # average coriolis terms
    ans += coriB
    ans /= 2

    # U difference of diagonal neighbors / 4
    term1 = U[2:,2:] + U[0:-2,0:-2]
    term1 -= U[0:-2,2:]
    term1 -= U[2:,0:-2]
    term1 /= 4
    
    # add 2*V at pt - V horizontal neighbors
    term1 += V[1:-1,1:-1] * 2
    term1 -= V[1:-1,2:]
    term1 -= V[1:-1,0:-2]
    
    # multiply by U at pt
    term1 *= U[1:-1,1:-1]
    
    # V difference of diagonal neighbors / 4
    term2 = V[2:,2:] + V[0:-2,0:-2]
    term2 -= V[0:-2,2:]
    term2 -= V[2:,0:-2]
    term2 /= 4
    
    # add 2*U at pt - U vertical neighbors
    term2 += U[1:-1,1:-1] * 2
    term2 -= U[2:,1:-1]
    term2 -= U[0:-2,1:-1]
    
    # multiply by V at pt
    term2 *= V[1:-1,1:-1]
    
    # find diff of terms
    term1 -= term2
    
    # divide by dx * dy
    term1 /= dx * dy
    
    # add to average of coriolis terms
    ans += term1
    result[1:-1,1:-1] = ans
    
    return result 
