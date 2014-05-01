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


from numpy import empty, shape, NaN

##
# Calculate the advection of quantity by Vector.
#
# @param Vector: A 2-tuple (U,V) of vector components. U and V must have the same shape,
#                be rank 2 or more, and have a shape of 3 or more in the first two dimensions. 
# @param quantity: A concentration array. This must be an array with the same dimensions as U 
#                  and V.
# @param dx: The spacing between adjacent data points in the X direction. This can be an array
#            with the same dimensions as U or a scalar.
# @param dy: The spacing between adjacent data points in the Y direction. This can be an array
#            with the same dimensions as U or a scalar.
# @return: The advection array.
# @rtype: An array of at least rank 2 with a shape of 3 or more in the first two dimensions.
#         The outer edges of the returned cannot be calculated and are set to NaN. 

def execute(U, V, quantity, dx, dy):
    """Calculate the advection of Vector.
Parameters:
    Vector - tuple(U,V) of arrays, at least 3x3
    quantity - quantity to advect: an array the same shape as U
    dx - X dimension spacing: an array the same shape as U or a scalar
    dy - Y dimension spacing: an array the same shape as U or a scalar
Returns:
    Advection: An array the same shape as U
"""
    
    result = empty(shape(U), dtype=U.dtype)
    result[0,:] = NaN
    result[-1,:] = NaN
    result[1:-1,0] = NaN
    result[1:-1,-1] = NaN
    
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

    # create a partial answer from horizontal neighbors, U, and dx
    ans = quantity[1:-1,0:-2] - quantity[1:-1,2:]
    # U data is coming in negated
    ans *= -U[1:-1,1:-1]
    ans /= dx
    
    # create another partial answer from vertical neighbors, V, and dy
    term = quantity[0:-2,1:-1] - quantity[2:,1:-1]
    term *= V[1:-1,1:-1]
    term /= dy
    
    # average the two partial results
    ans += term
    ans /= 2
    
    # put ans in the middle block of result
    # Answer is reversed
    result[1:-1,1:-1] = -ans
    return result
