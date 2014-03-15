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

## @file Gradient.py
from numpy import empty, shape, NaN
import Vector

##
# Calculate the X and Y gradient arrays of a 2+D array of scalars.
#
# @param scalar: Array of scalar values
# @param dx: Spacing between data points in X direction.
# @param dy: Spacing between data points in Y direction.
# @return: gradient of scalar
# @rtype: tuple(u, v)
def execute(scalar, dx, dy):
    "Calculate the 2D gradient arrays of a 2+D scalar array."       
    result_u = empty(shape(scalar), dtype=scalar.dtype)
    result_v = empty(shape(scalar), dtype=scalar.dtype)
       
    # Left/rt edges of result_u can't be calculated.
    result_u[:,0] = NaN
    result_u[:,-1] = NaN
    
    # Top/bot edges of result_v can't be calculated.
    result_v[0,:] = NaN
    result_v[-1,:] = NaN
    
    # If dx and dy are arrays, remove extra cells.
    shapedx = shape(dx)
    if len(shapedx) < sum(shapedx):
        dx = dx[:,1:-1]
        
    shapedy = shape(dy)
    if len(shapedy) < sum(shapedy):
        dy = dy[1:-1,:]

    # assume dx and dy are never zero
    # dx = masked_values(dx, 0.0, copy=False)
    # dy = masked_values(dy, 0.0, copy=False)
    
    # calculate d(scalar)/dx
    ans_u = scalar[:,2:] - scalar[:,0:-2]
    ans_u = ans_u/(2 * dx)
    
    # calculate d(scalar)/dy
    ans_v = scalar[0:-2,:] - scalar[2:,:]
    ans_v = ans_v/(2 * dy)
    
    result_u[:,1:-1] = ans_u
    result_v[1:-1,:] = ans_v
    return Vector.componentsTo(result_u, result_v)

