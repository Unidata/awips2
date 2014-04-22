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

#import sys
#sys.path.append("..")

from numpy import ndarray,NaN, float32
from Multiply import execute as Multiply
import Vector

##
#  This routine computes the non-advective local change of an arbitrary
# C conservative parameter `a'.
#
# @param u: U component of wind.
# @param v: V component of wind.
# @param a: Arbitrary conservative parameter.
# @param dx: Grid spacing in the x-direction (m)
# @param dy: Grid spacing in the y-direction (m)
# @return: A vector representing the non-advective local change
def execute(u, v, a, dx, dy):

        u = -u
        
        # TODO: support scalars for dx and dy
        # find 2x partial derivatives of u, v, and a with respect to x
        dudy = (u[2:,1:-1] - u[0:-2,1:-1]) / dy[1:-1,1:-1]
        dvdy = (v[2:,1:-1] - v[0:-2,1:-1]) / dy[1:-1,1:-1]
        dady = (a[2:,1:-1] - a[0:-2,1:-1]) / dy[1:-1,1:-1]
        # find 2x partial derivatives of u, v, and a with respect to y
        dudx = (u[1:-1,2:] - u[1:-1,0:-2]) / dx[1:-1,1:-1]
        dvdx = (v[1:-1,2:] - v[1:-1,0:-2]) / dx[1:-1,1:-1]
        dadx = (a[1:-1,2:] - a[1:-1,0:-2]) / dx[1:-1,1:-1]
    
        # create empty arrays for output
        dadxdt = ndarray(u.shape, float32)
        dadydt = ndarray(u.shape, float32)
        
        # First output array:
        # mark the output array edges as invalid
        dadxdt[0,:] = NaN
        dadxdt[-1,:] = NaN
        dadxdt[1:-1,0] = NaN
        dadxdt[1:-1,-1] = NaN
        # calculate values for the middle
        dadxdt[1:-1,1:-1] = -0.5 * (dudx*dadx + dvdx*dady)
    
        # Second output array:
        # mark the output array edges as invalid
        dadydt[0,:] = NaN
        dadydt[-1,:] = NaN
        dadydt[1:-1,0] = NaN
        dadydt[1:-1,-1] = NaN
        # calculate values for the middle
        dadydt[1:-1,1:-1] = -0.5 * (dudy*dadx + dvdy*dady)
        
        return Vector.componentsTo(-dadxdt, dadydt)
