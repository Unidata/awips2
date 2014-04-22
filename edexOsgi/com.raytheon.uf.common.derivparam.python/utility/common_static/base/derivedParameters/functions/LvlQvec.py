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

from numpy import log, exp

import PartialDerivative as Partial
import DgeoComps
import Vector
##
# Find Q vectors from height, temp, and pressure.
#
# @param height: Height (m)
# @type height: 2D numpy array
# @param temp: Temperature (K)
# @type temp: 2D numpy array 
# @param pressure: Pressure (mb)
# @type pressure: scalar or 2D numpy array
# @param dx: Spacing between data points in the X direction
# @type dx: scalar or 2D numpy array
# @param dy: Spacing between data points in the Y direction
# @type dy: scalar or 2D numpy array
# @param coriolis: Coriolis force (kg-m/s^2)
# @type coriolis: 2D numpy array
# @return: Q vectors and dtemp/dx and dtemp/dy 
# @rtype: tuple(U,V, dtdx, dtdy) of 2D masked numpy arrays
def execute(GHxSM, TxSM, P, dx, dy, coriolis):
    result_u, result_v, dtdx, dtdy = calculate(GHxSM, TxSM, P, dx, dy, coriolis)
    # convert the results we want to unmasked arrays
    return Vector.componentsTo(result_u, result_v)
  
##
# Find Q vectors and dtemp/dx and dtemp/dy from height, temp, and pressure.
# This is an adaptation of slqvect.f. That function had 
# multiple calls to a smoothing function, but I didn't find 
# anything to set the number of passes to anything but 0, so
# the smoothing loops have been omitted; comments in the code
# mark where they would have been called.
#
# In slqvect.f, comments described dtdx and dtdy as work arrays, but
# slfront.f was treating them as output arrays since they were filled
# with the values slfront.f needed during slqvect.f's processing. This
# method was created to return all 4 arrays, so fGen.py wouldn't have to 
# re-generate dtdx and dtdy.
#
# @attention: Returned vectors may contain NaN.
#
# @param height: Height (m)
# @type height: 2D numpy array
# @param temp: Temperature (K)
# @type temp: 2D numpy array 
# @param pressure: Pressure (mb)
# @type pressure: scalar or 2D numpy array
# @param dx: Spacing between data points in the X direction
# @type dx: scalar or 2D numpy array
# @param dy: Spacing between data points in the Y direction
# @type dy: scalar or 2D numpy array
# @param coriolis: Coriolis force (kg-m/s^2)
# @type coriolis: 2D numpy array
# @return: Q vectors and dtemp/dx and dtemp/dy 
# @rtype: tuple(U,V, dtdx, dtdy) of 2D masked numpy arrays
def calculate(height, temp, pressure, dx, dy, coriolis):
    "Find Q vectors from height, temp, and pressure."
    # assume dx, dy, and coriolis don't need masked
    
    # Compute the temperature to potential temperature ratio for this level.
    tptRatio = (1000/pressure) ** 0.286
    
    #******* Code to smooth height omitted *******
    
    # Compute the geostrophic wind components for this level.
    dugdx, dugdy, dvgdx, dvgdy = DgeoComps.execute(height, dx, dy, coriolis )
    
    #******* Code to smooth temp omitted *******
    
    # Compute the derivative of temp with respect to X and Y
    dtdx, dtdy = Partial.calculate(temp, dx, dy)
    
    # Calculate the X and Y components of Q.
    result_x = dugdx * dtdx
    result_x += dvgdx * dtdy
    result_x *= tptRatio
    
    result_y = dugdy * dtdx
    result_y += dvgdy * dtdy
    result_y *= -tptRatio
    
    return (result_x, result_y, dtdx, dtdy)