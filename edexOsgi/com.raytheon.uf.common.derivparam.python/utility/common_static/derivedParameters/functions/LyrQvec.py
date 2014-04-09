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
import DgeoComps as DgeoComps
import Vector

##
# Find Q vectors from upper and lower height and pressure.
#
# @param height_up: Height at each grid point for the top of the layer (m)
# @type height_up: 2D numpy array
# @param height_lo: Height at each grid point for the bottom of the layer (m)
# @type height_lo: 2D numpy array
# @param pressure_up: Pressure level corresponding to height_up (mb)
# @type pressure_up: scalar or 2D numpy array
# @param pressure_lo: Pressure level corresponding to height_lo (mb)
# @type pressure_lo: scalar or 2D numpy array
# @param dx: Spacing between data points in the X direction
# @type dx: scalar or 2D numpy array
# @param dy: Spacing between data points in the Y direction
# @type dy: scalar or 2D numpy array
# @param coriolis: Coriolis force (kg-m/s^2)
# @type coriolis: 2D numpy array
# @return: Q vectors 
# @rtype: tuple(U,V) of 2D numpy arrays
def execute(height_up, height_lo, pressure_up, pressure_lo, dx, dy, coriolis):
    
    qx, qy, dtdx, dtdy = calculate(height_up, height_lo, pressure_up, pressure_lo, dx, dy, coriolis)
    # unmask the arrays we're interested in
    return Vector.componentsTo(qx, qy)

##
# Find Q vectors and dtdx and dtdy from upper and lower height and pressure.
#
# In qvector.f, comments described dtdx and dtdy as work arrays, but
# frontogen.f was treating them as output arrays since they were filled
# with the values frontogen.f needed during qvector.f's processing. This
# method was created to return all 4 arrays, so fGen.py wouldn't have to 
# re-generate dtdx and dtdy.
#
# @attention: Returned vectors may contain NaN.
#
#
# @param height_up: Height at each grid point for the top of the layer (m)
# @type height_up: 2D numpy array
# @param height_lo: Height at each grid point for the bottom of the layer (m)
# @type height_lo: 2D numpy array
# @param pressure_up: Pressure level corresponding to height_up (mb)
# @type pressure_up: scalar or 2D numpy array
# @param pressure_lo: Pressure level corresponding to height_lo (mb)
# @type pressure_lo: scalar or 2D numpy array
# @param dx: Spacing between data points in the X direction
# @type dx: scalar or 2D numpy array
# @param dy: Spacing between data points in the Y direction
# @type dy: scalar or 2D numpy array
# @param coriolis: Coriolis force (kg-m/s^2)
# @type coriolis: 2D numpy array
# @return: Q vectors 
# @rtype: tuple(U,V) of 2D numpy arrays
def calculate(height_up, height_lo, pressure_up, pressure_lo, dx, dy, coriolis):
    "Find Q vectors from upper and lower height and pressure."
    # assume dx, dy, and coriolis don't need masked
    # Acceleration due to gravity (m/s**2)
    gravAcc = 9.806
    
    # magic numbers used in calculations.
    # Taken from original Fortran code; not sure what they are.
    magic_exp = 0.286
    magic_fac = 287
    
    height_mid = height_up + height_lo
    height_mid /= 2
    
    #******* Code to smooth height omitted *******
    
    # calculate components of geostrophic wind
    dugdx, dugdy, dvgdx, dvgdy = DgeoComps.execute(height_mid, dx, dy, coriolis) 

    # get mean pressure
    # copied from Fortran; wouldn't sqrt(p_up * p_lo) be better?
    pavg = log(pressure_up) + log(pressure_lo)
    pavg /= 2
    pavg = exp( pavg )

    # derive cnvFac to convert potential temp to thickness
    pdiff = pressure_up - pressure_lo
    
    gravAcc = 9.806
    
    denom = (pavg/1000) ** magic_exp
    denom *= pdiff * magic_fac
    
    cnvFac = -gravAcc * pavg / denom
    
    temp = cnvFac * (height_up - height_lo)
    
    dtdx, dtdy = Partial.execute(temp, dx, dy)
    
    qx = -dugdx * dtdx
    qx -= dvgdx * dtdy
    
    qy = -dugdy * dtdx
    qy -= dvgdy * dtdy
    
    return (-qx, qy, dtdx, dtdy)