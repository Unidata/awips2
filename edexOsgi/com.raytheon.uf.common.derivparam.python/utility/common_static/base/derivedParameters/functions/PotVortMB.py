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
#
# Software History
#
# 2013/1/17    DR 15655    Melissa Porricelli    Modified final 'result' 
#                                                calculation to remove multiplication
#                                                by 0.5.  Displayed values were
#                                                off by a factor of this amount 
#                                                in comparison to A1.  
#                                                A1 calc in pvpres.f.
###

from numpy import zeros
import Gradient
import Vorticity

##
# Calculate the isobaric potential vorticity through a layer.
#
# User Notes:
#
# 1.  Stability is defined as -dP/d(theta). We calculate this through
#     the layer from the isobaric surface 'n' to the surface above it,
#     'n+1'.
# 2.  Since we are dealing with a layer, we calculate a mean absolute
#     vorticity using the winds at the upper and lower layers.
# 3.  The PV is then [mean abs. vort]/[stability] + theta->pres term
#
# originally from pvpres.f, by J.Ramer
# @change: Converted from Fortran on 2008-16-06
#
# @param t_up: Theta on upper isobaric sfc (K)
# @param t:lo: Theta on this isobaric sfc (K)
# @param p_up: Upper pressure (mb)
# @param p_lo: This (lower) pressure (mb)
# @param Wind_up: tuple(U,V) winds on upper surface (m/s)
# @param Wind_lo: tuple(U,V) winds on lower surface (m/s)
# @param dx: Spacing in X direction (m)
# @param dy: Spacing in Y direction (m)
# @param coriolis: Coriolis parameters (/s)
# @return: Isobaric potential vorticity
# @rtype: numpy array
def execute(t_up, t_lo, p_up, p_lo, vector_up, vector_lo, dx, dy, coriolis):
    ""

    u_up, v_up = vector_up
    u_lo, v_lo = vector_lo
    
    # Calculate the absolute vorticity at each isobaric surface.
    avort1 = Vorticity.execute(u_up, v_up, coriolis, dx, dy)
    avort2 = Vorticity.execute(u_lo, v_lo, coriolis, dx, dy)
    
    # Calculate the temperature gradient on each surface.
    grad_lo = Gradient.execute(t_lo, dx, dy)
    grad_up = Gradient.execute(t_up, dx, dy)
    dtdx1, dtdy1 = grad_lo
    dtdx2, dtdy2 = grad_up
    # Calculate difference arrays.
    dp = p_up - p_lo
    dt = t_up - t_lo
    du = u_up - u_lo
    dv = v_up - v_lo
    dtdx = dtdx1 + dtdx2
    dtdy = dtdy1 + dtdy2
    av = avort1 + avort2
    
    result = (-0.5 * (av*dt + (du*dtdy - dv*dtdx)) / dp)
    
    return result
    
