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

## @file CompBy.py
from numpy import cos, sin, sqrt
import Vector

##
# This function is based on the comp_by.f Fortran function by J. Ramer, Jul 1 2003.
# It returns the component of Vecs in the direction of ThGrd, normalized by the
# magnitude of ThGrd, as a two-component vector. ThGrd is rotated by Angle before 
# the component calculation.
#
# comp_by.f performs different calculations based on whether Angle is supplied at all,
# whether it is an integer, and the value of abs(Angle)/1000. This function corresponds
# to the calculations performed in comp_by.f when Angle is an integer and abs(Angle)/1000
# is equal to 2. Unless Angle is 0. ( see comp_by.f )
#
# @param Vecs: Data vector as a tuple of arrays
# @param ThGrd: Potential temperature gradient vector as a tuple of arrays
# @param Angle: Degrees through which to rotate first vector before
#               dotting it with the second vector. May be a scalar or array.
#               Any 1000s component is ignored for compatibility with old code;
#               otherwise, valid values fall in the range [-180..180]. Angles
#               outside the allowed range are treated as an angle of 0.
#
# @return: a tuple of 2 arrays representing the vector components of Vecs in the 
#          direction of ThGrd, normalized by the magnitude of ThGrd.
#

def execute(Vecs, ThGrd, Angle=0):
    ""
    # pi/180 for degrees to radians conversion
    dgtord = 0.01745329252

    Vecs_U, Vecs_V = Vecs
    ThGrd_U, ThGrd_V = ThGrd

    if Angle != 0:
        # Strip off any 1000s from angle.
        # The second part makes sure -2090 ends up as -90 rather than 910.
        # This works with scalars or arrays, where "if angle<0:" wouldn't.
        Angle = Angle % 1000 + (Angle < 0) * -1000
    
        # Angle has to be in the [-180..180] range.
        # If not, convert it to zero.
        Angle = Angle - (Angle > 180) * Angle
        Angle = Angle - (Angle < -180) * Angle
    
        # convert angle in degrees to U and V vector components
        rotate_u = cos( dgtord * Angle )
        rotate_v = sin( dgtord * Angle )
    
        # find the rotated vector
        rotated_u = ThGrd_U * rotate_u + ThGrd_V * rotate_v
        rotated_v = ThGrd_V * rotate_u - ThGrd_U * rotate_v
    
        mag = rotated_u * rotated_u + rotated_v * rotated_v
        
        # we have to divide by the magnitude, so mask away any zero values
        # find the components
        
        zeroMag = (mag == 0)
        mag[zeroMag] = 1
        mag = (rotated_u * Vecs_U + rotated_v * Vecs_V)/mag
        mag[zeroMag] = 0
        
        comp = rotated_u * mag
        comp2 = rotated_v * mag
        
        return Vector.componentsTo(comp, comp2)
    else:
        # initialize to u and v from ThGrd
        rotated_u = ThGrd_U
        rotated_v = ThGrd_V
    
        #calculate the magnitude
        mag = rotated_u * rotated_u + rotated_v * rotated_v
        mag = sqrt(mag)
        
        #magical math that I don't know what it does
        zeroMag = (mag == 0)
        mag[zeroMag] = 1
        mag = (rotated_u * Vecs_U + rotated_v * Vecs_V)/mag
        mag[zeroMag] = 0
        
        return mag