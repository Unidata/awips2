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


from numpy import shape, empty, hypot, NaN

##
# Calculate the deformation of Vector.
#
# This function is currently only used by Tdef. It was put in the
# functions module to keep it in the same module as other functions
# derived from g2gkinematics.f.
#
# @param U:  U and V must be the same size,
#                at least 2d, and at least 3x3 in the first two dimensions.
# @param V:  U and V must be the same size,
#                at least 2d, and at least 3x3 in the first two dimensions.

# @param dx: The spacing between data points in the X direction (array or scalar)
# @param dy: The spacing between data points in the Y direction (array or scalar)
# @return: The deformation array of Vector.
def execute(U, V, dx, dy):
    
        # create an empty array the same dimensions as U
        result = empty(U.shape, U.dtype)
        
        # flag edges as invalid
        result[0,:] = NaN
        result[-1,:] = NaN
        result[1:-1,0] = NaN
        result[1:-1,-1] = NaN
        
        # strip off edges of dx if it's not a scalar
        shapedx = shape(dx)
        if len(shapedx) < sum(shapedx):
            dx = dx[1:-1,1:-1]

        # strip off edges of dy if it's not a scalar
        shapedy = shape(dy)
        if len(shapedy) < sum(shapedy):
            dy = dy[1:-1,1:-1]
        
        # Calculate deformation vector components.
        qqq = 0.5/dx
        www = 0.5/dy
        dst = (U[1:-1,0:-2] - U[1:-1,2:]) * qqq
        dst += (V[0:-2,1:-1] - V[2:,1:-1]) * www
        dsh = (U[0:-2,1:-1] - U[2:,1:-1]) * qqq
        dsh += (V[1:-1,2:] - V[1:-1,0:-2]) * www
        
        # find vector magnitude of dst and dsh
        ans = hypot(dst, dsh) * 100000.0
        
        # put ans in the valid block of result
        result[1:-1,1:-1] = ans
        return result
     
