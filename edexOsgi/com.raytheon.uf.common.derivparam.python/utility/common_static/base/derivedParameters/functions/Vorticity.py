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
# Calculate the vorticity of Vector.
#
# @param Vector: 2-tuple of U and V wind component arrays
# @param Q: Coriolis array
# @param dx: physical distance between data points in X direction
# @param dy: physical distance between data points in Y direction
# @return: 2D vorticity array   
#
def execute(vec_U, vec_V, Q, dx, dy):
    """Calculate vorticity of Vector."""
    
    vshape = shape(vec_U)
    
    if len(vshape) < 2:
        raise TypeError, "Vector must be at least 2-D."
    
    if vshape[0] < 3:
        raise ValueError, "Vector's first dimension must be at least 3."
    
    if vshape[1] < 3:
        raise ValueError, "Vector's second dimension must be at least 3."
    
    # create an empty array to hold the result
    rslt = empty(vshape, dtype=vec_U.dtype)
    
    # outer edge is automatically invalid
    rslt[0,:] = NaN
    rslt[-1,:] = NaN
    rslt[1:-1, 0] = NaN
    rslt[1:-1, -1] = NaN

    # find the difference between off-by-1 rows of vec_U
    # RCG - negated the U data.  The data was coming in with the wrong sign. 
    diff_u = -vec_U[0:-2,1:-1] + vec_U[2:,1:-1]
    # find the difference between off-by-1 columns of vec_V
    diff_v = vec_V[1:-1, 2:] - vec_V[1:-1,0:-2]
    
    # allow dx to be a constant, one-element array, or have same shape as vec_V
    dxshape = shape(dx)
    ldxs = len(dxshape)
    if ldxs == 0 or sum(dxshape)==ldxs:
        dvdx = diff_v / dx
    elif dxshape==vshape:
        dvdx = diff_v / dx[1:-1,1:-1]
    else:
        raise TypeError, "dx must be a scalar or the same shape as Vector[1]." 

    # allow dy to be a constant, one-element array, or have same shape as vec_U
    dyshape = shape(dy)
    ldys = len(dyshape)
    if ldys == 0 or sum(dyshape)==ldys:
        dudy = diff_u / dy
    elif dyshape==vshape:
        dudy = diff_u / dy[1:-1,1:-1]
    else:
        raise TypeError, "dy must be a scalar or the same shape as Vector[0]." 

    # get final result; handle Q as a scalar or one-element array
    Qshape = shape(Q)
    lqs = len(Qshape)
    if lqs == 0 or sum(Qshape)==lqs:
        ans = (dvdx + dudy)/2 + Q
    else:
        ans = ( dvdx + dudy )/2 + Q[1:-1,1:-1];

    rslt[1:-1,1:-1] = ans

    return rslt

#Calls Vorticity but instead of blanking the edges, wraps the x parameter around to the other side, useful for worldwide grids.
def executeWrapX(vec_U, vec_V, Q, dx, dy):
    return execute(wrapX(vec_U), wrapX(vec_V), wrapX(Q), wrapX(dx), wrapX(dy))[:,1:-1]

#Utility function
def wrapX(v):
    vshape = shape(v)
    lvs = len(vshape)
    if lvs == 0 or sum(vshape)==lvs:
        return v
    else:
        newV = empty((vshape[0], vshape[1]+2), v.dtype)
        newV[:,1:-1] = v
        newV[:,0] = v[:,-1]
        newV[:,-1] = v[:,0]
        return newV
