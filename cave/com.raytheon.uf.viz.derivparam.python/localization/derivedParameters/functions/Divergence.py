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

from numpy import empty, isscalar, shape, NaN

##
# Calculate the divergence of Vector.
# Because of the way in which this is calculated, the outermost
# row and column of the output array is always invalid.
#
# @change: quan parm added 2008/06/23 wldougher
#
# @param Vector: 2-tuple (U,V) of arrays of values
# @param dx: 2D array of spacing between grid points
# @param dy: 2D array of spacing between grid points
# @param quan: Quantity parameter (optional; defaults to constant 1)
# @return: divergence 
def execute(vec, dx, dy, quan=1.0):
    """Calculate the divergence of Vector from dx and dy.\n\
    for each output cell,              \n
        u = Vector[0]                  \n
        v = Vector[1]                  \n
        diff_u = u[i+1,j]*q[i+1,j] - u[i-1,j]*q[i-1,j]   \n
        diff_v = u[i,j+1]*q[i,j+1] - u[i,j-1]*q[i,j-1]   \n
        dudx = diff_u/dx[i,j]          \n
        dvdy = diff_v/dy[i,j]          \n
        diverg[i,j] = (dudx + dvdy)/2  \n 
        """
    vec_U,vec_V = vec
    
    vshape = shape(vec_U)
    
    if len(vshape) < 2:
        raise TypeError, "Divergence: Vector must be at least 2-D."
    
    if vshape[0] < 3:
        raise ValueError, "Divergence: Vector's first dimension must be at least 3."
    
    if vshape[1] < 3:
        raise ValueError, "Divergence: Vector's second dimension must be at least 3."
    
    rslt = empty(vshape, dtype=vec_U.dtype)
    
    rslt[0,:] = NaN
    rslt[-1,:] = NaN
    rslt[1:-1, 0] = NaN
    rslt[1:-1, -1] = NaN
    
    if shape(quan) == ():
        quan_rl = quan
        quan_tb = quan
    else:
        quan_rl = quan[1:-1]
        quan_tb = quan[:,1:-1]

    QU = quan_rl * vec_U[1:-1,:]
    QV = quan_tb * vec_V[:,1:-1]
    diff_U = QU[:,2:] - QU[:,0:-2]
    diff_V = QV[0:-2,:] - QV[2:,:]
    
    # if dx is a constant or one-element array, just divide by it.
    # otherwise, divide each cell in diff_U by the corresponding cell in dx.
    dxshape = shape(dx)
    ldxs = len(dxshape)
    if ldxs == 0 or sum(dxshape)==ldxs:
        dudx = diff_U/dx
    elif dxshape==vshape:
        dudx = diff_U/dx[1:-1,1:-1]
    else:
        raise TypeError, "Divergence: dx must be a scalar or the same shape as Vector."

    # if dy is a constant or one-element array, just divide by it.
    # otherwise, divide each cell in diff_V by the corresponding cell in dy.
    dyshape = shape(dy)
    ldys = len(dyshape)
    if ldys == 0 or sum(dyshape)==ldys:
        dvdy = diff_V/dy
    elif dyshape==vshape:
        dvdy = diff_V/dy[1:-1,1:-1]
    else:
        raise TypeError, "Divergence: dy must be a scalar or the same shape as Vector."
        
    rslt[1:-1,1:-1] = (dudx + dvdy)/2
    
    return rslt
