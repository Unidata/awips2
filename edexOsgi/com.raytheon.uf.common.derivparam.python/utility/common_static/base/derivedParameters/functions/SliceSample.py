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
##

import gridslice
from numpy import zeros, float32, NaN

gridslice_maskval = 1e37

def execute(*args):
    #createNumpySlice(vc, s3d, targetLevel, sense, hyb-optional)
    # vc - cube/list of data that is the same as targetLevel
    # s3d - cube for output parameter
    # targetLevel - 2d grid of data values to find in vc to interpolate s3d
    # sense - corresponds to vc for vertical coordinate and whether to search for highest/lowest occurence
    
    if len(args) == 4:
        # cube, grid, sense, hybrid
        grid = args[1]
        if isinstance(grid, float):
            grid = zeros((args[0][1].shape[1], args[0][1].shape[2]),float32)
            grid.fill(args[1])
        rval = gridslice.createNumpySlice(args[0][1], args[0][0], grid, int(args[2]), int(args[3]))
        rval[rval == gridslice_maskval] = NaN
        return rval
    else:
        # cube, cube, grid, sense, hybrid
        rval = gridslice.createNumpySlice(args[1][0], args[0][0], args[2], int(args[3]), int(args[4]))
        rval[rval == gridslice_maskval] = NaN
        return rval
        