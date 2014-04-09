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

## @file PartialDerivative.py

from numpy import isscalar
from numpy import NaN

##
# Calculate d/dx and d/dy of Qty.
# This method assumes masked arrays as input and returns masked
# arrays as output. That saves work when the return value would just
# be masked again for use in other calculations. When in doubt, call
# execute() instead.
#
# The returned arrays are the same size as Qty, with all the outermost
# edges masked. d/dx could extend to the top and bottom, and 
# d/dy to the left and right edge, but this has not been done, in order
# to maintain the same output as the old g2gkinematics.f function #7.
#
# @param Qty: Quantity to differentiate.
# @type Qty: 2D numpy array
# @param dx: Spacing between data points in X direction.
# @type dx: scalar or 2D numpy array
# @param dy: Spacing between data points in Y direction.
# @type dy: scalar or 2D numpy array
# @return: d/dx and d/dy of Qty.
# @rtype: tuple(dq/dx,dq/dy) of masked 2D numpy arrays.
def calculate(Qty, dx, dy):
    "Calculate d/dx and d/dy of Qty."
    
    if not isscalar(dx):
        dx = dx[1:-1,1:-1]
        
    if not isscalar(dy):
        dy = dy[1:-1,1:-1]
        
    cropped_dqdx = Qty[1:-1,2:] - Qty[1:-1,0:-2]
    cropped_dqdx /= dx * 2
    
    cropped_dqdy = Qty[2:,1:-1] - Qty[0:-2,1:-1]
    cropped_dqdy /= dy * 2
    
    dqdx = Qty + NaN
    dqdy = Qty + NaN
    
    dqdx[1:-1,1:-1] = cropped_dqdx
    dqdy[1:-1,1:-1] = cropped_dqdy
    
    return (dqdx, dqdy)
    
##
# Calculate d/dx and d/dy of Qty.
# This method takes unmasked arrays and returns unmasked arrays.
#
# @param Qty: Quantity to differentiate.
# @type Qty: 2D numpy array
# @param dx: Spacing between data points in X direction.
# @type dx: scalar or 2D numpy array
# @param dy: Spacing between data points in Y direction.
# @type dy: scalar or 2D numpy array
# @return: d/dx and d/dy of Qty.
# @rtype: tuple(dq/dx,dq/dy) of 2D numpy arrays.
def execute(Qty, dx, dy):
    ""
    
    # assume dx and dy are never zero or near-inifinite
    
    dqdx, dqdy = calculate(Qty, dx, dy)

    return (dqdx, dqdy)