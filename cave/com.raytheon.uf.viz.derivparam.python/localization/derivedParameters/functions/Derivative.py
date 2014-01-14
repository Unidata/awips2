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

## @file Derivative.py

from numpy.ma.core import masked_greater
from numpy.ma.core import masked_values
from numpy.ma.core import filled
import Vector

##
# Calculate the derivative of A with respect to B.
#
# @param A0: Initial value of A
# @type A0: numpy array
# @param A1: Final value of A
# @type A0: numpy array
# @param B0: Initial value of B
# @type A0: numpy array
# @param B1: Final value of B
# @type A0: numpy array
# @return: The derivative of A with respect to B
# @rtype: numpy array
def execute(A0, A1, B0, B1):
    "Calculate the derivative of A with respect to B."
    
    if isinstance(A0, tuple):
        # Do derivitive of components of the vector
        u0 = A0[2]
        v0 = A0[3]
        u1 = A1[2]
        v1 = A1[3]
        uDeriv = execute(u0, u1, B0, B1)
        vDeriv = execute(v0, v1, B0, B1)
        return Vector.componentsTo(uDeriv, vDeriv)

    Adiff = A1-A0
    Bdiff = B1-B0
    
    result = Adiff / Bdiff
    
    return result
