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

#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/12        DR14296        M. Huang      Flipped result by applying -1
# 
#

from numpy import power
from LvlQvec import calculate as lvlQvec

def execute(GHxSM, TxSM, P, dx, dy, coriolis):
    slqx, slqy, dtdx, dtdy  = lvlQvec(GHxSM, TxSM, P, dx, dy, coriolis)
    
    # Compute the temperature to potential temperature ratio for this level.
    t2th = power(1000/P, 0.286)
    t2th *= 2
    
    # Now calculate the QG frontogensis function for this level.
    # Fgen = 2(Qx * d(theta)/dx + Qy * d(theta)/dy)
    result = slqx * dtdx
    result -= slqy * dtdy
    result *= t2th
    
    return result  
