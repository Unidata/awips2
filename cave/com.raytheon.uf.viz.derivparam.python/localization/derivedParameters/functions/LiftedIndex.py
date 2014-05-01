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


from numpy import copy
from numpy import exp
from numpy import isnan
from numpy import log
from numpy import maximum
from numpy import minimum
from numpy import shape
from numpy import sqrt
from numpy import NaN
from numpy import ndarray
import AdiabaticTemp
import TempOfTe

lowKTemp = 193

# Constants used in calculations.
# Wish the Fortran documented what these are.
const1 = 22.05565
const2 = 0.0091379024
const3 = 6106.396
const4 = 26.66082
const5 = 223.1986
const6 = 0.0182758048
const7 = -0.37329638
const8 = 41.178204
const9 = 0.0015945203
const10 = 3.498257
const11 = 0.286

import numpy as np
##
# Calculate lifted index.
#
# @param P: Pressure at current level (mb) [array]
# @param T: Temperature at current level (K) [array]
# @param RH: Relative humidity at current level (0-100) [array]
# @param T_500MB: Temperature at 500MB (K) [array]
# @param P_500MB: Pressure at 500MB (mb) [usually scalar==500]
# @return: Calculated lifted index array
def execute(P, T, RH, T_500MB, P_500MB=500):
    "Calculate lifted index from P,T,RH,T at 500MB, P at 500MB"
    # Mask any invalid inputs (even in scalars)
    T = np.copy(T);
    T[T < lowKTemp] = NaN
    
    rhqc = minimum(100.0, maximum(1.0, RH))
    # NaN cells in RH will be 1.0 in rhqc. Make them NaN again.
    rhqc[isnan(RH)] = NaN
    
    eee = const1 - const2 * T
    # T is in degrees K, so it shouldn't ever be zero.
    eee -= const3 / T
    eee = exp(eee) * rhqc
    
    b = const4 - log(eee)
    tdp = b-sqrt(b*b-const5)
    tdp /= const6

    tc = tdp - (T-tdp)*(const7+const8/T+const9*tdp)
    pc = P * (tc/T)**const10

    # We need to process cells where pc <= P_500MB differently
    # from the ones where it is > P_500MB. However, we don't want
    # to do anything with NaN cells. Create two boolean masks for 
    # array indexing. They are logical inverses of one another,
    # except that both contain False where cells in pc or P_500MB
    # are NaN. 
    le_mask = pc <= P_500MB
    gt_mask = pc > P_500MB

    if shape(P_500MB) == shape(le_mask):
        P5_le = P_500MB[le_mask]
        P5_gt = P_500MB[gt_mask]
    else: # assume it was a scalar
        P5_le = P_500MB
        P5_gt = P_500MB
        
    # Create an output array with all cells NaN.
    result = copy(T)
    result[:] = NaN
    
    intm_result = (P5_le/P)**const11
    
    # make sure to properly mask an intm_result array
    if type(intm_result) == ndarray:
        intm_result = intm_result[le_mask]
        
    intm_result *= -T[le_mask]
    result[le_mask] = T_500MB[le_mask] + intm_result
    
    result = result
    tc_ge = AdiabaticTemp.calculate(tc[gt_mask], pc[gt_mask]) * (P5_gt/pc[gt_mask])**const11        
    
    result[gt_mask] = T_500MB[gt_mask] - TempOfTe.execute(tc_ge, P5_gt)
    result[P < P_500MB] = NaN
    
    return result
