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

from numpy import sqrt
from numpy import ndarray
from numpy import float32

##
# Routine to calculate sweat index from the total totals,
# 850 dewpoint, and wind components at 850 and 500.
#
# @param tt: Total Totals
# @param td8: 850mb dewpoint (K)
# @param u8: 850mb u-component (K)
# @param v8: 850mb v-component (K)
# @param u5: 500mb u-component (K)
# @param v5: 500mb v-component (K)
# @return: Sweat Index

def execute(tt, td8, u8, v8, u5, v5):
    s8 = sqrt(u8*u8+v8*v8)
    s5 = sqrt(u5*u5+v5*v5)
    s = s8*s5
    z_mask = s == 0
    nz_mask = s != 0
    sdir = ndarray(tt.shape, float32)
    sdir[z_mask] = 0
    sdir[nz_mask] = (u5[nz_mask]*v8[nz_mask]-v5[nz_mask]*u8[nz_mask])/s[nz_mask]
    tt49= ndarray(tt.shape, float32)
    tt49_mask = tt>=49
    tt49[tt<49] = 0
    tt49[tt49_mask] = tt[tt49_mask]-49.0
    result = 12*(td8-273.15)
    result += 20*tt49 
    result += 2*1.944*s8 
    result += s5*1.944 
    result += 125*(sdir+0.2)
    return result