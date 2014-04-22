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
## @file gamma.py

from numpy.ma.core import masked_values
from numpy.ma.core import log

## 
# Calculate lapse rate from temperature and pressure pairs.
# This method is derived from the lapserate.f function, which 
# used an extra integer parameter to determine which of 5 functions
# were to be used. However, only the "4" (pressure) function was
# being used.
#
# @param Tlo: Temperature at lower level (K) 
# @param Plo: Pressure at lower level (mb)
# @param Thi: Temperature at upper level (K)
# @param Phi: Pressure at upper level (mb)
# @return: Lapse rate (C/km)  
def execute(Tlo, Plo, Thi, Phi):
    C = 0.034167
    
    Tratio = Thi/Tlo
    Pratio = Phi/Plo
    
    logTratio = log(Tratio)
    logPratio = log(Pratio)
    
    # Guard against divide-by-zero errors, again
    logPratio = masked_values(logPratio, 0, copy=False)
    
    result = C * logTratio / logPratio
    
    return result