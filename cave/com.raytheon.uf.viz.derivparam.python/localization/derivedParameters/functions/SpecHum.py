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

# ----------------------------------------------------------------
# Calculate Specific Humidity (g/Kg) from Pressure, Temperature and 
# Relative Humidity.
# ----------------------------------------------------------------
from numpy import exp
from numpy.ma.core import masked_greater
from numpy.ma.core import masked_values
from numpy.ma.core import filled

def execute(*args):
    if len(args) == 2: 
        "Calculate specific humidity (SHx) from pressure and vapor pressure"
        # From the old C version:
        #     formula is 1000*epsilon*e/(p-(1-epsilon)*e), where
        #     p is pressure, e is vapor pressure, and epsilon is the
        #     constant 0.622.  For evaluation, 622.0 (1000*epsilon) is
        #     divided into the constants in the denominator.
        #     p is input field 0, e is input field 1.  The 100.0 in
        #     the first constant is converting mb to pascals.
        P=args[0]
        VPP=args[1]
        oneMinusEpsilon = 0.378
        epsilonTimes1000 = 622
        num = 100.0/epsilonTimes1000 * VPP
        dnm = P - ((oneMinusEpsilon/epsilonTimes1000) * VPP)
        SHx = num / dnm

        return SHx
    else:
        "Calculate specific humidity (SHx) from pressure, temperature, and \
        relative humidity."
        # Get masked arrays so results for out-of-range values aren't generated
        Pma = args[0]
        Tma = args[1]
        RHma = args[2]
        # do the math
        eee = RHma*exp(28.48859-0.0091379024*Tma-6106.396/Tma)
        SHx = eee/(Pma-0.00060771703*eee)
        return SHx
