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

## @file TW.py
import gridslice
from numpy import abs
from numpy import bool
from numpy import clip
from numpy import empty
from numpy import exp
from numpy import log
from numpy import rank
from numpy import sqrt
from numpy import zeros
from numpy import where

## Constants declared in the original Fortran.
# I wish they had descriptive names.

## @var c1: constant used in multiple functions.
c1 = 0.0091379024
## @var c2: constant used in multiple functions.
c2 = 6106.3960

## 
# Calculate wet bulb temperature in degrees C
# from pressure, temperature, and relative humidity.
#
# @param P: Pressure in millibars
# @param T: Temperature in degrees Kelvin
# @param RH: Relative humidity from 0 to 100
# @return: wet bulb temperature in degrees C as a numpy array 
#
def execute1(P, T, RH):
    
    # Find dry-bulb temp from temp and relative humidity
    RH = clip(RH,1.0,100.0)
    b = T * c1
    b += c2/T
    b -= log(RH/100.0)
    val = b*b
    val -= 223.1986
    td = b - sqrt(val)
    td /= 0.0182758048
    
    TW = myTW(T, td, P)
    return TW

def execute2(GH3D, T3D, RH3D, TILT):
    P = zeros(T3D[0][0].shape, 'float32')
    for i in  range(len(RH3D[0])):
        T = T3D[0][i]
        RH = RH3D[0][i]
        P.fill(RH3D[1][0][i])
        TW = execute1(P, T, RH)
        T3D[0][i] = TW
    vc3d = GH3D[0]
    slice3d = T3D[0]
    slice = gridslice.createNumpySlice(vc3d, slice3d, TILT, 2)
    slice = where(slice == 9.99999993e+36, - 999999.0, slice)
    return slice
    
##
# This function takes arrays of values and filters them against expected values
# for terrestrial weather. Where the dewpoint temperature is greater than the
# observed temperature or the saturation vapor pressure is below 10 (millibars?),
# it returns a TW[i,j] estimate of (T[i,j]+Td[i,j])/2. Where T[i,j] is less than 100
# degrees Kelvin, it returns a TW[i,j] estimate of T[i,j]. Other cells are passed on
# to the calcTW() function for iterative estimation of TW.
#
# @param T: Observed temperature in degrees Kelvin.
# @param Td: Calculated dewpoint temperature in degrees Kelvin.
# @param P: Pressure in millibars
# @return: TW, the wet-bulb temperature
# @rtype: assumed to be a numpy array. 
def myTW(T, Td, P):
    td_lt_t = Td < T
    t_ge_cold = T>=100.0

    typical = td_lt_t & t_ge_cold

    satVP = zeros(T.shape, dtype=T.dtype)
    satVP[typical] = calcSatVP(T[typical])
    
    norm_svp = satVP <= 10.0
    sane = typical & norm_svp

    veryCold = td_lt_t & ~t_ge_cold
    td_satvp_bad = ~( td_lt_t & norm_svp ) # includes NaN cells
    
    if rank(P)==0:
        sel = lambda val, mask : val
    else:
        sel = lambda val, mask : val[mask]
        
    TW = empty(T.shape, dtype=T.dtype)
    TW[td_satvp_bad] = (T[td_satvp_bad] + Td[td_satvp_bad])/2
    TW[veryCold] = T[veryCold]
    TW[sane] = calcTW(T[sane], Td[sane], sel(P,sane), satVP[sane]) 
   
    return(TW)

##
# Calculate saturation vapor pressure from T.
#
# @param T: Temperature in degrees K
# @return: ew, saturation vapor pressure (in millibars?)
#    
def calcSatVP(T):
    "Calculate saturation vapor pressure from T."
    c0 = 26.66082
    ew = -c1*T
    ew += c0
    ew -= c2/T
    return ew

##
# Calculate TW iteratively from the input parameters. This is called 
# from myTW(); the parameter arrays have already been tested for
# "reasonable" values. 
#
# @param T: temperature in degrees K
# @param Td: dry-bulb temperature in degrees K
# @param P: pressure (in millibars?)
# @param satVP: saturation vapor pressure (in millibars?)
# @return: TW, the estimated wet-bulb temperature.
# @rtype: assumed to be a numpy array
#
def calcTW(T, Td, P, satVP):
    ""
    f = 0.0006355
    miniscule = 1e-5
    satVPmin = -50.0
    satVPmax = 10.0
    
    # One more special case: "ridiculously small" dewpoint vapor pressure.
    Tdx = Td
    dewptVP = calcSatVP(Tdx)
    dvpLtM50 = dewptVP < satVPmin
    while( any(dvpLtM50) ):
        Tdx[dvpLtM50] = Tdx[dvpLtM50] + 10
        dewptVP[dvpLtM50] = calcSatVP(Tdx[dvpLtM50])
        # Update dvpLtM50.
        # The fancy indexing is to skip cells that are already False,
        # since they won't become True as a result of the loop.
        dvpLtM50[dvpLtM50] = dewptVP[dvpLtM50] < satVPmin
        
    fp = P * f

    if rank(fp) == 0:
        sel = lambda val, mask : val
    else:
        sel = lambda val, mask : val[mask]
         
    # we need e**satVP and e**dewptVP for calculating s and the initial TW estimate
    # (e is the base of the natural logarithm).
    expSatVP = exp(satVP)
    expDewptVP = exp(dewptVP)
    
    s = expSatVP-expDewptVP
    s /= T-Tdx
    TW = T*fp
    TW += Tdx*s
    TW /= fp+s
    
    # new, non-exp'd satVP for the loop below
    satVP = calcSatVP(TW)

    # create some boolean arrays to use in the for loop.
    # creating them once here allows us to use the out parameter of
    # the numpy boolean functions to avoid repeatedly allocating space.
    # satVPinRange = empty(shape(TW), dtype=bool) 
    # svrTemp = empty(shape(TW), dtype=bool)
    
    # At each step of the iteration, esat(Tw)-esat(Td) is compared to
    # (T-Tw)*p/(eps*L).  When that difference is less than one part in 
    # 10000 of esat(Tw), or ten iterations have been done, the iteration stops.
    # This is basically trying to find the value of Tw where de is 0.  The
    # value s is the derivative of de with respect to Tw, a fairly standard
    # numerical technique for finding the zero value of a function.
    for iteration in xrange(10):
        satVPinRange = (satVPmin<=satVP) & (satVP<=satVPmax)
        if not any(satVPinRange):
            break

        expSatVP = exp(satVP[satVPinRange])

        de = calcDe(T[satVPinRange], TW[satVPinRange], expSatVP, 
                    expDewptVP[satVPinRange], sel(fp,satVPinRange))

        mag = abs(de/expSatVP)


        significant = miniscule <= mag
        if any(significant):
            satVPinRange[satVPinRange] = significant
            # now we can mix TW and de to obtain a new estimate for TW:
            TW[satVPinRange] = reestimateTW( TW[satVPinRange],
                                     expSatVP[significant],
                                     de[significant],
                                     sel(fp, satVPinRange) )
            # Get a new satVP to see if the iteration can stop.
            # Only cells with a different TW will have a different satVP.
            satVP[satVPinRange] = calcSatVP(TW[satVPinRange])
        else:
            break
            
    return TW

##
# Calculate the value of the de variable used in calcTW.
#
# @param T: The temperature in degrees Kelvin.
# @param TW: The current estimate of the wet-bulb temperature in degrees Kelvin.
# @param expSatVP: The current saturation vapor pressure estimate
# @param expDewptVP: The current dewpoint vapor pressure estimate
# @param fp: The pressure times the f constant.
# @return: the calculated value for de
#
def calcDe(T, TW, expSatVP, expDewptVP, fp):
    "Calculate de from temp, estimated "
    de = T-TW
    de *= fp
    de += expDewptVP
    de -= expSatVP
    return de

def reestimateTW(TW, expSatVP, de, fp):
    "Find a new estimate for TW based on the current estimate, \
    the saturation vapor pressure, de, and fp."
    s = -c2/(TW*TW)
    s += c1
    s *= expSatVP
    s -= fp
    newTW = TW - de/s
    return newTW
