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


import numpy as np
import AdiabaticTemp

# create an array of base pressures
pBases = np.array([200,350,500,600,700,850,1000], dtype=np.float32)

# create an array of splitpoints for pressure bins
pSplits = np.array([250,400,550,650,750,900], dtype=np.float32)

tmax = 333
tmin = 193

# create a lookup table of adiabatic temperatures for chosen base pressures 
parr, tarr = np.ogrid[0:len(pBases), tmin:tmax]
parr = pBases[parr]
tarr = np.array(tarr, dtype=np.float32)
lookupTable = AdiabaticTemp.execute(tarr, parr)

##
# Calculate the saturation temperature of an equivalent temperature at
# the specified pressure. Another way of expressing it is to find the
# temperature TofTe such that AdiabaticTemp.execute(TofTe,P)=T.
#
# @param T: Adiabatic temperature of TofTe at P (K)
# @type T: Numpy array of float32
# @param P: The pressure at which the calculation is made (mb)
# @type P: Numpy array of float32
# @return: TofTe
# @rtype: Numpy array 
def execute(T,P):
    "Find Tans such that AdiabaticTemp.execute(Tans,P) == T."
    if np.shape(T) != np.shape(P):
        bcastOnes = np.ones(T.shape, dtype=np.float32) * np.ones(np.shape(P), dtype=np.float32)
        T = T * bcastOnes
        P = P * bcastOnes
    pBaseIdx = np.searchsorted(pSplits, P)
    # find cells that are too low or too high
    lowTMask = T<lookupTable[pBaseIdx,1]
    goodhighTMask = ((T<=lookupTable[pBaseIdx,-1]) & ~(T<=tmax)) # NaNs in T are True in highTMask
    Tgood = np.array(T, dtype=np.float32)
    Tgood[goodhighTMask] = tmax
    highTMask = ~(Tgood<=lookupTable[pBaseIdx,-1])
    goodMask = ~( lowTMask | highTMask)
    Tgood = Tgood[goodMask]
    pBaseIdx = pBaseIdx[goodMask]
    
    # find t1 and t2, lower/upper integer estimate arrays
    t1 = np.ones(Tgood.shape, dtype=np.integer) * tmin
    t2 = np.array(Tgood, dtype=np.integer)
    TMask = t2-t1>=3
    t1Mask = np.ones(np.shape(TMask), dtype=np.bool)
    t2Mask = np.ones(np.shape(TMask), dtype=np.bool)
    # Tgood cells just above the lower threshold can result in False TMask cells.
    # Don't allow t1Mask or t2Mask to be True where TMask is False.
    t1Mask[:] = TMask
    t2Mask[:] = TMask
    while(np.any(TMask)):
        # t = (int)((t1+t2)/2), for cells where TMask is True
        tEst = np.array((t1[TMask]+t2[TMask])/2, dtype=np.integer)
        telu = lookupTable[pBaseIdx[TMask], tEst-tmin]
        
        # where adiabaticTemp(t,pBase) is > T, adjust t2 down.
        cmpMask = telu > Tgood[TMask]
        t2Mask[TMask] = cmpMask
        t2[t2Mask] = tEst[cmpMask]
        
        # where adiabaticTemp(t,pBase) is < T, adjust t1 up.  
        cmpMask = telu < Tgood[TMask]
        t1Mask[TMask] = cmpMask
        t1[t1Mask] = tEst[cmpMask]
        
        # where we hit it exactly, set t1 and t2 to one-off. 
        cmpMask = (telu == Tgood[TMask])
        t1Mask[TMask] = cmpMask
        t2Mask[TMask] = cmpMask
        t1[t1Mask] = tEst[cmpMask]-1
        t2[t2Mask] = tEst[cmpMask]+1

        # Clean up t1Mask and t2Mask for next iteration.
        # Without this, t?[t?Mask] could expect too many values.
        t1Mask[TMask] = False
        t2Mask[TMask] = False
        # figure out which cells still need the estimate narrowed.
        TMask[TMask] = t2[TMask]-t1[TMask]>=3

    # weight t1 and t2 by the ratio of base pressure to P
    # (t1 and t2 become floating-pt here)
    pBase = pBases[pBaseIdx]
    weight = np.sqrt(pBase/P[goodMask])
    t1 = (1-weight) * lookupTable[pBaseIdx, t1-tmin] + weight * t1.astype(T.dtype)
    t2 = (1-weight) * lookupTable[pBaseIdx, t2-tmin] + weight * t2.astype(T.dtype)
    
    # now find a new weight based on the difference between T and
    # the adiabatic temperature of t1 and t2.
    if np.isscalar(P):
        PP = P
    else:
        PP = P[goodMask]
    diff1 = T[goodMask] - AdiabaticTemp.execute(t1,PP)
    diff2 = AdiabaticTemp.execute(t2,PP) - T[goodMask]
    weight = diff2/(diff1+diff2)
    Tans = weight*t1 + (1-weight)*t2
    diff = AdiabaticTemp.execute(Tans,PP) - T[goodMask]
    
    # Iterate to find result to nearest .01 degree K
    TMask[:] = True # all cells still to calculate
    for loopcount in xrange(10):
        if np.any(TMask):
            # see which estimates are high and low
            t1Mask[TMask] = diff[TMask] < -0.01
            t2Mask[TMask] = diff[TMask] > 0.01
            
            # adjust down the high estimates
            t2[t2Mask] = Tans[t2Mask]
            diff2[t2Mask] = diff[t2Mask]
            
            # adjust up the low estimates
            t1[t1Mask] = Tans[t1Mask]
            diff1[t1Mask] = -diff[t1Mask]
            
            # calculate a new weight, estimated T, and diff for active cells
            if np.isscalar(P):
                PPP = P
            else:
                PPP = PP[TMask]
            weight[TMask] = diff2[TMask]/(diff1[TMask]+diff2[TMask])
            Tans[TMask] = weight[TMask]*t1[TMask] + (1-weight[TMask])*t2[TMask] 
            diff[TMask] = AdiabaticTemp.execute(Tans[TMask], PPP) - T[TMask]
            # eliminate cells we've finished
            TMask[TMask] = t1Mask[TMask] | t2Mask[TMask]
        else:
            break
    
    # Create an empty fully masked array for the result
    result = np.empty(np.shape(T), dtype=np.float32)
    result[:] = np.NaN 
    
    result[goodMask] = Tans
    # replace the very low input values with the original T
    result[lowTMask] = T[lowTMask]
    return result
    