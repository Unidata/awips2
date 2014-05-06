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

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    --/--/-----                                  Initial creation
#    May 06, 2014    3101          njensen        Cast numpy shape values to int
#                                                  for cross platform compatibility

from numpy import ndarray, float32, NaN
from numpy import sin, isnan, invert
from com.raytheon.uf.common.derivparam.python.function import DistFilterPythonAdapter as DistFilter

MAX_WAVE_NUMBER = 15

##
#  Apply a spatial filter to the first argument. The second argument is a constant which determines the distance over which to apply the filter, in km if positive, in number of grid points if negative.
#
# @param grid: the grid to filter
# @param distance: the distance over which to filter
# @param dx: Grid spacing in the x-direction (m)
# @param dy: Grid spacing in the y-direction (m)
# @param times: Number of times to filter
# @return: A filtered grid
def execute(input, dist, dx, dy, times=1):
    if (dist<0):
        npts = -dist;
    else:
        npts = dist * (1000/dx[input.shape[0]/2,input.shape[1]/2]);

    return executeJava(input, npts, times)
    #return executePython(input, npts, times)

def executeJava(input, npts, times):
    output = DistFilter.filter(input, npts, int(input.shape[1]), int(input.shape[0]), times).__numpy__[0]
    output[output==1e37] = NaN
    return output
    

def executePython(input, npts, times=1):
    n = int(npts + 0.99)
    if (n<2):
        n = 2;
    elif (n>MAX_WAVE_NUMBER):
        n = MAX_WAVE_NUMBER
    d = (n+1)/2;
    dd = d+d;
    m = dd+1;
    #Calculate wave table
    waveno = 3.14159265/npts
    jweights = ndarray([m,m], float32)
    iweights = ndarray([m,m], float32)
    for c in range(-d, d+1):
        if (c != 0):
            val = sin(waveno*c)/(waveno*c)
            iweights[c+d,:] = val
            jweights[:,c+d] = val
        else:
            iweights[c+d,:] = 1
            jweights[:,c+d] = 1
    weights = iweights*jweights
    weights = weights/weights.sum()
    output = input;
    for i in range(times):
        input = output;
        output = ndarray(input.shape, float32)
        for i in range(input.shape[0]):
            for j in range(input.shape[1]):
                wi1 = 0
                wi2 = m
                wj1 = 0
                wj2 = m
                i1 = i - d
                i2 = i + d + 1
                j1 = j - d
                j2 = j + d + 1
                if (i1 < 0):
                    wi1 = -i1
                    i1 = 0
                if (i2 > input.shape[0]):
                    wi2 = - (i2 - input.shape[0])
                    i2 = input.shape[0]
                if (j1 < 0):
                    wj1 = -j1
                    j1 = 0
                if (j2 > input.shape[1]):
                    wj2 =  - (j2 - input.shape[1])
                    j2 = input.shape[1]
                notnanmask = invert(isnan(input[i1:i2,j1:j2]))
                tot = weights[wi1:wi2,wj1:wj2][notnanmask].sum()
                if tot >= 0.95:
                    output[i,j] = (input[i1:i2,j1:j2][notnanmask]*weights[wi1:wi2,wj1:wj2][notnanmask]).sum()/tot
                else:
                    output[i,j] = NaN
    return output
