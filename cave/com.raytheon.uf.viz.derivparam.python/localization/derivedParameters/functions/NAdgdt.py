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

#import sys
#sys.path.append("..")

from numpy import ndarray,NaN
from numpy import float32
from Vector import execute as Vector
from Multiply import execute as Multiply

##
#  This routine computes the non-advective local change of an arbitrary
# C conservative parameter `a'.
#
# @param u: U component of wind.
# @param v: V component of wind.
# @param a: Arbitrary conservative parameter.
# @param dx: Grid spacing in the x-direction (m)
# @param dy: Grid spacing in the y-direction (m)
# @return: A vector representing the non-advective local change
def execute(u, v, a, dx, dy):

        u = -u
        
        # TODO: support scalars for dx and dy
        # find 2x partial derivatives of u, v, and a with respect to x
        dudy = (u[2:,1:-1] - u[0:-2,1:-1]) / dy[1:-1,1:-1]
        dvdy = (v[2:,1:-1] - v[0:-2,1:-1]) / dy[1:-1,1:-1]
        dady = (a[2:,1:-1] - a[0:-2,1:-1]) / dy[1:-1,1:-1]
        # find 2x partial derivatives of u, v, and a with respect to y
        dudx = (u[1:-1,2:] - u[1:-1,0:-2]) / dx[1:-1,1:-1]
        dvdx = (v[1:-1,2:] - v[1:-1,0:-2]) / dx[1:-1,1:-1]
        dadx = (a[1:-1,2:] - a[1:-1,0:-2]) / dx[1:-1,1:-1]
    
        # create empty arrays for output
        dadxdt = ndarray(u.shape, float32)
        dadydt = ndarray(u.shape, float32)
        
        # First output array:
        # mark the output array edges as invalid
        dadxdt[0,:] = NaN
        dadxdt[-1,:] = NaN
        dadxdt[1:-1,0] = NaN
        dadxdt[1:-1,-1] = NaN
        # calculate values for the middle
        dadxdt[1:-1,1:-1] = -0.5 * (dudx*dadx + dvdx*dady)
    
        # Second output array:
        # mark the output array edges as invalid
        dadydt[0,:] = NaN
        dadydt[-1,:] = NaN
        dadydt[1:-1,0] = NaN
        dadydt[1:-1,-1] = NaN
        # calculate values for the middle
        dadydt[1:-1,1:-1] = -0.5 * (dudy*dadx + dvdy*dady)
        
        return Vector(-dadxdt, dadydt)
    
def test():
    ua = [[-1.38, -1.88, -2.49000001, -3.12999988, -3.7099998, -4.0999999],
     [-1.22000003, -1.68999994, -2.26999998, -2.88999987, -3.46000004, -3.82999992],
     [-1.03999996, -1.46999991, -2.00999999, -2.5999999, -3.12999988, -3.49000001],
     [-0.84999996, -1.24000001, -1.70999992, -2.24000001, -2.73000002, -3.07999992],
     [-0.64999998, -0.98999995, -1.39999998, -1.8499999, -2.28999996, -2.62999988],
     [-0.44999999, -0.75, -1.09000003, -1.45999992, -1.82999992, -2.15999985]]
    va = [[ 5.10999966, 5.42999983, 5.76999998, 6.10999966, 6.40999985, 6.63999987],
     [ 4.63999987, 4.94000006, 5.23999977, 5.53999996, 5.80999994, 6.06999969],
     [ 4.17000008, 4.44999981, 4.73999977, 5.01999998, 5.26999998, 5.52999973],
     [ 3.68999982, 3.99000001, 4.27999973, 4.53999996, 4.76999998, 5.03999996],
     [ 3.22000003, 3.53999996, 3.83999991, 4.0999999, 4.32999992, 4.5999999],
     [ 2.75, 3.0999999, 3.41999984, 3.69999981, 3.92999983, 4.19000006]]
    aa = [[ 441.26159668, 440.68197632, 440.29559326, 439.90917969, 439.71600342,
     439.52279663],
     [ 441.06838989, 440.68197632, 440.29559326, 439.90917969, 439.52279663,
     439.52279663],
     [ 441.06838989, 440.68197632, 440.29559326, 439.90917969, 439.52279663,
     439.52279663],
     [ 441.06838989, 440.68197632, 440.48876953, 440.10238647, 439.71600342,
     439.52279663],
     [ 441.06838989, 440.68197632, 440.48876953, 440.10238647, 439.90917969,
     439.71600342],
     [ 441.06838989, 440.68197632, 440.48876953, 440.10238647, 439.90917969,
     439.71600342]]
    dxa = [[ 34735.1328125, 34695.375, 34642.5625, 34590.0078125,
     34537.7265625, 34485.7109375],
     [ 34823.0859375, 34783.625, 34731.25390625, 34679.13671875,
     34627.28125, 34575.69921875],
     [ 34939.6328125, 34900.6171875, 34848.8359375, 34797.3046875,
     34746.03125, 34695.01953125],
     [ 35055.359375, 35016.7890625, 34965.6015625, 34914.6484375,
     34863.953125, 34813.5234375],
     [ 35170.2421875, 35132.125, 35081.5234375, 35031.1640625,
     34981.046875, 34931.1875],
     [ 35284.27734375, 35246.60546875, 35196.59765625, 35146.82421875,
     35097.29296875, 35048.0078125]]
    dya = [[ 34735.1328125, 34695.375, 34642.5625, 34590.0078125,
     34537.7265625, 34485.7109375],
     [ 34823.0859375, 34783.625, 34731.25390625, 34679.13671875,
     34627.28125, 34575.69921875],
     [ 34939.6328125, 34900.6171875, 34848.8359375, 34797.3046875,
     34746.03125, 34695.01953125],
     [ 35055.359375, 35016.7890625, 34965.6015625, 34914.6484375,
     34863.953125, 34813.5234375],
     [ 35170.2421875, 35132.125, 35081.5234375, 35031.1640625,
     34981.046875, 34931.1875],
     [ 35284.27734375, 35246.60546875, 35196.59765625, 35146.82421875,
     35097.29296875, 35048.0078125 ]]
    u = ndarray([6, 6], float32)
    v = ndarray([6, 6], float32)
    a = ndarray([6, 6], float32)
    dx = ndarray([6, 6], float32)
    dy = ndarray([6, 6], float32)
    for i in range(0,6):
        for j in range(0,6):
            u[i,j] = ua[i][j]
            v[i,j] = va[i][j]
            a[i,j] = aa[i][j]
            dx[i,j] = dxa[i][j]
            dy[i,j] = dya[i][j]
    
    res = execute(u, v, a, dx, dy)
    print res[2].flatten()
    print res[3].flatten()
    
#test()