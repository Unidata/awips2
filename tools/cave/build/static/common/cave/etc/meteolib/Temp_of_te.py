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

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12 Feb 2010     #4502         jelkins        Initial Creation.

from MeteoLibTools import MeteoLibTools as MeteoLib
from CPointer import CPointer
from numpy import zeros
from numpy import sqrt

meteoLib = MeteoLib()
adiabatic_te = meteoLib.adiabatic_te

tmin = 193
tmax = 333
nval = 1 + tmax - tmin
TeData_ptr = CPointer(zeros(7 * nval,'f'))
Te1000_ptr = CPointer()
Te850_ptr = CPointer()
Te700_ptr = CPointer()
Te600_ptr = CPointer()
Te500_ptr = CPointer()
Te350_ptr = CPointer()
Te200_ptr = CPointer()

def temp_of_te(te, press):
    """ This routine calculates the saturation tempurature of an equivalent
     temperature at given pressure using the adiabatic definition
     
     This function has been ported from the c language temp_of_te
    
    """
    press_ptr = CPointer([press])
    te_ptr = CPointer([te])
    
    # grant access to the static variables
    from Temp_of_te import tmin
    from Temp_of_te import tmax 
    from Temp_of_te import nval
    from Temp_of_te import TeData_ptr
    from Temp_of_te import Te1000_ptr 
    from Temp_of_te import Te850_ptr
    from Temp_of_te import Te700_ptr 
    from Temp_of_te import Te600_ptr
    from Temp_of_te import Te500_ptr
    from Temp_of_te import Te350_ptr
    from Temp_of_te import Te200_ptr
    
    TeLookup_ptr = CPointer()
    base = None
    t = None
    p = None
    t1 = None
    t2 = None
    d = None
    d1 = None
    d2 = None
    w = None
    i = None
    
    # very first time, construct lookup table Te's of T from 193 to 343 K
    if (Te1000_ptr.target == None):
        Te1000_ptr.copy(TeData_ptr)
        Te1000_ptr -= tmin
        Te850_ptr.copy(Te1000_ptr)
        Te850_ptr += nval;
        Te700_ptr.copy(Te850_ptr)
        Te700_ptr += nval
        Te600_ptr.copy(Te700_ptr)
        Te600_ptr += nval
        Te500_ptr.copy(Te600_ptr)
        Te500_ptr += nval
        Te350_ptr.copy(Te500_ptr)
        Te350_ptr += nval
        Te200_ptr.copy(Te350_ptr)
        Te200_ptr += nval
        p = 1000
        for t in range(tmin, tmax+1):
            Te1000_ptr[t] = adiabatic_te(t, p)
        p = 850;
        for t in range(tmin, tmax+1):
            Te850_ptr[t] = adiabatic_te(t, p)
        p = 700;
        for t in range(tmin, tmax+1):
            Te700_ptr[t] = adiabatic_te(t, p)
        p = 600;
        for t in range(tmin, tmax+1):
            Te600_ptr[t] = adiabatic_te(t, p)
        p = 500;
        for t in range(tmin, tmax+1):
            Te500_ptr[t] = adiabatic_te(t, p)
        p = 350;
        for t in range(tmin, tmax+1):
            Te350_ptr[t] = adiabatic_te(t, p)
        p = 200;
        for t in range(tmin, tmax+1):
            Te200_ptr[t] = adiabatic_te(t, p)
            
    # find correct table, check for beyond bounds of table
    if (press_ptr[0] <= 250):
        TeLookup_ptr.copy(Te200_ptr)
        base = 200;
    elif (press_ptr[0] <= 400):
        TeLookup_ptr.copy(Te350_ptr)
        base = 350;
    elif (press_ptr[0] <= 550) :
        TeLookup_ptr.copy(Te500_ptr)
        base = 500;
    elif (press_ptr[0] <= 650):
        TeLookup_ptr.copy(Te600_ptr)
        base = 600;
    elif (press_ptr[0] <= 750):
        TeLookup_ptr.copy(Te700_ptr)
        base = 700;
    elif (press_ptr[0] <= 900) :
        TeLookup_ptr.copy(Te850_ptr)
        base = 850;
    else:
        TeLookup_ptr.copy(Te1000_ptr)
        base = 1000;

    if (te_ptr[0] < TeLookup_ptr[tmin + 1]):
        return te_ptr[0]
    if (te_ptr[0] >= TeLookup_ptr[tmax]):
        return 1e37
    
    # use table to get first guesses for value of temp 
    #  if (diag) printf("te,base %.2f %.0f\n",*te,base); 
    t1 = tmin
    t2 = te_ptr[0]
    if (t2 > tmax):
        t2 = tmax
    while (t2 - t1 >= 3):
        t = ((t1 + t2) / 2)
        if (TeLookup_ptr[t] > te_ptr[0]):
            t2 = t
        elif (TeLookup_ptr[t] < te_ptr[0]):
            t1 = t
        else:
            if (t1 < t - 1):
                t1 = t - 1
            if (t2 > t + 1):
                t2 = t + 1
            break
        
    # if (diag) printf("t1,t2,te1,te2  %.2f %.2f  %.2f %.2f\n",t1,t2,
    # TeLookup[(int)t1],TeLookup[(int)t2]);
    w = sqrt(base / (press_ptr[0]))
    t1 = (1 - w) * TeLookup_ptr[t1] + w * t1;
    t2 = (1 - w) * TeLookup_ptr[t2] + w * t2;
    # if (diag) printf("t1,t2 %.2f %.2f\n",t1,t2);
    
    # Iterate to find the exact solution
    d1 = te_ptr[0] - adiabatic_te(t1, press_ptr[0])
    d2 = adiabatic_te(t2, press_ptr[0]) - te_ptr[0]
    w = d2 / (d1 + d2)
    t = w * t1 + (1 - w) * t2
    d = adiabatic_te(t, press_ptr[0]) - te_ptr[0]
    
    for i in range(0,10):
        if (d > 0.01):
            d2 = d
            t2 = t
        elif (d < -0.01):
            d1 = -d
            t1 = t
        else:
            break
        w = d2 / (d1 + d2)
        t = w * t1 + (1 - w) * t2
        d = adiabatic_te(t, press_ptr[0]) - te_ptr[0];

    #  if (diag) printf("t,i %.2f %d\n",t,i);
     
    return t;

def test():
    """ Unit Test
    """
    
    result = temp_of_te(270, 500)
    print result
