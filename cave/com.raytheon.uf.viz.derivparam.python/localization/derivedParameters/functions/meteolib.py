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

# Provide access to the meteolib functions through the java library


from com.raytheon.edex.meteoLib import Controller
from jep import jarray, JFLOAT_ID
import numpy

#allows calling ztopsa with either a float, numpy.float32, or ndarray
def ztopsa(Z):
    if isinstance(Z, float) or isinstance(Z, numpy.float32):
        if (Z > -9998): 
            return Controller.ztopsa(float(Z))
    elif isinstance(Z, numpy.ndarray):
        result = numpy.ndarray(Z.shape, numpy.float32)
        for i in range(len(Z)):
            result[i] = ztopsa(Z[i]);
        return result
    return Z

#allows calling ptozsa with either a float, numpy.float32, or ndarray
def ptozsa(P):
    if isinstance(P, float) or isinstance(P, numpy.float32):
        if (P > -9998):
            return Controller.ptozsa(float(P))
    elif isinstance(P, numpy.ndarray):
        result = numpy.ndarray(P.shape, numpy.float32)
        for i in range(len(P)):
            result[i] = ptozsa(P[i]);
        return result
    return P

def calctd2(P,T,Q):
    jP = numpy2java(P)
    jT = numpy2java(T)
    jQ = numpy2java(Q)
    jresult = Controller.calctd2(jP,jT,jQ,int(P.shape[0]),int(P.shape[1]))
    result = java2numpy(jresult, P.shape)
    return result

def numpy2java(a):
    if (len(a.shape) == 1):
        jA = jarray(a.shape[0], JFLOAT_ID, 0)
        for i in range(a.shape[0]):
            jA[i] = float(a[i])
        return jA
    elif(len(a.shape) == 2):
        jA = jarray(a.shape[0]*a.shape[1], JFLOAT_ID, 0)
        for i in range(a.shape[0]):
            for j in range(a.shape[1]):
                jA[i*a.shape[0]+j] = float(a[i][j])
        return jA

def java2numpy(a, shape):
    if (len(shape) == 1):
        nA = numpy.ndarray(shape, numpy.float32)
        for i in range(shape[0]):
            nA[i] = a[i]
            if (nA[i] > 1e+36):
                nA[i] = -9999
        return nA
    elif(len(shape) == 2):
        nA = numpy.ndarray(shape, numpy.float32)
        for i in range(shape[0]):
            for j in range(shape[1]):
                nA[i][j] = a[i*shape[0]+j] 
                if (nA[i][j] > 1e+36):
                    nA[i][j] = -9999
        return nA