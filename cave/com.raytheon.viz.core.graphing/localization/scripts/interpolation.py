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

import numpy as np
from scipy.interpolate import Rbf

def logInterpolate(data, gridX, gridY, xMin, xMax, yMin, yMax):
    x = data.__numpy__[0]
    y = data.__numpy__[1]
    z = data.__numpy__[2]
        
    y = (np.log10(y) - np.log10(yMin))/(np.log10(yMax) - np.log10(yMin))*(gridY - 1)
    x = (x - xMin)/(xMax - xMin)*(gridX - 1)

    xspace = np.linspace(0, gridX, gridX)
    yspace = np.linspace(0, gridY, gridY)
    XI,YI = np.meshgrid(xspace,yspace)
    
    rbf = Rbf(x,y,z, function="linear")
    ZI = rbf(XI,YI)
    data.setResults(xspace.astype(np.float32),yspace.astype(np.float32),ZI.astype(np.float32))
    return data
    
    
    
def linInterpolate(data, gridX, gridY, xMin, xMax, yMin, yMax):
    x = data.__numpy__[0]
    y = data.__numpy__[1]
    z = data.__numpy__[2]
    
    y = (y - yMin)/(yMax - yMin)*(gridY - 1)
    x = (x - xMin)/(xMax - xMin)*(gridX - 1)
    
    xspace = np.linspace(0, gridX, gridX)
    yspace = np.linspace(0, gridY, gridY)
    XI,YI = np.meshgrid(xspace,yspace)
    
    rbf = Rbf(x,y,z, function="linear")
    ZI = rbf(XI,YI)
    
    data.setResults(xspace.astype(np.float32),yspace.astype(np.float32),ZI.astype(np.float32))
    return data