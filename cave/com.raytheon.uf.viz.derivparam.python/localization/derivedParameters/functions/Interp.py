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
from scipy.interpolate import Rbf
from numpy import zeros, float32, NaN, isnan, array

##
# Designed to replace interp_up and interp_down in design files for point data
#
def execute(paramArray, vertArray, numLevels, vertPoints, maxGap=None):
    ret = zeros(numLevels.shape, float32)
    ret.fill(NaN)
    for i in range(len(vertArray)):
        verts = vertArray[i]
        if type(verts) == float32:
            verts = [verts]
        params = paramArray[i]
        # clone verts and params before modifying
        verts = array(verts)
        params = array(params)
        gi = 0
        for ci in range(len(verts)):
            if isnan(verts[ci]) or isnan(params[ci]):
                continue
            verts[gi] = verts[ci]
            params[gi] = params[ci]
            gi += 1
        if(gi == 0):
            ret[i] = NaN
            continue
        verts = verts[:gi]
        params = params[:gi]
        if isinstance(vertPoints, float):
            vertPoint = vertPoints
        elif isinstance(vertPoints, float32):
            vertPoint = vertPoints
        else:
            vertPoint = vertPoints[i]
        if (maxGap != None):
            below = None
            above = None
            gap = None
            match = None
            for point in verts:
                if (point > vertPoint and (above == None or above > point)):
                    above = point
                if (point < vertPoint and (below == None or below < point)):
                    below = point
                if (point == vertPoint):
                    match = point
            if(above == None and below == None):
                continue
            elif(above == None):
                gap = (vertPoint - below)*2
            elif(below == None):
                gap = (above - vertPoint)*2
            else:
                gap = above - below
            if((gap == None or gap > maxGap) and match == None):
                ret[i] = NaN
                continue
            # If withion gap and only 1 value just use it
            if(len(verts) == 1):
                ret[i] = params[0]
                continue
        if(len(verts) == 1):
            ret[i] = NaN
            continue
        rbf = Rbf(verts, params, function="linear")
        ret[i] = rbf(vertPoint)
    return ret
