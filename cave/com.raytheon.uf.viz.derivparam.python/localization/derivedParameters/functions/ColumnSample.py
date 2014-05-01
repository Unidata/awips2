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
from numpy import zeros, float32, NaN, isnan

##
# Sample a column of profiler data in a way which matches A1
# The most notable feature is that A1 stores all profiler data at evenly spaced level
# where as A2 stores only the levels which have data. This function essentially
# reinserts the missing level and then selects the closest level
#
def execute(paramArray, vertArray, numLevels, vertPoints, sfcParams=None, sfcVerts=None):
    ret = zeros(numLevels.shape, 'float32')
    ret.fill(NaN)
    for i in range(len(vertArray)):
        count = numLevels[i]
        verts = vertArray[i][:count]
        params = paramArray[i][:count]
        if isinstance(vertPoints, float):
            vertPoint = vertPoints
        elif isinstance(vertPoints, float32):
            vertPoint = vertPoints
        else:
            vertPoint = vertPoints[i]
        if sfcParams == None:
            sfcParam = NaN
        elif isinstance(sfcParams, float):
            sfcParam = sfcParams
        elif isinstance(sfcParams, float32):
            sfcParam = sfcParams
        else:
            sfcParam = sfcParams[i]
        if sfcVerts == None:
            sfcVert = NaN
        elif isinstance(sfcVerts, float):
            sfcVert = sfcVerts
        elif isinstance(sfcVerts, float32):
            sfcVert = sfcVerts
        else:
            sfcVert = sfcVerts[i]
        # expand the input vertices to include surface and every 250 meters
        inVert = [sfcVert]
        inData = [sfcParam]
        for j in range(0,len(verts)):
            # Profiler data should be spaced every 250 from 500 to 9250, and then every 1000 above that
            heightAGL = verts[j] - sfcVert
            if heightAGL < 0:
                continue
            if heightAGL > 9250:
                index = 36 + int((verts[j] - sfcVert - 9250)/1000)
            else:
                index = int((verts[j] - sfcVert - 250)/250)
            while len(inVert) <= index:
                inData.append(NaN)
                if len(inVert) > 36:
                    inVert.append(9250 + sfcVert + 1000*(len(inVert) - 36))
                else:
                    inVert.append(250 + sfcVert + 250*len(inVert))
            inVert[index] = verts[j]
            inData[index] = params[j]
        # add a NaN on the top to prevent interpolating over.
        inData.append(NaN)
        if len(inData) > 36:
            inVert.append(9250 + sfcVert + 1000*(len(inVert) - 36))
        else:
            inVert.append(250 + sfcVert + 250*len(inVert))
		#grab the closest level
        bestDist = 999999
        for j in range(0,len(inVert)):
			dist = abs(inVert[j]-vertPoint)
			if dist<bestDist:
				bestDist = dist
				ret[i] = inData[j];
    return ret