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
# Returns waveHeight if it exists, otherwise return windWaveHeight
# 
# ----------------------------------------------------------------
import numpy
from numpy import concatenate
from numpy import isnan

# select waveHeight if it exists, otherwise return windWaveHeight
def combineHeights(waveHeight, windWaveHeight):
    return numpy.where(isnan(waveHeight), windWaveHeight, waveHeight)
    
##
# Returns waveHeight if it exists, otherwise return windWaveHeight
#
# @param waveHeight : waveHeight
# @param windWaveHeight : windWaveHeight
# @return:
# @rtype:
# 
def execute1(waveHeight,windWaveHeight):
    return combineHeights(waveHeight,windWaveHeight)
