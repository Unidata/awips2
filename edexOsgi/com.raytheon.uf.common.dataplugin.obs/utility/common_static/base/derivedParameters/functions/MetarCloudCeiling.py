##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract EA133W-17-CQ-0082 with the US Government.
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

#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- -----------------
# Nov 01, 2018  7314     bsteffen  Initial creation
#

import numpy

def execute(skyCover, skyLayerBase):
    stationCount = skyLayerBase.shape[0]
    levelCount = skyLayerBase.shape[1]
    cloud_ceiling = numpy.ndarray((stationCount, ), numpy.float32)
    for station_idx in range(stationCount):
        cc = 12000
        for level_idx in range(levelCount):
            cover = skyCover[level_idx, station_idx]
            if cover in ('BKN', 'OVC', 'VV'):
                cc = skyLayerBase[station_idx, level_idx]
                break
        cloud_ceiling[station_idx] = cc
    return cloud_ceiling
