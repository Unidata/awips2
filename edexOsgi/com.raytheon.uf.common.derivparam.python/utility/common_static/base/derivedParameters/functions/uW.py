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

import gridslice
from numpy import concatenate
from numpy import zeros
from numpy import where
from Average import execute as Average

def execute1(uComp, u10):
    u10 = u10.reshape(-1, 1);
    return concatenate((u10, uComp), 1)

def execute3(vwpU):
    return concatenate((zeros([vwpU.shape[0],1], 'float32'), vwpU), 1)

