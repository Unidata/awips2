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

from numpy import add
from numpy import array
from numpy import zeros
from Magnitude import execute as Magnitude

def execute(uStk, vStk):
    res = zeros(uStk[0].shape, 'float32')
    for i in range(1, len(uStk)):
        u1 = uStk[i-1]
        v1 = vStk[i-1]
        u2 = uStk[i]
        v2 = vStk[i]
        res += Magnitude(u2-u1, v2-v1)
    return res
