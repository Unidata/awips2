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

from numpy import power, piecewise

T0 = 288.0
gamma = 0.0065
p0 = 1013.2
c1 = 5.256
c2 = 14600
z11 = 11000
p11 = 226.0971

# Routine to calculate pressure from height based on a standard
#  atmosphere
def execute(z):
    return piecewise(z, [z < z11, z >= z11], [lambda z: p0*power((T0-gamma*z)/T0,c1), lambda z: p11*power(10, (z11-z)/c2)])
