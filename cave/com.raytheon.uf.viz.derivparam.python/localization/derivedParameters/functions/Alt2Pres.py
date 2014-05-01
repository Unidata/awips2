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
###

from numpy import power

##
# Routine to pressure from elevation and altimeter setting.
#
# @param alt: Altimeter setting (X)
# @param z: Elevation in meters.
# @return: Pressure (X)

def execute(alt, z):
    T0 = 288.0
    gamma = 0.0065
    g_Rgamma = 5.2532
    result = (T0-gamma*z)/T0
    result = power(result, g_Rgamma)
    result = alt*result
    return result
