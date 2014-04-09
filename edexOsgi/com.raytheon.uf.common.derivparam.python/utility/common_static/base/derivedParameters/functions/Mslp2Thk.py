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
# This routine estimates 1000 to 500 mb thickness from 500 height
#  and mean sea level pressure.

def execute(mslp, GH):
        "Calculate thickness from mean sea level pressure and height."
        # Constants from Fortran code.
        # Wish these had more descriptive names; don't know what they are.
        a = 0.4599042
        b = 3.262312
        c = 0.1902672
        # calculate thickness
        dZ = GH * a
        denom = power(mslp, c)
        denom -= b
        dZ /= denom
        return dZ
