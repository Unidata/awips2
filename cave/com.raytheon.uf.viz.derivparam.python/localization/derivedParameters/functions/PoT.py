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


from numpy import power
    
##
# Calculate theta from temperature and pressure.
# This function accepts numpy arrays
#
# @param P: Pressure in millibars
# @param T: Temperature in degrees Kelvin
# @return: Potential temperature in degrees Kelvin
# 
def execute(P, T):
    "Calculate theta (PoT) from temperature and pressure."

    pComponent = 1000.0/P
    pComponent = power(pComponent, 0.286)
    theta = T * pComponent
    # convert theta to an unmasked array with 1e37 for invalid values
    return theta
