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
# Calculate Specific Humidity (g/Kg) from Pressure, Temperature and 
# Relative Humidity.
# ----------------------------------------------------------------
from numpy import concatenate
from numpy import where
from numpy import equal


def execute(specHum, q2):
    q2 = q2.reshape(-1, 1);
    result = concatenate((q2, specHum), 1)
    return where(equal(result, -9999), -9999, result*1000)
