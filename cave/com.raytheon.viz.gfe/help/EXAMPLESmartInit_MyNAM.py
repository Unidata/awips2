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
# EXAMPLE OF MODIFYING AN EXISTING ALGORITHM, AND ADDING A NEW RH ALGORITHM
# FOR THE NAM12 MODEL.


from NAM12 import *

class MyNAM12Forecaster(NAM12Forecaster):
    def __init__(self):
        NAM12Forecaster.__init__(self)

    def calcSnowAmt(self, T, QPF):
        m2 = less_equal(T, 32)
        snowamt = where(m2, 10.0 * QPF, 0)
        return snowamt

    def calcRH(self, rh_FHAG2):
        return clip(rh_FHAG2, 0, 100)

def main():
    MyNAM12Forecaster().run()


