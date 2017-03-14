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
from Init import *

##--------------------------------------------------------------------------
## Module that produces surface weather elements from SREF model
## output.
##
##--------------------------------------------------------------------------
class SREFForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "SREF")

##--------------------------------------------------------------------------
## These levels will be used to create vertical soundings.  These are
## defined here since they are model dependent.
##--------------------------------------------------------------------------
    def levels(self):
        return ["MB1000", "MB850", "MB700", "MB500", "MB250"]

    def calcT(self, tmean_FHAG2):
        return self.KtoF(tmean_FHAG2)

    def calcTd(self, dptmean_FHAG2):
        return self.KtoF(dptmean_FHAG2)

    def calcWind(self, uwmean_FHAG10, vwmean_FHAG10):
        uw = uwmean_FHAG10 * -1.0
        vw = vwmean_FHAG10 * -1.0
        (mag, dir) = self._getMD(uw, vw)
        mag = mag * 1.94
        dir = clip(dir, 0, 359.5)
        return (mag, dir)

def main():
    SREFForecaster().run()
