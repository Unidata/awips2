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
#HPCGuide Smart Init
#based on local smart init provided by B. Rasch (BYZ)

from Init import *
class HPCGuideForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "HPCGuide",  "HPCGuide")

    def calcMaxT(self, mxt_FHAG2):
        MaxT = self.KtoF(mxt_FHAG2)
        return MaxT

    def calcMinT(self, mnt_FHAG2):
        MinT = self.KtoF(mnt_FHAG2)
        return MinT

    def calcTd(self, dpt_FHAG2):
        Td = self.KtoF(dpt_FHAG2)
        return Td

    def calcWind(self, wind_FHAG10):
        mag, dir = wind_FHAG10
        mag = self.convertMsecToKts(mag)   # convert to knots from m/s
        return (mag, dir)

    def calcPoP(self, pop_SFC):
        return clip(pop_SFC, 0.0, 100.0)  # clip

    def calcSky(self, tcc_EA):
        return clip(tcc_EA, 0.0, 100.0)  # clip

def main():
    HPCGuideForecaster().run()