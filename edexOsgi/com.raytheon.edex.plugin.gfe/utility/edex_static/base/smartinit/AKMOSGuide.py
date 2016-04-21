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
# AK MOS Guide Grids from Decoder/ingest:
# Maximum temperature (F)
# Minimum temperature (F)
# 2-m temperature (F)
# 2-m dew point (F)
# Relative humidity (%)
# Wind direction (deg)
# Wind speed (kts)
# 6-hour probability of precipitation (%)
# 12-hour probability of precipitation (%)

from Init import *
class AKMOSGuideForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "MOSGuide", "MOSGuide")

    def calcT(self, t_FHAG2):
        return self.KtoF(t_FHAG2)

    def calcTd(self, dpt_FHAG2):
        return self.KtoF(dpt_FHAG2)

    def calcRH(self, rh_FHAG2):
        return rh_FHAG2

    def calcMaxT(self, mxt_FHAG2):
        return self.KtoF(mxt_FHAG2)

    def calcMinT(self, mnt_FHAG2):
        return self.KtoF(mnt_FHAG2)

    def calcPoP6(self, pop6hr_SFC):
        return pop6hr_SFC

    def calcPoP12(self, pop12hr_SFC):
        return pop12hr_SFC

#    def calcQPF6(self, tp6hr_SFC):
#        return (tp6hr_SFC / 25.4)

#    def calcQPF12(self, tp12hr_SFC):
#        return (tp12hr_SFC / 25.4)

    def calcWind(self, ws_FHAG10, wd_FHAG10):
        return (ws_FHAG10 * 1.94, wd_FHAG10)

#    def calcWindGust(self, wgs_FHAG10):
#        return (wgs_FHAG10 * 1.94)

#    def calcSky(self, tcc_SFC):
#        return tcc_SFC
       
#    def calcTstmPrb12(self, thp12hr_SFC):
#        return thp12hr_SFC

#    def calcTstmPrb6(self, thp6hr_SFC):
#        return thp6hr_SFC

#    def calcTstmPrb3(self, thp3hr_SFC):
#        return thp3hr_SFC

def main():
    AKMOSGuideForecaster().run()
