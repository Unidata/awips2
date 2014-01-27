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
class MSASForecaster(Forecaster):
     def __init__(self):
         Forecaster.__init__(self, "MSAS", "MSAS")

     def calcT(self, pot_SFC, alti_SFC, stopo):
         topoft = stopo * 3.2808
         altimb = alti_SFC / 100.0
         tsl = 288.0 * power((altimb / 1013.25), 0.190357143)
         tst = (6.5 * topoft) / 3280.8
         psfcmb = altimb * exp(log((tsl - tst) / tsl) * 5.253283302)
         tsfck = pot_SFC * pow((psfcmb / 1000.0), 0.287)
         return self.KtoF(tsfck)

     def calcTd(self, dpt_SFC):
         return self.KtoF(dpt_SFC)

     def calcWind(self, wind_SFC):
         mag = wind_SFC[0]
         dir = wind_SFC[1]
         mag = mag * 1.94
         dir = clip(dir, 0, 359.5)
         return (mag, dir)

def main():
     MSASForecaster().run()
