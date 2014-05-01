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
#Example file for adding a new model.  There are other steps that are
#necessary to make this work, such as adding new databases to your localConfig
#and setting up the init stream to recognize this file.  Refer to the 
#SmartInit documentation for details.

#This example will extract the WaveHeight and Wind information out of the
#GWW model and make an IFP database with that information.

from Init import *
class GWWForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "GWW", "GWW")

    def calcWaveHeight(self, htsgw_SFC):
        grid = where(greater(htsgw_SFC, 50), 0.0, htsgw_SFC/3.048)
        return clip(grid, 0, 100)

    def calcWind(self, wind_SFC):
        mag = where(greater(wind_SFC[0], 50), 0, wind_SFC[0]*1.94)
        dir = where(greater(wind_SFC[0], 50), 0, wind_SFC[1])
        dir = clip(dir, 0, 359.5)
        return (mag, dir)

def main():
    GWWForecaster().run()


