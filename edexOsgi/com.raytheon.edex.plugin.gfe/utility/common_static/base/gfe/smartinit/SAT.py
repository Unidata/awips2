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

#
# Smart Init for satellite data
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
#                                  Initial creation
# Jul 20, 2018  7310     mapeters  Add GOES-R support
#
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

from Init import *

class SATForecaster(Forecaster):

##-------------------------------------------------------------------------
##  ifpInit module to calculate surface weather values from satellite data
##-------------------------------------------------------------------------
    def __init__(self):
        Forecaster.__init__(self, "Satellite", "SAT")

    def gvar2T(self, ir):
        t = where(ir < 177, (660 - ir) / 2.0, (418 - ir))
        t = t - 273
        return t

    def decimalToPercent(self, decimal):
        return decimal * 100

    def kelvinToCelsius(self, kelvin):
        return kelvin - 273.15

    # Legacy GOES

    def calcIR11E(self, ir11East_SFC):
        return self.gvar2T(ir11East_SFC)

    def calcIR13E(self, ir13East_SFC):
        return self.gvar2T(ir13East_SFC)

    def calcIR39E(self, ir39East_SFC):
        return self.gvar2T(ir39East_SFC)

    def calcWaterVaporE(self, waterVaporEast_SFC):
        return self.gvar2T(waterVaporEast_SFC)

    def calcFogE(self, IR11E, IR39E):
        return IR11E - IR39E

    def calcVisibleE(self, visibleEast_SFC):
        return visibleEast_SFC

    def calcIR11W(self, ir11West_SFC):
        return self.gvar2T(ir11West_SFC)

    def calcIR13W(self, ir13West_SFC):
        return self.gvar2T(ir13West_SFC)

    def calcIR39W(self, ir39West_SFC):
        return self.gvar2T(ir39West_SFC)

    def calcWaterVaporW(self, waterVaporWest_SFC):
        return self.gvar2T(waterVaporWest_SFC)

    def calcFogW(self, IR11W, IR39W):
        return IR11W - IR39W

    def calcVisibleW(self, visibleWest_SFC):
        return visibleWest_SFC

    # GOES-R series

    def calcBlueVisibleBand047umE(self, blueVisibleBand047umEast_SFC):
        return self.decimalToPercent(blueVisibleBand047umEast_SFC)

    def calcRedVisibleBand064umE(self, redVisibleBand064umEast_SFC):
        return self.decimalToPercent(redVisibleBand064umEast_SFC)

    def calcVegetationNIRBand086umE(self, vegetationNIRBand086umEast_SFC):
        return self.decimalToPercent(vegetationNIRBand086umEast_SFC)

    def calcCirrusNIRBand137umE(self, cirrusNIRBand137umEast_SFC):
        return self.decimalToPercent(cirrusNIRBand137umEast_SFC)

    def calcSnowIceNIRBand161umE(self, snowIceNIRBand161umEast_SFC):
        return self.decimalToPercent(snowIceNIRBand161umEast_SFC)

    def calcCloudParticleSizeNIRBand224umE(self, cloudParticleSizeNIRBand224umEast_SFC):
        return self.decimalToPercent(cloudParticleSizeNIRBand224umEast_SFC)

    def calcShortwaveWindowIRBand390umE(self, shortwaveWindowIRBand390umEast_SFC):
        return self.kelvinToCelsius(shortwaveWindowIRBand390umEast_SFC)

    def calcUpperLevelWaterVaporIRBand619umE(self, upperLevelWaterVaporIRBand619umEast_SFC):
        return self.kelvinToCelsius(upperLevelWaterVaporIRBand619umEast_SFC)

    def calcMidLevelWaterVaporIRBand693umE(self, midLevelWaterVaporIRBand693umEast_SFC):
        return self.kelvinToCelsius(midLevelWaterVaporIRBand693umEast_SFC)

    def calcLowLevelWaterVaporIRBand734umE(self, lowLevelWaterVaporIRBand734umEast_SFC):
        return self.kelvinToCelsius(lowLevelWaterVaporIRBand734umEast_SFC)

    def calcCloudTopPhaseIRBand844umE(self, cloudTopPhaseIRBand844umEast_SFC):
        return self.kelvinToCelsius(cloudTopPhaseIRBand844umEast_SFC)

    def calcOzoneIRBand961umE(self, ozoneIRBand961umEast_SFC):
        return self.kelvinToCelsius(ozoneIRBand961umEast_SFC)

    def calcCleanWindowIRBand1033umE(self, cleanWindowIRBand1033umEast_SFC):
        return self.kelvinToCelsius(cleanWindowIRBand1033umEast_SFC)

    def calcLegacyWindowIRBand1121umE(self, legacyWindowIRBand1121umEast_SFC):
        return self.kelvinToCelsius(legacyWindowIRBand1121umEast_SFC)

    def calcDirtyWindowIRBand1229umE(self, dirtyWindowIRBand1229umEast_SFC):
        return self.kelvinToCelsius(dirtyWindowIRBand1229umEast_SFC)

    def calcCarbonDioxideIRBand1328umE(self, carbonDioxideIRBand1328umEast_SFC):
        return self.kelvinToCelsius(carbonDioxideIRBand1328umEast_SFC)

    # def calcFogE(self, ...):

    def calcBlueVisibleBand047umW(self, blueVisibleBand047umWest_SFC):
        return self.decimalToPercent(blueVisibleBand047umWest_SFC)

    def calcRedVisibleBand064umW(self, redVisibleBand064umWest_SFC):
        return self.decimalToPercent(redVisibleBand064umWest_SFC)

    def calcVegetationNIRBand086umW(self, vegetationNIRBand086umWest_SFC):
        return self.decimalToPercent(vegetationNIRBand086umWest_SFC)

    def calcCirrusNIRBand137umW(self, cirrusNIRBand137umWest_SFC):
        return self.decimalToPercent(cirrusNIRBand137umWest_SFC)

    def calcSnowIceNIRBand161umW(self, snowIceNIRBand161umWest_SFC):
        return self.decimalToPercent(snowIceNIRBand161umWest_SFC)

    def calcCloudParticleSizeNIRBand224umW(self, cloudParticleSizeNIRBand224umWest_SFC):
        return self.decimalToPercent(cloudParticleSizeNIRBand224umWest_SFC)

    def calcShortwaveWindowIRBand390umW(self, shortwaveWindowIRBand390umWest_SFC):
        return self.kelvinToCelsius(shortwaveWindowIRBand390umWest_SFC)

    def calcUpperLevelWaterVaporIRBand619umW(self, upperLevelWaterVaporIRBand619umWest_SFC):
        return self.kelvinToCelsius(upperLevelWaterVaporIRBand619umWest_SFC)

    def calcMidLevelWaterVaporIRBand693umW(self, midLevelWaterVaporIRBand693umWest_SFC):
        return self.kelvinToCelsius(midLevelWaterVaporIRBand693umWest_SFC)

    def calcLowLevelWaterVaporIRBand734umW(self, lowLevelWaterVaporIRBand734umWest_SFC):
        return self.kelvinToCelsius(lowLevelWaterVaporIRBand734umWest_SFC)

    def calcCloudTopPhaseIRBand844umW(self, cloudTopPhaseIRBand844umWest_SFC):
        return self.kelvinToCelsius(cloudTopPhaseIRBand844umWest_SFC)

    def calcOzoneIRBand961umW(self, ozoneIRBand961umWest_SFC):
        return self.kelvinToCelsius(ozoneIRBand961umWest_SFC)

    def calcCleanWindowIRBand1033umW(self, cleanWindowIRBand1033umWest_SFC):
        return self.kelvinToCelsius(cleanWindowIRBand1033umWest_SFC)

    def calcLegacyWindowIRBand1121umW(self, legacyWindowIRBand1121umWest_SFC):
        return self.kelvinToCelsius(legacyWindowIRBand1121umWest_SFC)

    def calcDirtyWindowIRBand1229umW(self, dirtyWindowIRBand1229umWest_SFC):
        return self.kelvinToCelsius(dirtyWindowIRBand1229umWest_SFC)

    def calcCarbonDioxideIRBand1328umW(self, carbonDioxideIRBand1328umWest_SFC):
        return self.kelvinToCelsius(carbonDioxideIRBand1328umWest_SFC)

    # def calcFogW(self, ...):

def main():
    SATForecaster().run()
