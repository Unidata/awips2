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

def main():
    SATForecaster().run()
