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

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

from Init import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from SPC model
## output.
##
##--------------------------------------------------------------------------
class SPCForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "SPC", "SPC")

##-------------------------------------------------------------------------
## Returns the SPC Fire Wx grids
##--------------------------------------------------------------------------
    def calcSPCDryTstmPrb(self, drytprob_SFC):
        return drytprob_SFC

    def calcFireWxProb(self, jfwprb_SFC):
        return jfwprb_SFC


def main():
    SPCForecaster().run()
