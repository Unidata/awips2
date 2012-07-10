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
from WW3 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from WW3 model
## output.
##
##--------------------------------------------------------------------------
class WNAwaveForecaster(WW3Forecaster):
    def __init__(self):
        WW3Forecaster.__init__(self, "WNAwave", "WNAwave")

def main():
    WNAwaveForecaster().run()
