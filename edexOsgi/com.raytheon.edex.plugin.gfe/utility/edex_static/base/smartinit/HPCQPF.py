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
## Module that calculates surface weather elements from HPC QPF model output.
##
##--------------------------------------------------------------------------
class HPCQPFForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "HPCQPF", "HPCQPF")

##--------------------------------------------------------------------------
##  Calculates HPC QPF from the QPF model
##--------------------------------------------------------------------------
    def calcQPF(self, tpHPC_SFC):
        #  Assign the value filtering out everything above 1000 mm
        grid = where(greater(tpHPC_SFC, 1000), 0.0, tpHPC_SFC / 25.4)
        return clip(grid, 0, 5)  # clip at zero and 5 inches

def main():
    HPCQPFForecaster().run()

