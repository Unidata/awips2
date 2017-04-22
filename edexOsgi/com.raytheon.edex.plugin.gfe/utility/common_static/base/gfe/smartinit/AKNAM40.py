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
from NAM40 import *

class AKNAM40Forecaster(NAM40Forecaster):

    # Alaska does not set most of the boundry layer temps
    def calcT(self, t_FHAG2, t_BL030, p_SFC, topo, stopo, gh_c, t_c):
        p_SFC = p_SFC / 100  # get the surface pres. in mb
        pres = [p_SFC, p_SFC - 15]
        return self._calcT([t_FHAG2, t_BL030], pres, topo, stopo, gh_c, t_c)

    # Alaska does not get pop_SFC
    def calcPoP(self, gh_c, rh_c, QPF, topo):
        rhavg = where(less(gh_c, topo), float32(-1), rh_c)
        rhavg[greater(gh_c, topo + 5000 * 0.3048)] = -1 
        count = where(not_equal(rhavg, -1), float32(1), float32(0))
        rhavg[equal(rhavg, -1)] = 0 
        count = add.reduce(count, 0)
        rhavg = add.reduce(rhavg, 0)
        ## add this much based on humidity only
        dpop = where(count, rhavg / (count + .001), float32(0)) - 70.0
        dpop[less(dpop, -30)] = -30 
        ## calculate the base PoP
        pop = where(less(QPF, 0.02), QPF * 1000, QPF * 350 + 13)
        pop += dpop            # add the adjustment based on humidity
        pop.clip(0, 100, pop)  # clip to 100%
        return pop

def main():
    AKNAM40Forecaster().run()