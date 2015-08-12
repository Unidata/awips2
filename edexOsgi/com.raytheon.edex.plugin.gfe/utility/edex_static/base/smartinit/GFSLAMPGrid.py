################################################################################
#  This file contains entries for the 2.5km Gridded LAMP database initialization.
#
#  Author:  Joshua Watson					Created: 09/28/2011
#	    ERH					  	  Last Modified: 12/12/2014 
#

################################################################################
#  Import existing model database initialization parameters
from Init import *
class GFSLAMPGridForecaster(Forecaster): 
    def __init__(self): 
        Forecaster.__init__(self, "GFSLAMPGrid","GFSLAMPGrid") 

    def calcCigHgt(self, cc_CLG):
        ceil = cc_CLG / 0.3048 
        ceil[less(cc_CLG, 0)] = -99.
        return ceil

    def calcVis(self, vis_SFC):
        return (vis_SFC / 0.3048 / 5280.0)

    def calcT(self, t_FHAG2):
        return self.KtoF(t_FHAG2)

    def calcTd(self, dpt_FHAG2):
        return self.KtoF(dpt_FHAG2)

################################################################################
#  Set this file up to run with SmartInitialization

def main(): 
    GFSLAMPGridForecaster().run() 
