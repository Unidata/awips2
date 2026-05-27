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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 19, 2018  7271     randerso  Renamed and/or removed models
#
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

##
# This file contains entries for the 2.5km Gridded LAMP database
# initialization.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/28/2011                    Josh Watson    Initial Creation.
#    12/22/2015                    Josh Watson    
#
##



#  Import existing model database initialization parameters
from Init import *
class GLAMPForecaster(Forecaster): 
    def __init__(self): 
        Forecaster.__init__(self, "GLAMP","GLAMP") 

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

    def calcSky(self, tcc_SFC):
        return clip(tcc_SFC, 0, 100)

    def calcWind(self, wd_FHAG10, ws_FHAG10):
        spd = self.convertMsecToKts(ws_FHAG10)
        dir = wd_FHAG10
        return (spd, dir)

    def calcPoP(self, pop1hr_SFC):
        return pop1hr_SFC

    def calcPoP6(self, pop6hr_SFC):
        return pop6hr_SFC

    def calcPoP12(self, pop12hr_SFC):
        return pop12hr_SFC

################################################################################
#  Set this file up to run with SmartInitialization

def main(): 
    GLAMPForecaster().run() 
