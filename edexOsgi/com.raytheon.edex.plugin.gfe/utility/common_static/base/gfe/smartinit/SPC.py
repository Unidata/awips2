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
# Date          Ticket#   Engineer     Description
# ------------- --------  -----------  --------------------------------------------
#                                      Initial creation
# May 14, 2019  DCS 20864 MPorricelli  Added fire wx params; removed PTOR, PRSVR,
#                                      PRSIGV (per P. Jendrowski review)
# Feb 04, 2021  DCS 21298 MPorricelli  Added definitions from NIC v10.0
#
##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

from Init import *
import numpy as np

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from SPC model
## output.
##
##--------------------------------------------------------------------------
class SPCForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "SPC", "SPC")


    def calcCnvtvOutlooks(self, srcono_SFC):
        """
        Source grid contains discrete values as follows:
        0 - No Thunder
        2 - General Thunder
        3 - Marginal
        4 - Slight
        5 - Enhanced
        6 - Moderate
        8 - High
        """

        co = np.rint(srcono_SFC).astype("int8")
        co[co < 2] = 0
        co[(co >= 2) & (co <= 6)] -= 1
        co[co > 6] = 6
        key = ["NONE", "TSTM", "MRGL", "SLGT", "ENH", "MOD", "HIGH"]
        return (co, key)

    def calcProbDmgWind(self, windprob_SFC):
        return windprob_SFC.clip(0.0, 100.0)

    def calcProbExtrmDmgWind(self, sigwindprob_SFC):
        return sigwindprob_SFC.clip(0.0, 100.0)

    def calcProbExtrmHail(self, sighailprob_SFC):
        return sighailprob_SFC.clip(0.0, 100.0)

    def calcProbExtrmSvr(self, prsigsv_SFC):
        return prsigsv_SFC.clip(0.0, 100.0)

    def calcProbExtrmTor(self, sigtrndprob_SFC):
        return sigtrndprob_SFC.clip(0.0, 100.0)

    def calcProbSvrHail(self, hailprob_SFC):
        return hailprob_SFC.clip(0.0, 100.0)

    def calcProbTor(self, ptor_SFC):
        return ptor_SFC.clip(0.0, 100.0)

    def calcProbTotSvr(self, prsvr_SFC):
        return prsvr_SFC.clip(0.0, 100.0)

##-------------------------------------------------------------------------
## Returns the SPC Fire Wx grids
##--------------------------------------------------------------------------

    def calcSPCDryTstmPrb(self, drytprob_SFC):
        return drytprob_SFC

    def calcFireWxProb(self, jfwprb_SFC):
        return jfwprb_SFC


def main():
    SPCForecaster().run()
