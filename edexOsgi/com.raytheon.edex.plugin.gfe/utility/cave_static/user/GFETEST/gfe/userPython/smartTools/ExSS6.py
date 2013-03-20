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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExSS6
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "None"
from numpy import *

ScreenList = ["T","Td"]

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, T, Td, GridTimeRange):
        "Creates a temporary element, TempRH"

        # Determine new value
        Tc = .556 * (T - 32.0)
        Tdc = .556 * (Td - 32.0)
        Vt = 6.11 * pow(10,(Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10,(Tdc * 7.5 / (Tdc + 237.3)))
        RH = (Vd / Vt) * 100.0

        # clip
        RH = clip(RH, 0.0, 100.0)

        # Create Element and add Grid
        self.createGrid("TempModel", "TempRH", "SCALAR", RH, 
          GridTimeRange, descriptiveName="TempRH", 
          timeConstraints=(0,3600,3600), minAllowedValue = 0.0,
          maxAllowedValue = 100.0, units="%")
