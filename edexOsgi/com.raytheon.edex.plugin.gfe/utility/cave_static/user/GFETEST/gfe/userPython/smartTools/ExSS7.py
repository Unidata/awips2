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
# ExSS7
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "Wx"
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, PoP, Wx):
        # Assign Wx based on PoP

        # Separate Wx into components
        wxValues = Wx[0]
        keys = Wx[1]

        wxValues = where( less(PoP, 20), self.getIndex("<NoCov>:<NoWx>:<NoInten>:<NoVis>:",keys), wxValues)
        wxValues = where(logical_and( greater_equal(PoP, 20), less(PoP, 35)),  self.getIndex("Chc:R:-:<NoVis>:" ,keys), wxValues)
        wxValues = where(logical_and( greater_equal(PoP, 35), less(PoP, 55)),  self.getIndex("Sct:RW:m:<NoVis>:" ,keys), wxValues)
        wxValues = where(greater_equal(PoP, 55),  self.getIndex("Wide:R:+:<NoVis>:" ,keys), wxValues)

        return (wxValues, keys)
