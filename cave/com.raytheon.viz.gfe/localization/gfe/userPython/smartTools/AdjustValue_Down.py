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
# AdjustValue_Down.py
#
# Author: njensen
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "variableElement"
# All weather elements except Hazards and Wx
ScreenList = ["SCALAR", "VECTOR"]
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, variableElement, variableElement_DeltaValue):
        "Adjusts Value Down using Delta Value within Active Edit Area."
        
        if type(variableElement) is list:
            veMode = self.getVectorEditMode()
            # vector magnitude
            if "Both" == veMode or "Magnitude Only" == veMode:
                variableElement[0] = variableElement[0] - variableElement_DeltaValue
            if "Direction Only" == veMode:
                variableElement[1] -= float32(10.0)
                variableElement[1] %= 360
        else:        
            # scalar
            variableElement = variableElement - variableElement_DeltaValue
            
        return variableElement
    