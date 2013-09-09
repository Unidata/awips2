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
# Assign_Value.py
#
# Author: njensen
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "variableElement"
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, variableElement, variableElement_PickUpValue):
        "Assign the value for the Active Weather Element within the Active Edit Area."
        
        if type(variableElement_PickUpValue) is list:            
            # for vector data
            variableElement[0].fill(variableElement_PickUpValue[0])
            variableElement[1].fill(variableElement_PickUpValue[1])
        elif type(variableElement_PickUpValue) is str:
            # for discrete/wx data            
            newval = self.getIndex(variableElement_PickUpValue, variableElement[1])
            variableElement[0].fill(newval)
        else:
            # for scalar data
            variableElement.fill(variableElement_PickUpValue)
        return variableElement
    