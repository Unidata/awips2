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
# Smooth.py
#
# Author: njensen / chammack
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
##

# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Aug 24, 2021  21543    aghanava  Added optional smoothCount argument so smooth
#                                  operation can be repeated internally during a single
#                                  invocation.
# Aug 24, 2021  21543    zalberts  Added None check after parm is assigned.
#

ToolType = "numeric"
WeatherElementEdited = "variableElement"
from numpy import *
import math

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, WEname, editArea, GridTimeRange, varDict):
        parm = self.getParmByExpr(WEname)
        if parm is not None:

            time = GridTimeRange.startTime().javaDate()

            smoothCount = 1
            if varDict and "smoothCount" in varDict:
                smoothCount = int(varDict["smoothCount"])

            parm.smooth(time, editArea, smoothCount)

        return None
