##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExSS1
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "QPF"
from numpy import *
import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, varDict):
        "This tool accesses QPF and tp  grids directly"

        #  determine siteid
        siteid = self.getSiteID()

        #  Get QPF and tp values
        qpf = self.getGrids("Fcst", "QPF", "SFC", GridTimeRange)
        tp = self.getGrids(siteid + "_D2D_NAM12", "tp","SFC", GridTimeRange)

        qpf = where(equal(qpf,0.0), tp, qpf)
        return qpf

