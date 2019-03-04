##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ExSS2
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "QPF"
from numpy import *
import SmartScript

VariableList = [("Model:" , "", "D2D_model")]
class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, varDict):
        "This tool accesses QPF and tp  grids directly"

        model = varDict["Model:"]

        #  Get QPF and tp values
        qpf = self.getGrids("Fcst", "QPF", "SFC", GridTimeRange)
        tp = self.getGrids(model, "tp","SFC", GridTimeRange)

        qpf = where(equal(qpf,0.0), tp, qpf)
        return qpf
