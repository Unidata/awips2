# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CopyRipRunupProbs - Version 1.0
#
# Author: S Kennedy/WFO MHX
#
# Copy all 144 hours of RipProb, ErosionProb and OverwashProb grids from 
# nwpsCG1### to the Fcst database, where ### is the WFO ID.
#
# SOFTWARE HISTORY
#  Date        Ticket#    Engineer    Description
#  ----------- ---------- ----------- --------------------------
#  10/24/2019  DCS21768   psantos     NWPS v1.3: add six new water elements
#
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

import SmartScript

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):

        wfo = self.getSiteID()

        model = "nwpsCG1" + wfo

        db = self.findDatabase(model)

        timeRange = self.createTimeRange(0, 144, "Database", db)

        self.copyCmd(['RipProb'], db, timeRange)
        self.copyCmd(['ErosionProb'], db, timeRange)
        self.copyCmd(['OverwashProb'], db, timeRange)
        self.copyCmd(['TwlWaves'], db, timeRange)
        self.copyCmd(['RunUp'], db, timeRange)
        self.copyCmd(['SetUp'], db, timeRange)
        self.copyCmd(['Swash'], db, timeRange)
        self.copyCmd(['TwlDT'], db, timeRange)
        self.copyCmd(['TwlDC'], db, timeRange)
