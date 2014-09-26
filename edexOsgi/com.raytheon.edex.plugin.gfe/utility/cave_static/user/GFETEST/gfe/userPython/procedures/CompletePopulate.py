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
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CompletePopulate
#
# Author:
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

import SmartScript

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, editArea, timeRange, varDict):
        """Copy from models. """

        elements = ["T", "Td", "Wind", "Sky", "Wx", "PoP", "LAL",
                    "QPF", "LAL", "SnowAmt", "MaxT", "MinT",
                    "TransWind", "CWR", "SnowLevel", "FzLevel",
                    "Haines", "MixHgt"]
        #
        print "Copying grids from models"
        dbID = self.findDatabase("DGEX")
        tr = self.createTimeRange(0, 168, "Database", dbID)
        self.copyCmd(elements, dbID, tr)
        #  
        dbID = self.findDatabase("GFS80")
        tr = self.createTimeRange(0, 168, "Database", dbID)
        self.copyCmd(elements, dbID, tr)
        # 
        dbID = self.findDatabase("NAM12")
        tr = self.createTimeRange(0, 60, "Database", dbID)
        self.copyCmd(elements, dbID, tr)
        #Copy 
        dbID = self.findDatabase("RUC80")
        tr = self.createTimeRange(0, 12, "Database", dbID)
        self.copyCmd(elements, dbID, tr)

        # Create the rest of the data from scratch
        timeRange = self.createTimeRange(-24, 168, "LT")

        print "Creating grids from scratch"
        elements1 = ['RH', 'WaveHeight', 'WindWaveHgt', 'Swell',
                     'Swell2','Period', 'Period2', 'WindGust']
        for element in elements1:
            self.createFromScratchCmd([element], timeRange, 6, 6)
        
        elements2 = ['HrsOfSun', 'InvBurnOffTemp', 'MaxRH', 'MinRH',
                     'RHtrend', 'Ttrend', 'Wetflag', 'WindChill', 'HeatIndex']
        for element in elements2:
            self.createFromScratchCmd([element], timeRange, 0, 0)

        # Create a Hazards grid
        hazKeys = []
        value = self.getIndex("CF.Y^FF.A^HF.W^HI.A^FW.W^TO.A:1234^BZ.W", hazKeys)
        grid = self._empty + value
        todayTR = self.getTimeRange("Today")
        self.createGrid("Fcst", "Hazards", "DISCRETE", (grid, hazKeys), todayTR)

        print "Interpolating"
        # Interpolate
        interpElements = ['T', 'Wind', 'Sky', 'TransWind']
        self.interpolateCmd(interpElements, timeRange, "GAPS","SYNC", 3)

        # Make data for the other elements
        print "Calling WindGustTool"
        self.callSmartTool("WindGustTool", "WindGust", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling WindChillTool"
        self.callSmartTool("WindChillTool", "WindChill", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling HeatIndexTool"
        self.callSmartTool("HeatIndexTool", "HeatIndex", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling RHTool"
        self.callSmartTool("RHTool", "RH", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling MinRHTool"
        self.callSmartTool("MinRHTool", "MinRH", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling MaxRHTool"
        self.callSmartTool("MaxRHTool", "MaxRH", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling TtrendTool"
        self.callSmartTool("TtrendTool", "Ttrend", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling RHtrendTool"
        self.callSmartTool("RHtrendTool", "RHtrend", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling WetflagTool"
        self.callSmartTool("WetflagTool", "Wetflag", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling HrsOfSunTool"
        self.callSmartTool("HrsOfSunTool", "HrsOfSun", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling InvBurnOffTempTool"
        self.callSmartTool("InvBurnOffTempTool", "InvBurnOffTemp", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling SwellTool"
        self.callSmartTool("SwellTool", "Swell", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling Swell2Tool"
        self.callSmartTool("Swell2Tool", "Swell2", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling PeriodTool"
        self.callSmartTool("PeriodTool", "Period", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling Period2Tool"
        self.callSmartTool("Period2Tool", "Period2", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling WindWaveHgtTool"
        self.callSmartTool("WindWaveHgtTool", "WindWaveHgt", editArea, timeRange,
                           missingDataMode="Create")
        print "Calling WaveHeightTool"
        self.callSmartTool("WaveHeightTool", "WaveHeight", editArea, timeRange,
                           missingDataMode="Create")
        print "CompletePopulate Done"

        



