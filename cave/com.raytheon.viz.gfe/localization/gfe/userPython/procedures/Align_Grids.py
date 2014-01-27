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
# Align_Grids
#
# Author: Paul Jendrowski
# Version: 1.0 - 11/08/2004
#
# Description:
# This procedure will align the time ranges of selected
# weather elements with the time range of another element.
# For instance, this will redo the PoP grid to match
# the timeranges of individual Wx grids. Note that WindGust
# is always forced to match the Wind grid.
#
# If a grid does not exist, then it will be created from scratch.
# If the grid to align does exist, the max value of all original
# grids that are within the time range of the source grid is
# used for PoP, Sky, WindGust. Rate based parameters
# (QPF, SnowAmt, etc.) are fragmented then summed.
#
# You can add whatever elements you wish to VariableList. However,
# you must add an appropriate entry in the __init__ method
# to defing if the variable needs to be fragmented (i.e., rate
# based parameters like QPF must be fragmented to sum correctly),
# split (generally all non-rate based should be split before
# being recombined into the aligned grids). _methdodDict
# defines how to recompute the value for the aligned grid from
# the original grid.  This value is any value that can be used
# as the mode argument to SmartScript.getGrids method. In addition,
# you can also use "Last" to get the last grid or "MaxTime" which
# will select the grid that has the highest percentage time in the
# period (if a tie for highest percentage, the first is used).
# method is generally for Wx/discrete grids that cannot be
# averaged or summed.  Finally, the execute method needs to be
# edited to define time ranges to use for each new element.
#
# Installation:
#   Install this file as a Procedure. This procedure also requires
#   getGridsTool and MakeTmpGrid SmartTools to be installed.
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify
MenuItems = ["Edit"]

Supported_elements=["Wx", "PoP", "Sky", "QPF","SnowAmt","IceAccum","Wind","WindGust"]
VariableList = [
         ("Select Time Range:","All", "radio",
          ["Selected Time", "All"]),
         ("Source Grid:" , "Wx", "radio",["Wx", "PoP", "QPF"]),
         ("Aligned Grids:" , [], "check",Supported_elements),
        ]

import SmartScript
import TimeRange

import time

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        # These are the elements that need to be split before rederiving the
        # values in the newly aligned grids.
        self._splitElements = ["Wx", "PoP", "Sky","Wind", "WindGust"]
        
        # These are the elements that need to be fragmented before
        # rederiving the values in the newly aligned grids.
        self._fragmentElements = ["QPF", "SnowAmt","IceAccum"]
        
        # Method to use the populate values in the aligned grid from the
        # original values.
        #**** There must be a key for every element in the "Aligned Grids:"
        #**** VariableList entry!!!
        # This value is any value except "List" that can be used
        # as the mode argument to SmartScript.getGrids method. In addition,
        # you can also use "Last" to get the last grid or "MaxTime" which
        # will select the grid that has the highest percentage time in the
        # period (if a tie for highest percentage, the first is used).
        # method is generally for Wx/discrete grids that cannot be
        # averaged or summed.
        self._methodDict = {
                            "Wx" : "MaxTime",
                            "PoP" : "Max",
                            "Sky" : "Max",
                            "WindGust" : "Max",
                            "Wind" : "TimeWtAverage",
                            "QPF" : "Sum",
                            "SnowAmt" : "Sum",
                            "IceAccum" : "Sum",
                           }

    def execute(self, editArea, timeRange, varDict):
        # First make a list of grids to make from the selection
        # Get time range from varDict
        period = varDict["Select Time Range:"]
        srcGrid = varDict["Source Grid:"]
        gridsToMake = []

        doWindGust = 0
        for elem in varDict["Aligned Grids:"]:
            if elem == "WindGust":
                doWindGust = 1
            elif elem != srcGrid:
                gridsToMake.append(elem)

        # Get current Greenwich Mean Time (GMT)
        GMT = time.gmtime(time.time())

        # Get current hour from GMT
        gmthour = GMT[3]
        #print "GMT hour=", gmthour

        # Create the timeranges over which to create the grids from scratch
        # (timerange varies by element and initial period)
        timeRangeDict = {}
        srcTR = self._getGridTimeRange("Fcst", srcGrid, "SFC", timeRange)
        #print "str=",timeRange, "SrcTR=",srcTR

#*** If and element is added to VariableList, you must set up the timeRangeDict
#*** for that element in the following if-else block

        if period == "Selected Time":
            # Set time range of grids that are actually there
            for elem in Supported_elements:
                timeRangeDict[elem] = srcTR
            timeRangeDict["WindGust"] = self._getGridTimeRange("Fcst", "Wind", "SFC", timeRange)
#            print "Selected tr=",timeRange
        else:
# 
# Here is where you can adjust the ending times of the preset time ranges used by the
# "All" option. To change these, simply modify the ending time of the following calls
# to createTimeRange to meet your local needs.
# 
            # First set a reasonable time range then determine which grids are
            # actually there. The start time is the current hour in GMT time.
            # Set up elements for full time range
            tr = self.createTimeRange(gmthour, 192, "Zulu")

            #for elem in ["Wx", "PoP", "Sky", "QPF","SnowAmt","IceAccum","Wind","WindGust"]:
            for elem in Supported_elements:
                timeRangeDict[elem] = self._getGridTimeRange("Fcst", srcGrid, "SFC", tr)
            
            # Override the default time ranges explicitly for selected elements
            tr = self.createTimeRange(gmthour, 60, "Zulu")
            timeRangeDict["SnowAmt"] = self._getGridTimeRange("Fcst", srcGrid, "SFC", tr)
            tr = self.createTimeRange(gmthour, 60, "Zulu")
            timeRangeDict["IceAccum"] = self._getGridTimeRange("Fcst", srcGrid, "SFC", tr)

            tr = self.createTimeRange(gmthour, 96, "Zulu")
            timeRangeDict["QPF"] = self._getGridTimeRange("Fcst", srcGrid, "SFC", tr)
            tr = self.createTimeRange(gmthour, 96, "Zulu")
            timeRangeDict["Wind"] = self._getGridTimeRange("Fcst", srcGrid, "SFC", tr)
            tr = self.createTimeRange(gmthour, 96, "Zulu")
            timeRangeDict["WindGust"] = self._getGridTimeRange("Fcst", "Wind", "SFC", tr)
        

        # Create temporary grid of each element to be aligned then remake
        # the grid from scratch with the time range from the source grid
        for elem in gridsToMake:
            #print "_alignGrid",elem,srcGrid,timeRangeDict[elem]
            self._alignGrid(elem,srcGrid,timeRangeDict[elem])
        if doWindGust:
            elem = "WindGust"
            #print "_alignGrid",elem,srcGrid,timeRangeDict[elem]
            self._alignGrid(elem,"Wind",timeRangeDict[elem])

    def _alignGrid(self,elem,srcGrid,timeRange):
        tmpName = "tmp" + elem
        varDict = {}
        varDict["Model"] = "Fcst"
        varDict["gridName"] = tmpName
        madeTmp=0
        # First check if any old grids exist
        elemGridInfo = self.getGridInfo("Fcst", elem, "SFC", timeRange)
        #print elem, len(elemGridInfo)
        if len(elemGridInfo) > 0:
            madeTmp = 1
            rslt = self.callSmartTool("MakeTmpGrid", elem, varDict=varDict,
                                  timeRange=timeRange)
            
        srcGridInfo = self.getGridInfo("Fcst", srcGrid, "SFC", timeRange)
        # Step through each of the source grids and create a scratch
        # grid with the same time range as the source grid.
        for info in srcGridInfo:
            #print info.gridTime()
            self.createFromScratchCmd([elem], info.gridTime(),  0, 0)
            # Check if the temporary version of the element needs
            # to be split. The temporary grid will be used to recompute
            # the values in the scratch grids
            if madeTmp and elem in self._splitElements:
                if len(self.getGridInfo("Fcst", tmpName, "SFC", info.gridTime())) > 0:
                    self.splitCmd([tmpName], info.gridTime())
        if madeTmp > 0:
            varDict["Element"] = tmpName
            if elem in self._fragmentElements:
                self.fragmentCmd([tmpName], timeRange)

            if self._methodDict.has_key(elem):
                varDict["Mode"] = self._methodDict[elem]
                self.callSmartTool("getGridsTool", elem, varDict=varDict,
                                   timeRange=timeRange)
            # Delete the temp grids
            #try:
            #    self.deleteObject(tmpName, "FcstGrid")
            #except:
            #    pass
            try:
                self.unloadWE("Fcst", tmpName, "SFC")
            except:
                pass

    def _getGridTimeRange(self, model,parm,level,timeRange):
    # Returns a timeRange covering any grids that intersect input timeRange
    # Returns a timeRange with duration less than 3600 if no grids found
        info = self.getGridInfo(model, parm, level, timeRange)
        if info != []:
            st = info[0].gridTime().startTime()
            et = info[len(info) - 1].gridTime().endTime()
            tr = TimeRange.TimeRange(st,et)
        else:
            tr = TimeRange.TimeRange(timeRange.startTime(),timeRange.startTime() + 1)
        return tr

## Error Handling
##   Call self.abort(errorString) to stop execution of your script and
##     display a message to the user.
##   For example:
##     if x > 1000:
##        self.abort("x is too large")
##
