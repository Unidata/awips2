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
# PopulateFromClimo
#
# This procedure calculates MinT or MaxT grids based on the NCDC or
# PRISM climatology grids stored in a netCDF file.  This file must be
# present for this procedure to work.
#
# Author: lefebvre
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import time

from numpy import *

import SmartScript
import TimeRange


VariableList = [("Weather Element:" , "MaxT", "radio", ["MaxT", "MinT"]),
                ("Climo Source:" , "PRISM", "radio", ["PRISM", "NCDC"]),
                ]
MenuItems = ["Populate"]


MODEL = "Fcst"
LEVEL = "SFC"

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
    ##
    # Get the list of time ranges at the grid whose element name is WEName
    # contains grids. The level of the weather element is assumed to be LEVEL.
    #
    # @param dbName: Name of the database to consult
    # @type dbName: string
    # @param WEName: Name of a weather element
    # @type WEName: string
    # @param timeRange: Limits of time range of interest, or None for all times
    # @type timeRange: Java TimeRange or Nonetype
    # @return: time ranges at which WEName has data.
    # @rtype: Python list of Python TimeRange objects
    def _getWEInventory(self, dbName, WEName, timeRange=None):
        # set up a timeRange if it is None
        if timeRange is None:
            timeRange = TimeRange.allTimes()
        parm = self.getParm(dbName, WEName, LEVEL)
        if parm is None:
            return []
        inv = parm.getGridInventory(timeRange.toJavaObj())
        if inv is None:
            self.statusBarMsg("inv is None","S")
        elif len(inv)==0:
            print(self.statusBarMsg("PFC: len(inv)==0","S"))
        trList = []
        for gd in inv:
            tr = TimeRange.TimeRange(gd.getGridTime())
            trList.append(tr)
        return trList

    # Main cubic spline method that accepts a list of grids and int time
    # along with a list of times for which grids are to be calculated.
    # This method returns the corresponding list of grids that matches the
    # interpTimes list.
    def _cubicSpline(self, grids, times, interpTimes):
            
        # STEP 1: Create coefficients for cubic spline curve
        # zCoefs : List of cubic spline coefficient grids computed to fit the
        #          curve defined by grids and times
        # n      : length of grids - 1.  
        # Determine coefficients
        if grids == []:
            self.statusBarMsg("No grids sent to _cublicSpline. No grids returned", "S")
            return
        gridShape = shape(grids[0])

        timeGrids = []
        for t in times:
            tGrid = zeros(gridShape) + t
            timeGrids.append(tGrid)
        
        n = len(grids) - 1
        zCoefs = self._spline3_coef(n, timeGrids, grids)
        
        # Create interpolated grids using coefficients
        # interpTimes : List of times for which we want interpolated grids
        # gridList : List of interpolated Grids
           
        # Create interpolated grids
        gridList = []
        for interpTime in interpTimes:
            x = zeros(gridShape) + interpTime  # make a grid of times
            xGrid = self._spline3_eval(n, timeGrids, grids, zCoefs, x)
            gridList.append(xGrid)

        return gridList

    # This method calculates the spline coefficients that are later used to
    # calculate grids at the interpolation times.  This method is just a helper
    # method to _cubicSpline and should not be called directly.
    def _spline3_coef(self, n, t, y):
        gridShape = y[0].shape
        # These will get filled in later with grids as values
        # They are just place holders
        h=[0] * n
        b=[0] * n
        u=[0] * n
        v=[0] * n
        z=[0] * (n+1)                
        # Calculate h and b
        #   range 0 thru n-1
        for i in range(n):
            h[i] = t[i+1] - t[i]
            b[i] = (y[i+1] - y[i])/h[i]
        # Calculate u and v as functions of h and b
        #   range 1 thru n-1
        u[1] = (2*(h[0] + h[1]))
        v[1] = (6*(b[1]-b[0]))
        for i in range(2, n):
            u[i] = (2.0*(h[i]+h[i-1]) - h[i-1].astype(float32)**2.0/u[i-1])
            v[i] = (6.0*(b[i]-b[i-1]) - h[i-1]*v[i-1]/u[i-1])
        # Calculate z
        #   range 0 thru n
        z[n] = zeros(gridShape)
        for i in range(n-1, 0, -1):
            z[i] = (v[i] - h[i]*z[i+1])/u[i]
        z[0] = zeros(gridShape)
        return z

    # This method accepts the spline coefficients and calculates a grid.
    # This method is a help method to _cubicSpline and should not be
    # called directly
    def _spline3_eval(self, n, t, y, z, x):
        for i in range(n-1, 0, -1):
            if x[0][0]-t[i][0][0] >= 0:
                break
        h = t[i+1]-t[i]
        tmp = (z[i]/2) + (x-t[i]) * (z[i+1]-z[i])/(6*h)
        tmp = -(h/6)*(z[i+1]+2*z[i]) + (y[i+1]-y[i])/h + (x-t[i]) * tmp

        return y[i] + (x-t[i]) * tmp

    def parmIsLoaded(self, weName):
        tupleList = self.loadedParms()   # list of all loaded parms
        for element, level, databaseID in tupleList:
            modelName = databaseID.modelName()
            if element == weName and level == "SFC" and modelName == "Fcst":
                return 1

        # if we got this far we didn't find it.
        return 0
        

    # This main method retrieves the climatology grids, assigns
    # appropriate times to each and calls the _cubicSpline method
    # to calculate the grid values inbetween the given climatology
    # grids.  This methods creates grids of MinT or MaxT over the
    # timeRange selected in the GridManager.
    def execute(self, timeRange, varDict):

        # get the climo source
        parmName= varDict["Weather Element:"]
        climoSource = varDict["Climo Source:"]

        if not self.parmIsLoaded(parmName):
            self.statusBarMsg("You must load the " + parmName +
                              " element before you can populate it.", "S")
            return   # can't go on            


        # get times for all the grids that overlap the selected time range
        hours = timeRange.duration() // 3600
        someTimeRange, gridTimes = self.getGridTimes("Fcst", parmName, "SFC",
                                                     timeRange.startTime(), hours)

        if len(gridTimes) == 0:
            self.statusBarMsg("Please select a MinT or MaxT timeRange to populate.", "S")
            return   # can't go on            
        
        # make a list of AbsTimes from the parmName times
        interpTimes = []
        for g in gridTimes:
            interpTimes.append(g.startTime().unixTime())

        siteID = self.getSiteID()
        # get all of the grids from the climo database
        dbName = siteID + "_D2D_" + climoSource + "Climo"

        if parmName == "MaxT":
            weName = "mxt"
        elif parmName == "MinT":
            weName = "mnt"
        else:
            self.statusBarMsg("Invalid parmName:" + parmName, "S")
            return

        # get the climo grid inventory
        trList = self._getWEInventory(dbName, weName)
        if len(trList) == 0:
            self.statusBarMsg("No climatology grids available for " + parmName, "S")
            return   # can't go on

        # Figure out what year it is

        currentTime = self._gmtime().unixTime()
        jan01Tuple = (time.gmtime(currentTime)[0],1,1,0,0,0,0,0,0) # 01 Jan this year
        jan01Secs = time.mktime(jan01Tuple) # 01 Jan in seconds

        # Fetch the grids from the climo database, but warp the times
        # so that they are set to this year.
        gridList = []
        times = []
        for tr in trList:
            grid = self.getGrids(dbName, weName, "SFC", tr)
            gridList.append(grid)
            times.append(tr.startTime().unixTime() + jan01Secs)

        # tack on the Dec. at the beginning and the Jan at the end so
        # calculations from Dec 15 to Jan 15 are correct.
        gridList.insert(0, gridList[-1])  # prepend the last grid
        gridList.append(gridList[1])  # append what was the first grid

        days31 = 31 * 24 * 3600  # the number of seconds in 31 days
        times.insert(0, times[0] - days31)  # 15 Dec the previous year
        times.append(times[-1] + days31)  # 15 Jan the next year

        interpGrids = self._cubicSpline(gridList, times, interpTimes)

        parm = self.getParm("Fcst", parmName, "SFC")
        maxLimit = parm.getGridInfo().getMaxValue()
        minLimit = parm.getGridInfo().getMinValue()

        for i in range(len(gridTimes)):
            # convert K to F first
            grid = self.KtoF(interpGrids[i])
            grid = clip(grid, minLimit, maxLimit)  # clip to min/max limits
            self.createGrid("Fcst", parmName, "SCALAR", grid, gridTimes[i])
