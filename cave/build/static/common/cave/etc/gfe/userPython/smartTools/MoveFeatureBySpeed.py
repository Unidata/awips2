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
# MoveFeatureBySpeed
#
# Author:                  Thomas R. Mazza
#                          adapted from EditAreaAdjust tool written by
#                          Les Colin
# Additional Contribution: Todd Lericos
# Last Updated:            Tue 10 Jun 8
# last Submitted to str:   Tue 10 Jun 8
# ----------------------------------------------------------------------------
import LogStream, time
ToolType = "numeric"
WeatherElementEdited = "variableElement"
from numpy import *
ScreenList = ["SCALAR","VECTOR"]
HideTool = 1
import math
import AbsTime
from math import *

#######   CONFIGURATION SECTION    #########################################################################
#
#  Add or delete models according to whether or not they are available at your office.
#

#sourceList = ["NAM12", "GFS40", "RUC80"]
sourceList = ["NAM12", "GFS40", "RUC40", "wrfnmm", "wrfarw", "WSETA"]

threeHour = ["NAM12"]
sixHour = ["GFS40"]
RUC = ["RUC13", "RUC40"]

resolution = 2.5

#
#######   END CONFIGURATION SECTION    #####################################################################

sourceList.append(("Fcst"))
sourceList.append(("Observed (enter below)"))


import SmartScript
## For available commands, see SmartScript

VariableList = []

VariableList.append(("Source:", "Observed (enter below)", "radio", sourceList))
VariableList.append(("Wind Level if using model:","MB700","radio",["MB925","MB850","MB700","MB500", "MB925-850", "MB850-700", "MB925-700", "MB925-500", "MB850-500", "MB700-500"]))
VariableList.append(("Movement Speed (Kts):", "15", "numeric"))
VariableList.append(("Movement Direction:" , "90", "numeric"))
VariableList.append(("Backfill upstream edges with:", "Original data", "radio", ["Original data", "Data from very edge\n(Fcst or Model only)", "Zeros"]))


# Set up Class

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)


    def execute(self, variableElement, variableElement_GridInfo, GridTimeRange, varDict):
        "Extrapolate features foward or backward in time based on observed or model speed"
        self.logToolUse("Smart Tool execution")

        modelList = ["NAM12", "GFS40", "RUC40", "RUC13", "wrfnmm", "wrfarw", "WSETA", "Fcst"]   #####################################################

        if varDict["Source:"] not in modelList and varDict["Source:"] != "Observed (enter below)":
            modelList.append((varDict["Source:"]))

        Backfill = varDict["Backfill upstream edges with:"]

        xLimit = len(variableElement[0])
        yLimit = len(variableElement)

        #########################################################################################
        #
        #  Get the source for the speed of motion

        source = varDict["Source:"]
        levels = []
        if source in modelList:
            site = self.getSiteID()

            if source == "Fcst":
                level = "SFC"
                msource = source
                wind = "Wind"
                Wind=self.getGrids(msource,wind,level,GridTimeRange)
                (speed, dir) = (Wind[0], Wind[1])
            else:
                #------------------------------------------------------------------------
                # Modification made by CAR to handle gaps in Model grids
                #--------------------------------------------------------------------------
                # This section expands the range of time to look for model data to use.
                # Model data is only available at certain times (e.g. NAM12: 00,03,06..etc)
                # Therefore, if the grid to be created is not at a time where model data
                # exists...then this code expands the time range to grab the nearest model
                # data. This range will be different for each model. Therefore...if models
                # are added to the top this section must be changed.
                #
                # Edited: Todd Lericos
                # Date: 3 Aug 2006
                #
                present = AbsTime.current()
                today = AbsTime.absTimeYMD(present.year, present.month, present.day)
                startTimeModel = (GridTimeRange.startTime() - today) /3600
#                print "over at Move", startTimeModel

                if source in threeHour:
                    self.modelRange = self.createTimeRange(startTimeModel-1, startTimeModel+2, "Zulu")
                elif source in sixHour:
                    self.modelRange = self.createTimeRange(startTimeModel-2, startTimeModel+4, "Zulu")
                else:
                    self.modelRange = GridTimeRange

                level = varDict["Wind Level if using model:"]
                msource=site+"_D2D_"+source
                wind = "wind"
                if level in ["MB925","MB850","MB700","MB500"]:
                    Wind=self.getGrids(msource,wind,level,self.modelRange)
                    (speed, dir) = (Wind[0], Wind[1])
                else:
                    if level == "MB925-850":
                        levels = self.buildLevels(950, 850, source)
                    elif level == "MB850-700":
                        levels = self.buildLevels(950, 700, source)
                    elif level == "MB925-700":
                        levels = self.buildLevels(925, 700, source)
                    elif level == "MB925-500":
                        levels = self.buildLevels(925, 500, source)
                    elif level == "MB850-500":
                        levels = self.buildLevels(850, 500, source)
                    elif level == "MB700-500":
                        levels = self.buildLevels(700, 500, source)
                    i = 0
                    j = 0
                    for k in xrange(len(levels)):
                        Wind=self.getGrids(msource,wind,levels[k],GridTimeRange)
                        (u,v)=self.MagDirToUV(Wind[0], Wind[1])
                        i += u
                        j += v
                    u = i / len(levels)
                    v = j / len(levels)

            #
            #  convert speed/dir arrays to speed/dir arrays 
            #
                    (speed, dir) = self.UVToMagDir(u,v)
            #
            #  convert from m/s to kts:
            #
            if wind == "wind":  ## have model data - need to convert from m/s to kts.
                speed *= 1.94384449244

        else:

            speed = varDict["Movement Speed (Kts):"]
            dir = varDict["Movement Direction:"]


        ########################################################################################
        #
        #  OK, we have the two components of motion, in kts.  Now convert to kph and compute
        #  movement over the grid.

        if source == "Observed (enter below)":
            
            speed *= 1.852 / resolution  ###  1 kt = 1.852 kph.
            dir = abs(abs(360 - dir) + 90)
            theta = dir % 360
            rads = pi * theta / 180

            x = int(round(speed * cos(rads)))
            y = int(round(speed * sin(rads)))

            if type(variableElement_GridInfo) is not str:

                newT = zeros(shape(variableElement),dtype=float64) - 80.0 # default value for T
                if Backfill == "Zeros":
                    changedMask = zeros(shape(variableElement),dtype=float64) # default value for T
                if x > 0 and y > 0:
                    newT[y:, x:] = variableElement[:-y, :-x]
                    if Backfill == "Zeros":
                        changedMask[y:, x:] = 1
                elif x > 0 and y < 0:
                    newT[:y, x:] = variableElement[-y:, :-x]
                    if Backfill == "Zeros":
                        changedMask[:y, x:] = 1
                elif x < 0 and y > 0:
                    newT[y:, :x] = variableElement[:-y, -x:]
                    if Backfill == "Zeros":
                        changedMask[y:, :x] = 1
                elif x < 0 and y < 0:
                    newT[:y, :x] = variableElement[-y:, -x:]
                    if Backfill == "Zeros":
                        changedMask[:y, :x] = 1
                elif x == 0 and y > 0:
                    newT[y:, x:] = variableElement[:-y, :]
                    if Backfill == "Zeros":
                        changedMask[y:, x:] = 1
                elif x == 0 and y < 0:
                    newT[:y, x:] = variableElement[-y:, :]
                    if Backfill == "Zeros":
                        changedMask[:y, x:] = 1
                elif x > 0 and y == 0:
                    newT[y:, x:] = variableElement[:, :-x]
                    if Backfill == "Zeros":
                        changedMask[y:, x:] = 1
                elif x < 0 and y == 0:
                    newT[y:, :x] = variableElement[:, -x:]
                    if Backfill == "Zeros":
                        changedMask[y:, :x] = 1
                   
            else:

                newT0 = zeros(shape(variableElement[0]),dtype=int32) - 80.0
                newT1 = zeros(shape(variableElement[1]),dtype=int32) - 80.0
                oldT0,oldT1 = variableElement
                if Backfill == "Zeros":
                    changedMask = zeros(shape(variableElement),dtype=float64) # default value for T

                if x > 0 and y > 0:
                    newT0[y:, x:] = oldT0[:-y, :-x]
                    newT1[y:, x:] = oldT1[:-y, :-x]
                elif x > 0 and y < 0:
                    newT0[:y, x:] = oldT0[-y:, :-x]
                    newT1[:y, x:] = oldT1[-y:, :-x]     
                elif x < 0 and y > 0:
                    newT0[y:, :x] = oldT0[:-y, -x:]
                    newT1[y:, :x] = oldT1[:-y, -x:] 
                elif x < 0 and y < 0:
                    newT0[:y, :x] = oldT0[-y:, -x:]
                    newT1[:y, :x] = oldT1[-y:, -x:]
                elif x == 0 and y > 0:
                    newT0[y:, x:] = oldT0[:-y, :]
                    newT1[y:, x:] = oldT1[:-y, :]
                elif x == 0 and y < 0:
                    newT0[:y, x:] = oldT0[-y:, :]
                    newT1[:y, x:] = oldT1[-y:, :]
                elif x > 0 and y == 0:
                    newT0[y:, x:] = oldT0[:, :-x]
                    newT1[y:, x:] = oldT1[:, :-x]
                elif x < 0 and y == 0:
                    newT0[y:, :x] = oldT0[:, -x:]
                    newT1[y:, :x] = oldT1[:, -x:]

                if Backfill == "Zeros":
                    changedMask[y:, x:] = 1
                   
                newT = newT0,newT1

        else: # source is a model
            speed1 = speed * 1.852 / resolution  ###  1 kt = 1.852 kph.
#            print "in move feature tool, missing hours ", missingHours
            theta = dir % 360

            (u,v)=self.MagDirToUV(speed1,theta)
            u /= resolution
            v /= resolution
            if  varDict["Movement Direction:"] < 0:
                u *= -1
                v *= -1
            newVariableElement = zeros(shape(variableElement),dtype=int16)
            if Backfill != "Original data":
                changedMask = zeros(shape(variableElement),dtype=float64) + 1

            for x in xrange(len(variableElement[0])):
                
                for y in xrange(len(variableElement)):
                    i = u[y,x]
                    j = v[y,x]
                    a = x - i
                    b = y - j
                    a = int(clip(a,0,xLimit - 1))
                    b = int(clip(b,0,yLimit - 1))
                    
                    newVariableElement[y,x] = variableElement[b,a]
                    
                    if Backfill == "Data from very edge\n(Fcst or Model only)":
                        
                        if u[y,x] > x :
                            newVariableElement[y,x] = newVariableElement[y,0]
                        if u[y,x] < 0 and u[y,x] > xLimit - x:
                            newVariableElement[y,x] = newVariableElement[y,xLimit]
                        if v[y,x] > y:
                            newVariableElement[y,x] = newVariableElement[0,x]
                        if v[y,x] < 0 and v[y,x] > yLimit - y:
                            newVariableElement[y,x] = newVariableElement[yLimit,x]

                    elif Backfill == "Zeros":
                        
                        if u[y,x] > x :
                            changedMask[y, x] = 0
                        if u[y,x] < 0 and abs(u[y,x]) > xLimit - x:
                            changedMask[y, x] = 0
                        if v[y,x] > y:
                            changedMask[y, x] = 0
                        if v[y,x] < 0 and abs(v[y,x]) > yLimit - y:
                            changedMask[y, x] = 0
                   
            newT = newVariableElement

        newT = where(less(newT, -30), variableElement, newT)
        if Backfill == "Zeros":
            newT[(changedMask <= 0)] = 0	
        # Return the new value
        
        return newT.astype(variableElement.dtype)

    def buildLevels(self, base, top, model):
        plevels = []
        if model in RUC:
            if base == 925:
                base = 950
            plevel = base
            while plevel >= top:
                plevels.append(("MB" + str(plevel)))
                plevel -= 50
        else:
            plevel = base
            while plevel >= top:
                plevels.append(("MB" + str(plevel)))
                plevel -= 25
        return plevels
        


    def logToolUse(self,string):
        gtime=time.gmtime()
        ts="%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d"%(gtime[0],gtime[1],gtime[2],
                                                  gtime[3],gtime[4],gtime[5])
        LogStream.logEvent("%s| %s" % (ts,string))
