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
# Numeric_MixHgt_Init
#
# Author:
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "MixHgt"
from numpy import *

VariableList = [("Initialize From Model: " , "", "D2D_model")]

import SmartScript
import string

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessTool(self, varDict):
        self.__D2Dmodel = varDict["Initialize From Model: "]
        print self.__D2Dmodel

    ## This tool calculates mixing height by first calculating the surface
    ## potential temp and then looking at increasing levels of the temp.
    ## sounding until the potential temp at that level is greater than
    ## the surface potential temperature.  Then the mixing height is
    ## calculated by interpolating between the level at which the pot. temp.
    ## was exceeded and the level below it.  Mixing height is returned in
    ## units of feet above ground.
        
    def execute(self, GridTimeRange, T, Topo):
        "Assigns MixHgt Index"

        levels = ["MB600","MB650","MB700","MB750","MB800","MB850",
                  "MB900","MB950"]

        # Get the ghCube and tCube
        sounding = self.makeNumericSounding(self.__D2Dmodel, 't', levels,
                                     GridTimeRange, noDataError=0)
        if sounding is None:
            self.noData()
        ghCube, tCube = sounding
        print ghCube[0][90,80], tCube[0][90,80]

        zeroGrid = zeros(T.shape, dtype=float)
        thetaCube = []
        pLevelCube = []
        # Make a potential temperature and pressure cubes
        for i in xrange(len(levels)):
            pLevel = zeroGrid + string.atof(levels[i][2:])
            potTemp = tCube[i] * pow((1000 / pLevel), 0.286)  # in millibars
            thetaCube = thetaCube + [potTemp]
            pLevelCube = pLevelCube + [pLevel]
        pLevelCube = array(pLevelCube)
        thetaCube = array(thetaCube)

        ## Calculate the surface potential temperature
        T_K = self.convertFtoK(T)
        # Get the surface pressure from NAM in pascals
        sfcPres = self.getGrids(self.__D2Dmodel, "p", "SFC", GridTimeRange)
        sfcPres = sfcPres / 100 # convert from Pascals to millibars
        sfcTheta = T_K * pow((1000 / sfcPres), 0.286)
        
        ##  find the height where the potential temp > surfacePotTemp
        # Initialize to -1
        mixingHt = zeros(T.shape, dtype=float) - 1
        lastTuple = (ghCube[0], thetaCube[0])
        for i in xrange(len(levels)):
             pMask = less(pLevelCube[i], sfcPres)
             tMask = less(thetaCube[i], sfcTheta)
             # Assign only if mixingHt is -1 (i.e. we haven't assigned
             #   it yet) AND pMask and tMask are true
             readyToSet = logical_and(equal(mixingHt,-1),
                                      logical_and(pMask, tMask))
             # Calculate the whole grid at this level
             #print "\n", i, readyToSet[90,80]
             newMh = self.getMixingHeight((ghCube[i], thetaCube[i]),
                                          lastTuple, sfcTheta)
             mixingHt = where(readyToSet, newMh, mixingHt)
             lastTuple = (ghCube[i], thetaCube[i])
      
        mixingHt = where(equal(mixingHt,-1), 0.0, mixingHt)
        mixingHt = mixingHt * 3.2808
        mixingHt = where(equal(mixingHt, 0.0), 0.0, mixingHt-Topo)
        
        #print "MixingHT:", mixingHt[90,80], "pres:", sfcPres[90,80],
        #print sfcTheta[90,80], Topo[90,80]
        
        return mixingHt

    def getMixingHeight(self, (h1, t1), (h2, t2), sfcT):
        # Since the sounding increases with height h1 < h2
        try:    
            result = h1 + (((h2 - h1) / (t2 - t1)) * (sfcT - t1))
            #print h1[90,80], h2[90,80], t1[90,80], t2[90,80], sfcT[90,80]
            #print "getMixingHeight", result[90,80]
            return result
        except:
            return h1



## POINT-BASED VERSION
##VariableList = [("Initialize From Model: " , "", "D2D_model")]

##import SmartScript
##import string

##class Tool (SmartScript.SmartScript):
##    def __init__(self, dbss):
##        SmartScript.SmartScript.__init__(self, dbss)

##    def preProcessTool(self, varDict):
##        self.__D2Dmodel = varDict["Initialize From Model: "]

##    ## This tool calculates mixing height by first calculating the surface
##    ## potential temp and then looking at increasing levels of the temp.
##    ## sounding until the potential temp at that level is greater than
##    ## the surface potential temperature.  Then the mixing height is
##    ## calculated by interpolating between the level at which the pot. temp.
##    ## was exceeded and the level below it.  Mixing height is returned in
##    ## units of feet above ground.
##    def execute(self, x, y, GridTimeRange, T, Topo):
##        "Assigns MixHgt Index"

##        levels = ["MB600","MB650","MB700","MB750","MB800","MB850",
##                  "MB900","MB950"]

##        sounding = self.makeSounding(self.__D2Dmodel, 't', levels,
##                                     x, y, GridTimeRange, noDataError=0)

##        if sounding is None:
##            self.noData()

##        thetaSounding = []
##        index = 0
##        for h, t in sounding:
##            pLevel = string.atof(levels[index][2:])
##            potTemp = t * pow((1000 / pLevel), 0.286)  # in millibars
##            thetaSounding.append((pLevel, h, potTemp))
##            index = index + 1

##        ## Calculate the surface potential temperature
##        T_K = self.convertFtoK(T)

##        # Get the surface pressure from NAM in pascals
##        sfcPres = self.getValue(self.__D2Dmodel, "p", "SFC",
##                                x, y, GridTimeRange)
##        sfcPres = sfcPres / 100 # convert from Pascals to millibars

##        sfcTheta = T_K * pow((1000 / sfcPres), 0.286)
        
##        ##  find the height where the potential temp > surfacePotTemp
##        mixingHt = 0.0   # initialize
##        lastTuple = (thetaSounding[0][1], thetaSounding[0][2])
##        for p, h, t in thetaSounding:
##            print "pLevel:", p, "ht:", h, "t:", t, "sfcTheta:", sfcTheta
##            if p < sfcPres:
##                if t < sfcTheta:
##                    mixingHt = self.getMixingHeight((h, t), lastTuple, sfcTheta)
##                    break
##            lastTuple = (h, t)

###        print "MixingHT:", mixingHt, "pres:", sfcPres
        
##        if mixingHt == 0.0:
##            return mixingHt
        
##        mixingHt = mixingHt * 3.2808  #  convert meters to feet

##        mixingHt = mixingHt - Topo   #  subtract Topo to get height above ground
        
##        return mixingHt

##    def getMixingHeight(self, (h1, t1), (h2, t2), sfcT):
##        # Since the sounding increases with height h1 < h2
###        print "h1:", h1, "t1:", t1, "h2:", h2, "t2:", t2, "sfcT:", sfcT
##        try:    
##            return h1 + ((h2 - h1) / (t2 - t1) * (sfcT - t1))
##        except:
##            return h1


