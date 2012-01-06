# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TransWind2_Tool  numeric version of a Transport Wind smarttool.
#
# Author: C. Gibson SLC 10/01 Modified from a wind smarttool written by Tom L.
#
# Designed for the NAM but can be adapted for other models. Two major modes of
#  operation 1. boundary layer (BL) winds included, or 2. only pressure "layers"
#  used. Follow comments to switch between. Also, you can comment out certain BL
#  levels as desired.
#
# ----------------------------------------------------------------------------
#
# This tool modified February 2004/Matt Davis/ARX
# 10/2004 - Updated to allow the NAM12, NAM40, and GFS choices for this procedure.
# NCNWS Fire Bundle Package Release 2.1
#
ToolType = "numeric" 
WeatherElementEdited = "TransWind"
from numpy import *
import SmartScript
import Dialog, time

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, Topo, MixHgt, Wind):

        fwModel = self.getObject("FireModel", "ModelType")

        if fwModel == "NAM12":
            modelSource = "_D2D_NAM12"
        elif fwModel == "NAM40":
            modelSource = "_D2D_NAM40"
        else:
            modelSource = "_D2D_GFS40"

        "Calculates average wind in the mixed layer."
        self.setVectorEditMode('Both')
        site =  self.getSiteID()
##        model = site + modelSource + monthrun
        model = site + modelSource
        print "Using " + model + " for Transport Wind Calculation"
        print "Transport Wind time range is: \n" + `GridTimeRange`
          
        
        self.__D2Dmodel = model

        
        layers = ["MB975", "MB950", "MB925", "MB900", "MB875","MB850","MB825","MB800","MB775","MB750","MB725","MB700","MB650","MB600"]

        # Get the ghCube
        gh = self.makeNumericSounding(self.__D2Dmodel, 'wind', layers,
                                      GridTimeRange, noDataError=1)
        if gh is None:
            self.noData()

        ghCube, windCube = gh
        magCube, dirCube = windCube

        levels = 0
        uTotal = 0
        vTotal = 0

        u,v = getUV(self,  Wind[0], Wind[1])
        levels = levels + 1
        uTotal = uTotal + u
        vTotal = vTotal + v
        

#  Average winds at pressure levels between Topo and MixHgt


        Topo_M = Topo / 3.2808
        MixHgt_M = (Topo + MixHgt) / 3.2808

        for i in xrange(len(layers)):
            MHset = less(ghCube[i],MixHgt_M)
            toposet = greater(ghCube[i],Topo_M)
            readyToSet = logical_and(less(ghCube[i],MixHgt_M), greater(ghCube[i],Topo_M))
            u,v = getUV(self,  magCube[i], dirCube[i])

            levels = where(readyToSet, levels + 1, levels)
            uTotal = where(readyToSet, uTotal + u, uTotal)
            vTotal = where(readyToSet, vTotal + v, vTotal)


        leveltest = equal(levels, 0)

        levels[leveltest] = 1

        vTotal = vTotal/levels
        uTotal = uTotal/levels
        mag_Msec, dir = getMD(self, uTotal, vTotal)
        mag_Kts = self.convertMsecToKts(mag_Msec)
        
        TransWind = (mag_Kts, dir)
        # Return the new value
        
        return TransWind

# converts mag, dir to u,v
#  addapted from BASE init.py
def getUV(self, mag, dir):
    rad = dir * 0.0174
    u = mag * sin(rad)
    v = mag * cos(rad)
    return (u, v)

# converts u,v to mag and direction.
#  adapted from BASE init.py
def getMD(self, u, v):
    mag = sqrt(u * u + v * v)
    dir = arctan2(u, v) / 0.0174

    dir[greater_equal(dir, 360)] -= 360
    dir[less(dir, 0)] +=360
    
    return (mag, dir)
