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

from math import log, exp
from numpy import NaN, isnan

class Level:
    def __init__(self, pr, ht, tp, td, wd, ws):
        self.pr = pr
        self.ht = ht
        self.tp = tp
        self.td = td
        self.wd = wd
        self.ws = ws
    def getPr(self):
        return self.pr
    def getHt(self):
        return self.ht
    def getParam(self, index):
        if(index == 1):
            value = self.pr
        elif(index == 2):
            value = self.ht
        elif(index == 3):
            value = self.tp
        elif(index == 4):
            value = self.td
        elif(index == 5):
            value = self.wd
        elif(index == 6):
            value = self.ws
        return value

def execute(prMan, htMan, tpMan, tdMan, wdMan, wsMan, prSigT, tpSigT, tdSigT, htSigW, wdSigW, wsSigW, numMand, numSigT, numSigW, validTime, wmoStaNum):
    numMand[numMand <= -9999] = 0
    numSigT[numSigT <= -9999] = 0
    numSigW[numSigW <= -9999] = 0
    levelsMap = {}
    for c in range(0, len(numMand)):
        key = (validTime[c],wmoStaNum[c])
        levels = levelsMap.get(key,[])
        levelsMap[key] = levels
        for i in range(0, numMand[c]):
            levels.append(Level(prMan[c][i], htMan[c][i], tpMan[c][i], tdMan[c][i], wdMan[c][i], wsMan[c][i]))
        for i in range(0, numSigT[c]):
            levels.append(Level(prSigT[c][i], NaN, tpSigT[c][i], tdSigT[c][i], NaN, NaN))
    for levels in levelsMap.values():
        levels.sort(key=Level.getPr)
        for i in range(1,len(levels)):
            level = levels[i]
            # Interpolate missing Heights
            if isnan(level.ht):
                levelblo = levels[i-1]
                tv1 = level.tp
                tv2 = levelblo.tp
                mtv = ((tv1 + tv2) / 2);
                delta = log(levelblo.pr) - log(level.pr);
                delta *= 29.2898 * mtv;
                level.ht = levelblo.ht + delta
    for c in range(0, len(numMand)):
        key = (validTime[c],wmoStaNum[c])
        levels = levelsMap.get(key,[])
        levelsMap[key] = levels
        for i in range(0, numSigW[c]):
            levels.append(Level(NaN, htSigW[c][i], NaN, NaN, wdSigW[c][i], wsSigW[c][i]))
    for levels in levelsMap.values():
        levels.sort(key=Level.getPr)
        levelsCopy = levels[:]
        # Remove Pressure levels
        for i in range(1,len(levels)):
            if levelsCopy[i].pr == levelsCopy[i-1].pr:
                levels.remove(levelsCopy[i])
        levels.sort(key=Level.getHt)
        levelsCopy = levels[:]
        # Remove duplicate height levels
        for i in range(1,len(levels)):
            if levelsCopy[i].ht == levelsCopy[i-1].ht:
                levels.remove(levelsCopy[i])
        for i in range(1,len(levels)):
            level = levels[i]
            levelblo = levels[i-1]
            # Interpolate missing Temps
            if isnan(level.tp):
                levelabv = None
                for j in range(i,len(levels)):
                    if not(isnan(levels[j].tp)):
                        levelabv = levels[j]
                        break
                if levelabv != None:
                    level.tp = levelblo.tp + (level.ht - levelblo.ht)*(levelabv.tp - levelblo.tp)/(levelabv.ht - levelblo.ht)

            # Interpolate missing Dewpoints
            if isnan(level.td):
                levelabv = None
                for j in range(i,len(levels)):
                    if not(isnan(levels[j].td)):
                        levelabv = levels[j]
                        break
                if levelabv != None:
                    level.td = levelblo.td + (level.ht - levelblo.ht)*(levelabv.td - levelblo.td)/(levelabv.ht - levelblo.ht)
            # Interpolate missing Pressure
            if isnan(level.pr):
                zA = levelblo.ht;
                zB = level.ht;
                dz = zB - zA;
                tvA = levelblo.tp + 273.13;
                tvB = level.tp + 273.13;
                k = (tvA + tvB) / 2 * 28.2898;
                pA = levelblo.pr;
                k = (-dz / k) + log(pA);
                level.pr = exp(k);
    levelsList = []
    for c in range(0, len(numMand)):
        key = (validTime[c],wmoStaNum[c])
        levels = levelsMap.get(key, None)
        if (levels != None):
            levelsList.append(levels)
    return levelsList