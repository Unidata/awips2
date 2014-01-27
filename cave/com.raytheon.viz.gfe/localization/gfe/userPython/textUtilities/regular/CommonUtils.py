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
# CommonUtils.py
# Utility methods for Text Products.
#
# Author: hansen
# ----------------------------------------------------------------------------

from math import *
import types, string
import os, re, time
import TimeRange, WeatherSubKey


class CommonUtils:
    def __init__(self):
        pass

    def coveragePoP_value(self, coverage):
        # Pop ranges that correspond to each of the weather coverages
        popValue = self.coveragePoP_table()
        return popValue[coverage]
    
    def coveragePoP_table(self):
        # Pop ranges that correspond to each of the weather coverages
        return {
            "<NoCov>": (0,0),
            "Iso":  (0, 20),
            "SChc": (0, 20),
            "Patchy": (0, 20),
            "Areas":(30, 50),
            "Chc":  (30, 50),
            "Sct":  (30, 50),
            "Lkly": (60, 70),
            "Num":  (60, 70),
            "Brf":  (80, 100),
            "Frq":  (80, 100),
            "Ocnl": (80, 100),
            "Pds":  (80, 100),
            "Inter":(80, 100),
            "Def":  (80, 100),
            "Wide": (80, 100),
            }

    def rankFuzzFactor(self):
        # Used when combining weather subkeys.
        # If the difference between the subkey rankings is
        # less than this amount, the subkeys will be combined and
        # the dominant coverage or intensity will be chosen
        #
        return 10

    def arealCoverages(self):
        # These are areal and duration coverages
        return ["Wide","Num","Sct","Areas","WSct","Iso","Patchy",
                "Brf", "Frq", "Ocnl", "Pds", "Inter"]

    def makeAggregateSubkey(self, subkey1, rank1, subkey2, rank2):
        # Assumes subkey1 and subkey2 have the same wxType
        # Combines the two subkeys into a new one according to the following rules:
        #  If the ranks are significantly different using the
        #  rankFuzzFactor, then the higher ranked coverage and intensity
        #  will be used for the new subkey.
        #  Otherwise, the dominant coverage and intensity will be used.
        #  In any case, the lowest visibility will be used and all
        #  attributes will be taken.
        # This method is used throughout the code:
        #  --"rankedWx": SampleAnalysis
        #  --"filterSubkeys": TextRules
        #  --"combineWxStats" called by "preProcessWx": TextRules
        # If similarAttrLists is not None, collapse similarAttributes e.g.
        #   DmgW, GW will be collapsed to DmgW
        wxType = subkey1.wxType() # Assumed to be similar, so we'll take the first one
        if wxType == "<NoWx>":
            WeatherSubKey.weatherSubKey(self._argDict['dataMgr'], "<NoCov>", "<NoWx>", "<NoInten>", "<NoVis>", [])
            
        rankFuzzFactor = self.rankFuzzFactor()
        #print "rank1, rank2", rank1, rank2
        #print "rankFuzz", rankFuzzFactor
        if rank1 > rank2 + rankFuzzFactor:
            coverage = subkey1.coverage()
            intensity = subkey1.intensity()
        elif rank2 > rank1 + rankFuzzFactor:
            coverage = subkey2.coverage()
            intensity = subkey2.intensity()
        else:            
            coverage = self.getDominant(
                "coverage", subkey1.coverage(), subkey2.coverage())
            intensity = self.getDominant(
                "intensity", subkey1.intensity(), subkey2.intensity())
        #print "coverage", coverage
        vis = self.getVis([subkey1, subkey2], outputFormat="Text")
        attrList = subkey1.attributes() + subkey2.attributes()
        attrList = self.removeSimilarAttrs(attrList)
        attrList = self.removeDups(attrList)
        if vis is None:
            vis = "<NoVis>"
        #print coverage, wxType, intensity, vis, attrList
        return WeatherSubKey.weatherSubKey(self._argDict['dataMgr'], coverage, wxType, intensity, vis, attrList)
    
    def removeSimilarAttrs(self, attrList):
        similarAttrLists = self.similarAttributeLists()
        newAttrs = []
        for similarAttrs in similarAttrLists:
            for attr in similarAttrs:
                if attr in attrList:
                    # Take only the first one found in each list
                    # and ignore the rest
                    newAttrs.append(attr)
                    for a in similarAttrs:
                        if a in attrList: attrList.remove(a)
                    break
        newAttrs += attrList
        return newAttrs

    def rankedSortOrder(self, val1, val2):
        # Sort by rank, weather type, coverage
        # Will handle rankList OR simply subkeys
        try:
            subkey1, rank1 = val1
            subkey2, rank2 = val2
        except:
            subkey1 = val1
            rank1 = 0
            subkey2 = val2
            rank2 = 0
        fuzzFactor = self.rankFuzzFactor()
        #print "subkey1, subkey2", subkey1, subkey2
        if rank1 - rank2 > fuzzFactor:
            return -1
        elif rank2 - rank1 > fuzzFactor:
            return 1
        else: # Base order on dominant coverage
            cov1 = subkey1.coverage()
            cov2 = subkey2.coverage()
            if cov1 == cov2:
                # Base order on dominant type
                wxType1 = subkey1.wxType()
                wxType2 = subkey2.wxType()
                index1 = self.getIndex("wxType", wxType1)
                index2 = self.getIndex("wxType", wxType2)
                if index1 < index2:
                    return -1
                else:
                    return 1
            else:
                index1 = self.getIndex("coverage", cov1)
                index2 = self.getIndex("coverage", cov2)
                if index1 < index2:
                    return -1
                else:
                    return 1

    def rankedWxTypeOrder(self, val1, val2):
        # Sort by weather type
        # Will handle rankList OR simply subkeys
        try:
            subkey1, rank1 = val1
            subkey2, rank2 = val2
        except:
            subkey1 = val1
            rank1 = 0
            subkey2 = val2
            rank2 = 0
        # Base order on dominant type
        wxType1 = subkey1.wxType()
        wxType2 = subkey2.wxType()
        index1 = self.getIndex("wxType", wxType1)
        index2 = self.getIndex("wxType", wxType2)
        if index1 < index2:
            return -1
        elif index1 == index2:
            return 0
        else:
            return 1

    def divideRange(self, timeRange, hours=0):
        "Divide the timeRange into sub-ranges of the given number of hours"

        # We will start from the END of the time range and work backwards
        # so that "odd" hours will end up at the beginning of the time range.
        # For example:
        #    Suppose our time range is 8 hours long from
        #    10 am to 6 pm). This method will return  a 2-hour sub-range from 10 am
        #    to noon and a 6-hour sub-range from noon to 6 pm.

        subRanges = []
        endTime = timeRange.endTime()
        done = 0
        while done == 0:
            newStart = endTime - hours * 3600
            if newStart <= timeRange.startTime():
                newStart = timeRange.startTime()
                done = 1
            newRange = TimeRange.TimeRange(newStart, endTime)
            subRanges.append(newRange)
            endTime = newStart
        subRanges.reverse()
        return subRanges

    def removeDups(self, list):
        # Remove duplicates preserving order
        newList = []
        for entry in list:
            if entry not in newList:
                newList.append(entry)
        return newList
##      This version sorts first    
##        list.sort()
##        last = None
##        newList = []
##        for entry in list:
##            if entry == last:
##                continue
##            last = entry
##            newList.append(entry)
##        return newList            

