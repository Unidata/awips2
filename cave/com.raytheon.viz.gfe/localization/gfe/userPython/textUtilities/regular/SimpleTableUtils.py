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
# SimpleTableUtils.py
# Methods for producing simple engine-driven tables.
#
# Author: hansen
# ----------------------------------------------------------------------------

import TextUtils
import string, types

class SimpleTableUtils(TextUtils.TextUtils):
    def __init__(self):    
         TextUtils.TextUtils.__init__(self)
        
    def table_light_winds_phrase(self):
        # Phrase to use for winds under the wind_threshold when reported in a Table
        return "CALM"
    
    def table_wind_threshold(self):
        # Phrase to use for winds under the wind_threshold when reported in a Table
        return 5

    #********************
    #  Table Product ReportAs Methods for formatting weather element values for tables

    def singleValue(self, element, stats):
        # Takes in one value, formats, and returns it
        # If vector type, stats is mag,dir where direction can
        #  be mag or dir

        valStr = ""
        if stats is None:
            return valStr

        if element.dataType() == "Vector":
            mag = self.callMethod(stats[0], element.conversion())
            mag = self.round(mag,"Nearest",element.roundVal())
            if mag <= self.table_wind_threshold():
                valStr = self.table_light_winds_phrase()
            else:
                magStr = self.fformat(mag, element.roundVal())
                magStr = string.rjust(magStr, element.maxWidth())
                dir = stats[1]
                if type(dir) is not types.StringType:
                   dir = self.round(dir,"Nearest",element.roundVal())
                   dirStr = self.convertDirection(dir)
                else:
                   dirStr = dir
                valStr = string.rjust(dirStr,2) + magStr
        else:
            val = self.callMethod(stats, element.conversion())
            value = self.round(val, "Nearest", element.roundVal())
            valueStr = self.fformat(value, element.roundVal())
            valStr = string.rjust(valueStr, element.maxWidth())

        return valStr

    def avgValue(self, element, stats):
        # Takes in values, averages, formats, and returns them
        # Scalar input: (val1, val2)
        # Vector input: vectorRange:(mag1, mag2, dir1, dir2)

        valStr = ""
        if stats is None:
            return valStr

        if element.dataType() == "Vector":
            mag1 = stats[0]
            mag2 = stats[1]
            #print "stats", stats
            dir1 = stats[2]
            dir2 = stats[3]
            mag, dir = self.vectorAverage((mag1, dir1),(mag2, dir2))
            mag = self.callMethod(mag, element.conversion())
            mag = self.round(mag, "Nearest", element.roundVal())
            if mag <= self.table_wind_threshold():
                valStr = self.table_light_winds_phrase()
            else:
                valStr = self.fformat(mag, element.roundVal())
                valStr = string.rjust(valStr, element.maxWidth())
                # put in the direction
                dir = self.convertDirection(dir)
                valStr = string.rjust(dir, 2) + valStr
        else:
            val1 = self.average(stats[0], stats[1])
            val1 = self.callMethod(val1, element.conversion())
            val = self.round(val1, "Nearest", element.roundVal())
            valStr = self.fformat(val, element.roundVal())
            valStr = string.rjust(valStr, element.maxWidth())

        return valStr

    def range2Value(self, element, stats):
        # Takes in two values and reports range
        # Vector input: vectorRange: (mag1, mag2, dir1, dir2)

        valStr = ""
        if stats is None:
            return valStr

        val1 = self.callMethod(stats[0], element.conversion())
        val2 = self.callMethod(stats[1], element.conversion())
        val1 = self.round(val1,"Nearest", element.roundVal())
        val2 = self.round(val2,"Nearest", element.roundVal())
        val1Str = self.fformat(val1, element.roundVal())
        val2Str = self.fformat(val2, element.roundVal())
        if element.dataType() == "Scalar":
            if val1 == val2:
                valStr = string.rjust(val1Str, element.maxWidth())
            else:
                valStr = string.rjust(val1Str, element.maxWidth()) + "-" + \
                             string.rjust(val2Str, element.maxWidth())
        else:
            dir1 = self.dirToText(stats[2])
            dir2 = self.dirToText(stats[3])
            dir1 = string.rjust(dir1,2)
            dir2 = string.rjust(dir2,2)
            if val1 == val2 and dir1 == dir2:
                valStr = string.rjust(val1Str, element.maxWidth()) + dir1
            else:
                valStr = dir1 + " " + string.rjust(val1Str, element.maxWidth()) + \
                         " - " + \
                         dir2 + " " + string.rjust(val2Str, element.maxWidth())

        valStr = string.replace(valStr,"- ","-")
        return valStr

    def range4Value(self, element, stats):
        # Reports range of two averages
        # Scalar input: (min1, max1, min2, max2)
        # Vector input: vectorText: (mag1, mag2, mag3, mag4, dir1, dir2)

        valStr = ""
        if stats is None:
            return valStr

        if element.dataType() == "Vector":
            mag1 = self.average(stats[0], stats[1])
            mag2 = self.average(stats[2], stats[3])
            mag1 = self.callMethod(mag1, element.conversion())
            mag1 = self.round(mag1, "Nearest", element.roundVal())
            mag2 = self.callMethod(mag2, element.conversion())
            mag2 = self.round(mag2, "Nearest", element.roundVal())
            dir1 = stats[4]
            dir2 = stats[5]
            val1Str = self.fformat(mag1, element.roundVal())
            val1Str = string.rjust(val1Str, element.maxWidth())
            val1Str = string.rjust(dir1,2) + val1Str
            val2Str = self.fformat(mag2, element.roundVal())
            val2Str = string.rjust(val2Str, element.maxWidth())
            val2Str = string.rjust(dir2,2) + val2Str
        else:
            val1 = self.average(stats[0], stats[1])
            val2 = self.average(stats[2], stats[3])
            val1 = self.callMethod(val1, element.conversion())
            val2 = self.callMethod(val2, element.conversion())
            val1 = string.rjust(self.round(val1,"Nearest", element.roundVal()),
                                element.maxWidth())
            val2 = string.rjust(self.round(val2,"Nearest", element.roundVal()),
                                element.maxWidth())
            val1Str = self.fformat(val1, element.roundVal())
            val2Str = self.fformat(val2, element.roundVal())
        if val1Str == val2Str:
            valStr = val1Str
        else :
            valStr = val1Str + " - " + val2Str

        return valStr
 
