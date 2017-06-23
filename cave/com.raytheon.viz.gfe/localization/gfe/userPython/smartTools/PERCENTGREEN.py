# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# PERCENTGREEN.py
#
# Author: dtomalak
# ----------------------------------------------------------------------------


ToolType = "numeric"
WeatherElementEdited = "PERCENTGREEN"
from numpy import *
HideTool = 0

# You can screen the elements for which your tool will appear by using
# a ScreenList.  For example:
#
#ScreenList = ["T","Td"]
#ScreenList = ["SCALAR","VECTOR","WEATHER","DISCRETE"]

# If desired, Set up variables to be solicited from the user:
# VariableList = [
#         ("Variable name1" , defaultValue1, "numeric"),
#         ("Variable name2" , "default value2", "alphaNumeric"),
#         ("Variable name3" , ["default value1", "default value2"], "check",
#                       ["value1", "value2", "value3"]),
#         ("Variable name4" , "default value4", "radio",
#                       ["value1", "value2", "value3"]),
#         ("Variable name5" , defaultValue, "scale",
#                       [minValue, maxValue], resolution),
#         ("Variable name6" , "", "model"),
#         ("Variable name7" , "", "D2D_model"),
#         ("Label contents" , "", "label"),
#         ("", dialogHeight, "scrollbar"),
#        ]

# Set up Class
import SmartScript
import types, string, imp, cPickle, time, sys
from math import *
import re
import Exceptions
import UnitConvertor
# For available commands, see SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  %comment
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...
    
    def execute(self, PERCENTGREEN, varDict):
        "THIS TOOL WILL POPULATE A % GREEN GRID FOR THE RANGELAND FIRE DANGER INDEX"
        ####CONFIGURABLE SECTION
        ###PERCENT GREEN DATA IS NEEDS TO BE IN FIPS CODE VALUE FORMAT!!!
        #STATE DICTIONARY
        #DICTIONARY OF EACH DESIRED STATE AND THE FILENAME OF % GREEN FILE
        self._statesdict = {"NE"     :  "ne.green.txt",
                            "IA"     :  "ia.green.txt",
                            }

        #DATA DIRECTORY - name of directory where data is stored
        #ex "/home/local/testdat (leave off last /)
        datadir = "/data/local/PercentGreen/"
        
                            #SET VARIABLES TO "NONE"
        ####END CONFIGURATIONS!!!!!!!!!!!!!!
        ############################################################
        ############################################################
        ############## MAKE NO CHANGES   ###########################
        ############################################################
        # 
        #COLLECT FIPS AREAS IN DATABASE 
        alleditareas = self.editAreaList()
        FIPSonly = []
        statekeys = self._statesdict.keys()
        for area in alleditareas:
            #TEST FOR FIPS CODES
            if len(area) != 6:
                continue
            else:
                test = area[0:2]
                test2 = area[2:]
                if test in statekeys:
                    #do something
                    if string.find(test2, "C") != -1:
                        #AREA HAS PASSED ALL TESTS>>>IS LIKELY A FIPS CODE
                        FIPSonly.append(area)
                        continue
                    else:
                        continue
                else:
                    continue
        #FOREACH STATE GRAB THE DATA AND PUT IT IN STRING FORMAT
        #WILL RETURN ONE LIST FOR ALL STATES
        datadict = {}
        for state in statekeys:
            stfile = self._statesdict[state]
            try:
                getdat = open(datadir + "/" + stfile, "r")
                data = getdat.readlines()
                getdat.close()
                for line in data:
                    line = string.strip(line) #CLEAN OUT EXTRA SPACES if there is any
                    val = string.split(line, " ")
                    if len(val) > 2:
                        #PREVENT NON DATA POINTS FROM GETTTING INTO DATA DICT
                        continue
                    if val[0] in FIPSonly:
                        datadict[str(val[0])] = str(val[1])
                    else:
                        continue
            except:
                continue
        #DATA NOW IN DICTIONARY FORM...STEP THROUGH EACH KEY AND ASSIGN A DATA VALUE
        #USING WHERE STATEMENTS
        newgreen = zeros(PERCENTGREEN.shape, int32)

        for zone in datadict.keys():
            area = zone
            value = int(datadict[zone])
            areamask = self.encodeEditArea(area)
            newgreen[not_equal(areamask,0)] = value
        
        PERCENTGREEN = newgreen
        return PERCENTGREEN
    