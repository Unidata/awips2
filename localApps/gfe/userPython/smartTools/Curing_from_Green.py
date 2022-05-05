# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Curing_from_Green.py
#
# Author: dtomalak
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "Curing"
HideTool = 0


import SmartScript
from math import *
from numpy import *


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, Curing):
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
                    if "C" in test2:
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
                    line = line.strip() #CLEAN OUT EXTRA SPACES if there is any
                    val = line.split(" ")
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
        newgreen = zeros(Curing.shape, int32)
        
        #WILL DEFAULT TO ZERO IF NO NEW DATA IS FOUND
        for zone in datadict:
            area = zone
            value = int(datadict[zone])
            areamask = self.encodeEditArea(area)
            newgreen[not_equal(areamask,0)] = value
        
        Curing = 100 - newgreen
        return Curing
