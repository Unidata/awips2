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
##############################################################################
#-----------------------------------------------------------------------------
# Name: IFPS2AvnFPS.py
# Description: Provide aviation element forecasts from TAF edit areas
# Author: Fred McMullen with help from Dave Holtz/Tracy Hansen/Paul Jendrowski
#-----------------------------------------------------------------------------
##############################################################################

import TextRules, ConfigParser
import SampleAnalysis
import string, time, re, os, sys, types, copy, AFPS
import ProcessVariableList

class TextProduct(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    """This text formatter replaces avn_unloader which is retired in OB7.1 and
       builds thereafter."""
    #
    RootDir = '/awips/adapt/avnfps'
    Definition =  {
	    "type": "smart",
	    "displayName": "avnFPS",
	    "autoSend": 0,              # set to 1 to automatically transmit product
	    "autoSendAddress": "000",   # transmission address
	    "autoStore": 0,             # set to 1 to automatically store product in textDB
	    "autoWrite": 0,             # set to 1 to automatically write product to file
	    "debug": 0
	    }
    #
    # Useful dictionary
    _translateCode = { 'Iso' : 'IS', 'Sct' : 'SC', 'Num' : 'NM', 'Wide': 'WP', 'Ocnl': 'O', 'SChc': 'S',
                       'Chc' : 'C', 'Lkly': 'L',  'Def' : 'D',  'Patchy': 'IS', 'Areas':'SC', 'Frq': 'L',
                       'Brf': 'S', 'Pds': 'C', 'Inter': 'L', '<NoCov>':' ', '<NoInten>':' ', '--':'-',
                       '-':'-', 'm':'m', '+':'+' }
    
    _vsbyCodes = ['F','ZF','IF','IC','H','BS','BN','K','BD','Y','ZY','VA']
    #
    # Look at the arguments to get the required TAF ids
    try:
	dummy1Dict = {}
	dummy2Dict = {}
        tmp = []
        add = False
        #
        # Generate tmp from -r flags
        for tafID in sys.argv[1:]:
            if add:
                tmp.append(tafID)
                add = False
            elif tafID == '-r':
                add = True

        for tafID in tmp:
	    dummy1Dict[tafID] = '%s/data/grids/%s' % ( RootDir, tafID )
	    dummy2Dict[tafID] = 'NOTUSED'

	Definition['outputFileDict'] = dummy1Dict.copy()
	Definition['pilDict'] = dummy2Dict.copy()
            
    except:
        print 'Unable to read command for TAFs'
                    
    def __init__(self):            
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)
        
    def generateForecast(self, argDict):
        # Get variables
        self._getVariables(argDict)
        #        
        self._areaList = self.getAreaList(argDict)
        self._determineTimeRanges(argDict)

        # Sample the data
        self._sampleData(argDict)

        # Initialize the output string
        fcstDict = self._initializeFcst(argDict)
        
        # Generate the product for each edit area in the list
        for editArea, areaLabel in self._areaList:
            if fcstDict.has_key(areaLabel):
                fcst = fcstDict[areaLabel]
                fcst = self._makeProduct(fcst, editArea, areaLabel, argDict)
                fcstDict[areaLabel]=fcst

        finalFcst = self._getFinalFcst(fcstDict, argDict)
        return finalFcst

    def _getVariables(self, argDict):
        # Make argDict accessible
        self.__argDict = argDict
        
        # Get Definition variables
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        # Get VariableList and _issuance_list variables
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"
                
        self._rowLabelWidth = 8
        self._Width = 5
        # Make a list of outputs based on names in editAreas
        self._outputList=[]
        for editArea, areaLabel in self.__argDict['editAreas']:
            self._outputList.append(areaLabel)
            
        return

################################################################################
    
    def _determineTimeRanges(self, argDict):        
        # Calculate ddhhmm string value
        self._currentTime = int(round(time.time()))
        self._currentTimeStr = str(self._currentTime)
        self._issueTime = AFPS.AbsTime.current()
        self._currentTime = self._currentTime + 3600
        self._pastTime = self._currentTime - 3600
        self._ddhhmmTime = time.strftime("%d%H%M",time.gmtime(self._pastTime))
        
        rangeTime1 = time.strftime("%H",time.localtime(self._currentTime))
        rangeTime = int(rangeTime1)
        TimeRange1hr = self.createTimeRange(rangeTime-1, (rangeTime+72))

        timePeriod = 1
        timeSpan = 1
        numPeriods = 72
        self._Periods = self.getPeriods(TimeRange1hr, timePeriod,
                                       timeSpan, numPeriods)
        return 

    def _sampleData(self, argDict):
        # Sample the data.  Sets up self._sampler
        sampleList = []
        sampleList.append((self._analysisList(),self._Periods))
        sampleInfo = []
        for analList, periods in sampleList:
            sampleInfo.append((analList, periods, self._areaList))
            
        self._sampler = self.getSampler(argDict, sampleInfo)

        return
    
    def _preProcessProduct(self, fcst, argDict):
        fcst = 'Time ' + str(self._pastTime-(self._pastTime%3600)) + '\n'
        return fcst

    def _makeProduct(self, fcst, editArea, areaLabel, argDict):
        #
        # Ordering of elements isn't terribly important however comparing
        # results to the older formatter its easier if ordering is the same.
        #
        statList = self.getStatList(self._sampler, self._analysisList(),
                                    self._Periods, editArea)
        #Sky
        fcst=fcst+ self.makeRow( "Sky ", self._Width, self._Periods,
                                 statList, self._skyValue, ['Sky'],
                                 self._rowLabelWidth)

        #Temp    
        fcst=fcst+ self.makeRow( "Temp", self._Width, self._Periods,
                                 statList, self._scalarValue, ['T'],
                                 self._rowLabelWidth)

        # DewPt
        fcst=fcst+ self.makeRow( "DwptT", self._Width, self._Periods,
                                 statList, self._scalarValue, ['Td'],
                                 self._rowLabelWidth)

        # Wind direction
        fcst=fcst+ self.makeRow( "WDir ", self._Width, self._Periods,
                                 statList, self._windValue, ['dir'],
                                 self._rowLabelWidth)

        # Windspd
        fcst=fcst+ self.makeRow( "WSpd ", self._Width, self._Periods,
                                 statList, self._windValue, ['speed'],
                                 self._rowLabelWidth)

        # Windgust
        fcst=fcst+ self.makeRow( "WGust ", self._Width, self._Periods,
                                 statList, self._scalarValue, ['WindGust'],
                                 self._rowLabelWidth)

        #PoP
        fcst=fcst+ self.makeRow( "PoP1h", self._Width, self._Periods,
                                 statList, self._scalarValue, ['PoP1h'],
                                 self._rowLabelWidth)

        #Obvis
        fcst=fcst+ self.makeRow( "Obvis", self._Width, self._Periods,
                                 statList, self._obvisValue, ['Wx'],
                                 self._rowLabelWidth)

        #Tstm Coverage term
        fcst=fcst+ self.makeRow( "Tstm ", self._Width, self._Periods,
                                 statList, self._wxTstm, ['Wx'],
                                 self._rowLabelWidth)

        #Tstm Coverage term
        fcst=fcst+ self.makeRow( "Tint ", self._Width, self._Periods,
                                 statList, self._wxTstmInt, ['Wx'],
                                 self._rowLabelWidth)

        #Wx1
        fcst=fcst+ self.makeRow( "PTyp1", self._Width, self._Periods,
                                 statList, self._wxVal, [0],
                                 self._rowLabelWidth)

        #Wx1 coverage
        fcst=fcst+ self.makeRow( "Prob1", self._Width, self._Periods,
                                 statList, self._wxValCov, [0],
                                 self._rowLabelWidth)        
        
        #Wx1 intensity
        fcst=fcst+ self.makeRow( "Ints1 ", self._Width, self._Periods,
                                 statList, self._wxValInst, [0],
                                 self._rowLabelWidth)

        #Wx2
        fcst=fcst+ self.makeRow( "PTyp2 ", self._Width, self._Periods,
                                 statList, self._wxVal, [1],
                                 self._rowLabelWidth)

        #Wx2 coverage
        fcst=fcst+ self.makeRow( "Prob2", self._Width, self._Periods,
                                 statList, self._wxValCov, [1],
                                 self._rowLabelWidth) 

        #Wx2 intensity
        fcst=fcst+ self.makeRow( "Ints2 ", self._Width, self._Periods,
                                 statList, self._wxValInst, [1],
                                 self._rowLabelWidth)

        #Wx3
        fcst=fcst+ self.makeRow( "PTyp3 ", self._Width, self._Periods,
                                 statList, self._wxVal, [2],
                                 self._rowLabelWidth)

        #Wx3 coverage
        fcst=fcst+ self.makeRow( "Prob3", self._Width, self._Periods,
                                 statList, self._wxValCov, [2],
                                 self._rowLabelWidth) 

        #Wx3 intensity
        fcst=fcst+ self.makeRow( "Ints3 ", self._Width, self._Periods,
                                 statList, self._wxValInst, [2],
                                 self._rowLabelWidth)

        #Predominate
#        fcst=fcst + self.makeRow( "PrdHt ", self._Width, self._Periods,
#                                  statList, self._scalarValue, ['PredHgt'],
#                                  self._rowLabelWidth) 
#
#        fcst=fcst + self.makeRow( "PrdCt ", self._Width, self._Periods,
#                                  statList, self._scalarValue, ['PredHgtCat'],
#                                  self._rowLabelWidth) 
#
#        #Conditional
#        fcst=fcst + self.makeRow( "CigHt ", self._Width, self._Periods,
#                                  statList, self._scalarValue, ['CigHgt'],
#                                  self._rowLabelWidth) 
#
#        fcst=fcst + self.makeRow( "CigCt ", self._Width, self._Periods,
#                                  statList, self._scalarValue, ['CigHgtCat'],
#                                  self._rowLabelWidth) 
#
#        #Vsby
#        fcst=fcst + self.makeRow( "Vsby ", self._Width, self._Periods,
#                                  statList, self._vsbyValue, ['Vsby'],
#                                  self._rowLabelWidth )
        return fcst
    
    ########################################################################
    # PRODUCT-SPECIFIC METHODS
    ########################################################################

    def _analysisList(self):
      return [
          ("T", self.avg),
          ("Wind", self.vectorAvg),
          ("WindGust", self.moderatedMax),
          ("Sky", self.avg),
          ("Td", self.avg),
          ("PoP", self.avg),
          ("Wx", self.dominantWx),
#          ("PredHgt", self.avg),
#          ("PredHgtCat", self.avg),
#          ("CigHgt", self.avg),
#          ("CigHgtCat", self.avg),
#          ("Vsby", self.avg)
          ]

    def _scalarValue(self, statDict, timeRange, argList):
        """Method to return a simple scalar as a string without any processing"""
        val = self.getStats(statDict, argList[0])
        try:
            return str(int(round(val)))
        except:
            return "999"

    def _vsbyValue(self, statDict, timeRange, argList):
        """Method to return a simple scalar as a string.  Value from grid is multiplied by 100.0"""
        val = self.getStats(statDict, argList[0])
        try:
            return str(int(round(val*100.)))
        except:
            return "999"

    def _windValue(self, statDict, timeRange, argList):
        """Method to return a vector component (direction or magnitude) as a
        string some processing"""
        windType = argList[0]
        windVal = self.getStats(statDict, "Wind")
        
        try:
            mag, dir = windVal

            if windType == "dir":
                return str(int(round(dir)/10.))
            else:
                return str(int(round(mag)))
        except:
            return "999"
            

    def _skyValue(self, statDict, timeRange, argList):
        """Method to return a simple scalar as a string with minor processing"""
        value =  self.getStats(statDict, argList[0])
        try:
            return str(int((value+5)/10.))
        except:
            return "999"
 
    def _obvisValue(self, statDict, timeRange, argList):
        """Find the first obstruction to vision type and report that"""
        wxStats = self.getStats(statDict, argList[0])
        wxType = '999'
        if wxStats is None:
            return wxType
        else:
            for subkey in wxStats:
                if subkey.wxType() in self._vsbyCodes:
                    wxType = subkey.wxType()
                    if wxType == 'F' or wxType == 'ZF' or wxType == 'IF':
                        if subkey.intensity() == '+':
                            wxType = 'F'
                        else:
                            wxType = 'BR'
                    break
        return wxType

    def _wxVal(self, statDict, timeRange, argList):
        """Get the Nth weather type.  N is given in argList[0]"""
        wxStats = self.getStats(statDict, 'Wx')
        value = '999'
        if wxStats is None:
            return value

        skip = argList[0]
        count = 0
        for subkey in wxStats:
            if subkey.wxType() not in ['<NoWx>','T'] + self._vsbyCodes:
                if count == skip:
                    value = subkey.wxType()
                    break
                else:
                    count = count + 1
                
        return value

    def _wxValInst(self, statDict, timeRange, argList):
        """Get the Nth weather intensity.  N is given in argList[0]"""
        wxStats = self.getStats(statDict, 'Wx')
        value = '999'
        if wxStats is None:
            return value

        skip = argList[0]
        count = 0
        for subkey in wxStats:
            if subkey.wxType() not in ['<NoWx>','T'] + self._vsbyCodes:
                if count == skip:
                    value = self._translateCode.get(subkey.intensity(), 'm' )
                    break
                else:
                    count = count + 1
            
        return value

    def _wxValCov(self, statDict, timeRange, argList):
        """Get the Nth weather coverage.  N is given in argList[0]"""
        wxStats = self.getStats(statDict, 'Wx')
        value = '999'
        if wxStats is None:
            return value

        skip = argList[0]
        count = 0
        for subkey in wxStats:
            if subkey.wxType() not in ['<NoWx>','T'] + self._vsbyCodes:
                if count == skip:
                    value = self._translateCode.get(subkey.coverage(), 'S' )
                    break
                else:
                    count = count + 1
            
        return value

    def _wxTstm(self, statDict, timeRange, argList):
        """Search for any reference to thunderstorms and, if found, return the coverage expected"""
        wxStats = self.getStats(statDict, argList[0])
        value = '999'
        if wxStats is None:
            return value

        for subkey in wxStats:
            if subkey.wxType() == 'T':
                value = self._translateCode.get(subkey.coverage(), 'S' )
                break
                
        return value

    def _wxTstmInt(self, statDict, timeRange, argList):
        """Search for any reference to thunderstorms and, if found, return the intensity expected"""
        wxStats = self.getStats(statDict, argList[0])
        value = '999'
        if wxStats is None:
            return value

        for subkey in wxStats:
            if subkey.wxType() == 'T':
                value = self._translateCode.get(subkey.intensity(), 'm' )
                break
                
        return value
    
    def _initializeFcst(self, argDict):
        fcstDict = {}   
        for output in self._outputList:
            fcst = ""
            # Make sure there is a crsID for each LAC
            if self._pilDict.has_key(output):
                self._currentPil = self._pilDict[output]
                self._currentoutput = output
                fcstDict[output] = self._preProcessProduct(fcst, argDict)
            else:
                msg = "Setup Error!  Definition['pilDict']" + \
                    " has no entry for ID=" + lac
                raise "definitionError", msg
        return fcstDict

    def _getFinalFcst(self, fcstDict, argDict):
        #
        # Write results to individual files
        for output in self._outputList:
            fcst = fcstDict[output]
            
            if self._outputFileDict.has_key(output):
                f=open(self._outputFileDict[output],"w")
                f.write(fcst)
                f.close()

        return ' '
