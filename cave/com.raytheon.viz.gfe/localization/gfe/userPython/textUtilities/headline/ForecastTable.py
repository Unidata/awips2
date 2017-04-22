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
# ForecastTable.py
#
# Forecast type: "table"
# Class for processing table Forecasts.
#
# Author: hansen
# ----------------------------------------------------------------------------

import string, getopt, sys, time, types
import TextRules
import SampleAnalysis

class WeEntry:
    def __init__(self, name, label, analysis, format, dataType,
                 roundVal=5, conversion=None, maxWidth=3):    
        # Weather element name,
        self.__name = name
        # Weather Element label --
        # If you want the label to appear on  multiple lines,
        #  separate the lines with vertical bars
        #  e.g. Maximum|Temperature 
        self.__label = label
        # SampleAnalysis method
        self.__analysis = analysis
        # Method to format the analyzed value(s)given dataType
        #   TextRules.singleValue --
        #           needs one value : reports single value
        #   TextRules.range2Value --
        #           needs two values : reports range of 2 values
        #NOTE: The Format method must accept input which matches
        # the output of the analysis method.
        self.__format = format
        # DataType: Scalar or Vector
        self.__dataType = dataType
        # Rounding increment e.g. 5 = round final value to
        #                       nearest multiple of 5
        self.__roundVal = roundVal
        # Conversion method
        self.__conversion = conversion
        # Number of digits in max value -- used to determine
        #         column width for entries
        self.__maxWidth = maxWidth
        # Period can be set during processing for TextRules
        # to access.
        self.__period = None
    def name(self):
        return self.__name
    def label(self):
        return self.__label
    def analysis(self):
        return self.__analysis
    def format(self):
        return self.__format
    def dataType(self):
        return self.__dataType
    def roundVal(self):
        return self.__roundVal
    def conversion(self):
        return self.__conversion
    def maxWidth(self):
        return self.__maxWidth
    def setPeriod(self, period):
        self.__period = period
    def getPeriod(self):
        return self.__period

class TableVariables:

    # Class to hold the row/column variables for the table
    
    def __init__(self, constVal, rowList, colList,
                 setElement, setPeriod, setArea):
        
        # This is the value of the variable held constant.
        self.__constVal = constVal

        # rowList and colList are tuples of label, value pairs
        self.__rowList = rowList
        self.__colList = colList

        # These are executable python statements to set up the
        # current element, period, and area from the current
        # row, column, or constant value. 
        self.__setElement = setElement
        self.__setPeriod = setPeriod
        self.__setArea = setArea
        
    def constVal(self):
        return self.__constVal
    def rowList(self):
        return self.__rowList
    def colList(self):
        return self.__colList
    def setElement(self):
        return self.__setElement
    def setPeriod(self):
        return self.__setPeriod
    def setArea(self):
        return self.__setArea
    
class ForecastTable(TextRules.TextRules, SampleAnalysis.SampleAnalysis):
    def __init__(self):
        TextRules.TextRules.__init__(self)
        SampleAnalysis.SampleAnalysis.__init__(self)

    def generateForecast(self, argDict):
        # Create a table

        # Set Up table variables
        self.__argDict = argDict
        self.__language = argDict["language"]
        self.__rowHeading = argDict["heading"]

        self.__userDict = {}
        
        fcstDef = argDict["forecastDef"]
        self.__ut = argDict["utility"]
        
        begText = self.__ut.set(fcstDef,"beginningText","")
        endText = self.__ut.set(fcstDef,"endingText","")
        if type(begText) in [types.MethodType, types.FunctionType]:
            begText = begText(self, fcstDef, argDict)
        if type(endText) in [types.MethodType, types.FunctionType]:
            endText = endText(self, fcstDef, argDict)
        editAreaLoopBegText = self.__ut.set(fcstDef,"editAreaLoopBegText","")
        timeRangeLoopBegText = self.__ut.set(fcstDef,"timeRangeLoopBegText","")
        editAreaLoopEndText = self.__ut.set(fcstDef,"editAreaLoopEndText","")
        timeRangeLoopEndText = self.__ut.set(fcstDef,"timeRangeLoopEndText","")

        self.__loopMethod = self.__ut.set(fcstDef,"loopMethod")
        self.__endMethod = self.__ut.set(fcstDef,"endMethod")
        if type(self.__loopMethod) == types.StringType:
            exec "self.__loopMethod = self."+self.__loopMethod
        if type(self.__endMethod) == types.StringType:
            exec "self.__endMethod = self."+self.__endMethod

        colJust = self.__ut.set(fcstDef,"columnJustification",
                                       "Center")
        if colJust == "Center":
            self.__alignMethod = string.center
        elif colJust == "Right":
            self.__alignMethod = string.rjust
        else:
            self.__alignMethod = string.ljust

        self.__minColWidth =  self.__ut.set(fcstDef,"minimumColumnWidth",8)
        
        constVar = self.__ut.set(fcstDef, "constantVariable", "period")
        outerLoop = self.__ut.set(fcstDef, "outerLoop", "EditArea")
        self.__editAreas = argDict["editAreas"]
        self.__rawRanges = argDict["rawRanges"]
        # Loop through constant variable if multiple edit areas
        # or ranges given
        fcst = begText
        if constVar == "EditArea":
            if outerLoop == "EditArea":
                for editArea in self.__editAreas:
                    argDict["editArea"] = editArea
                    fcst = fcst + editAreaLoopBegText
                    for rawRange, rangeName in self.__rawRanges:
                        fcst = fcst + timeRangeLoopBegText
                        argDict["timeRange"] = rawRange
                        argDict["timeRangeName"] = rangeName
                        fcst = fcst + self.__generateTable(fcstDef, argDict)
                        fcst = fcst + timeRangeLoopEndText
                        fcst = self.fillSpecial(fcst, argDict)
                    fcst = fcst + editAreaLoopEndText
                    fcst = self.fillSpecial(fcst, argDict)
            else:
                for rawRange, rangeName in self.__rawRanges:
                    argDict["timeRange"] = rawRange
                    argDict["timeRangeName"] = rangeName
                    fcst = fcst + timeRangeLoopBegText
                    for editArea in self.__editAreas:
                        argDict["editArea"] = editArea
                        fcst = fcst + editAreaLoopBegText
                        fcst = fcst + self.__generateTable(fcstDef, argDict)
                        fcst = fcst + editAreaLoopEndText
                        fcst = self.fillSpecial(fcst, argDict)
                    fcst = fcst + timeRangeLoopEndText
                    fcst = self.fillSpecial(fcst, argDict)
        elif constVar == "TimePeriod" or constVar == "WeatherElement":
            for rawRange, rangeName in self.__rawRanges:
                argDict["timeRange"] = rawRange
                argDict["timeRangeName"] = rangeName
                fcst = fcst + timeRangeLoopBegText
                fcst = fcst + self.__generateTable(fcstDef, argDict)                
                fcst = fcst + timeRangeLoopEndText
                fcst = self.fillSpecial(fcst, argDict)
        fcst = fcst + endText
        fcst = self.fillSpecial(fcst, argDict)
        return fcst
                    
    def __generateTable(self, fcstDef, argDict):
        # Set up the constant value, row values, and column variables 
        tableVars = self.__setupVariables(fcstDef, argDict)
        if tableVars is None:
            return self.__errorMsg
        if self.__sampler is None:
            return "Cannot Sample Database: Check for Invalid Weather Elements or Edit Areas"

        # Add to argDict so endMethod could use it for labeling the
        # table
        argDict["tableVars"] = tableVars

        # Create Table
        
        # Beginning Text
        table = ""
        # Create table heading and determine column lengths
        tableHeading, colLengths = self.__getTableHeading(
            self.__rowHeading, tableVars)
        table = table + tableHeading

        # Fill in  each Row and Column
        for rowValue, rowLabel in tableVars.rowList():
            # rowEntries is a list of colValue, value tuples
            # describing the entries in this row.
            # It is passed to a loopMethod which might be
            # collecting statistics or modifying the row.
            rowEntries = []
            entries = [rowLabel]
            for colValue, colLabel in tableVars.colList():
                entry = self.__getNextEntry(
                    tableVars, colValue, rowValue, rowEntries)
                entries.append(entry)
            row = self.__createRow(entries, colLengths)
            # Hook for gathering statistics or modifying row
            if not self.__loopMethod is None:
                args = self.__loopMethod.func_code.co_varnames
                if args[0] == "self":
                    rowText = self.__loopMethod(
                        self, row, rowLabel, rowEntries, self.__userDict, argDict)
                else:
                    rowText = self.__loopMethod(
                        row, rowLabel, rowEntries, self.__userDict, argDict)
                # For backward compatibility, only use returned value if is not None
                if rowText is not None:
                    row = rowText
            table = table + row
        
        # Call User's end method
        if not self.__endMethod is None:
            args = self.__endMethod.func_code.co_varnames
            if args[0] == "self":
                table  = self.__endMethod(self, table, self.__userDict, argDict)
            else:
                table  = self.__endMethod(table, self.__userDict, argDict)
                
            
        argDict["element"] = self.__weList[0].name()

        # Translate
        table  = self.translateForecast(table, self.__language)
        # Generate Html
        self.__doHtml(argDict, table)
        return table

    def __createRow(self, rowEntries, colLengths):
        # Given a set of rowEntries and corresponding colLengths
        # return a row which can span more than one line.
        # rowEntries will have vertical bars separating entry lines.
        # E.g. rowEntry = "PARTLY|CLOUDY" will appear on two lines
        # The first rowEntry is left-justified, the rest use the
        # self.__alignMethod
        lines = [""]
        index = 0
        # First row entry is left-justified
        alignMethod = string.ljust
        for rowEntry in rowEntries:            
            # Split entry into list of words
            words = string.split(rowEntry, "|")

            if len(words) > len(lines):
                # Add more lines if necessary
                for i in range(len(words)-len(lines)):
                    spaces = 0
                    for ind in range(0,index):
                        spaces = spaces + colLengths[ind]
                    lines.append(alignMethod("",spaces))

            # Add the words to the appropriate line of the heading
            ind = 0
            newlines = []
            for line in lines:
                if ind > len(words)-1:
                    word = ""
                else:
                    word = words[ind]
                line = line + alignMethod(word, colLengths[index])
                newlines.append(line)
                ind = ind + 1
            lines = newlines
                                                 
#            for word in words:
#                lines[words.index(word)] = lines[words.index(word)] + \
#                      alignMethod(word,colLengths[index])

            # After first time through, switch the alignment method
            alignMethod = self.__alignMethod
            index = index + 1
            
        # Compose the lines of the row
        row = ""
        for line in lines:
            row = row + line + "\n"
        return row
        
    def __setupVariables(self, fcstDef, argDict):

        constVar = self.__ut.set(fcstDef, "constantVariable", "period")
        rowVar = self.__ut.set(fcstDef, "rowVariable", "area")
        columnVar = self.__ut.set(fcstDef, "columnVariable", "element")
        periodLabelMethod = self.__ut.set(fcstDef,"periodLabelMethod",
                                      self.periodLabel)
        if type(periodLabelMethod) == types.StringType:
            exec "periodLabelMethod = self."+periodLabelMethod
        periodLabelFormat = self.__ut.set(fcstDef,"periodLabelFormat",
                                      None)
        weTuples = self.__ut.set(fcstDef,"elementList")
        self.__weList = self.__setUpElements(weTuples)
        if self.__weList is None:
            return None
        period = argDict["timePeriod"]
        span = self.__ut.set(fcstDef,"timeSpan", period)
        if span == "timePeriod":
            span = period
        
        # Set up Constant variable
        # If time varies, we need to compute a variable histo
        #  for each time period.
        if constVar == "TimePeriod":
            constVal = argDict["timeRange"]
            periods = [(constVal, "")]
            setPeriod = "period = constVal"
        if constVar == "EditArea":
            constVal, constLabel = argDict["editArea"]
            areas = [(constVal, constLabel)]
            setArea = "area = constVal"
        if constVar == "WeatherElement":
            constVal = self.__weList[0]
            elements = [constVal]
            setElement = "element = constVal"

        # Set up row variables and labels
        if rowVar == "TimePeriod":
            periods = rowList = self.getPeriods(
                argDict["timeRange"], period, span, None, periodLabelMethod,
                periodLabelFormat)
            setPeriod = "period = rowValue"
        if rowVar == "EditArea":
            areas = rowList = self.__editAreas
            setArea = "area = rowValue"
        if rowVar == "WeatherElement":
            elements = rowList = self.__getElements(self.__weList)
            setElement = "element = rowValue"

        # Set up column variables and labels
        if columnVar == "TimePeriod":
            periods = colList = self.getPeriods(
                argDict["timeRange"], period, span, None, periodLabelMethod,
                periodLabelFormat)
            setPeriod = "period = colValue"
        if columnVar == "EditArea":
            areas = colList = self.__editAreas
            setArea = "area = colValue"
        if columnVar == "WeatherElement":
            elements = colList = self.__getElements(self.__weList)
            setElement = "element = colValue"

        # Create the HistoSampler and the SampleAnalysis objects
        elementList = []
        for element in self.__weList:
            elementList.append((element.name(), element.analysis()))
        sampleInfo = [(elementList, periods, areas)]
        self.__sampler = self.getSampler(argDict, sampleInfo)
        return TableVariables(constVal, rowList, colList,
                              setPeriod, setArea, setElement)

    def __getElements(self, weList):
        # Make a list of label, element tuples using the element list
        elementList = []
        for element in weList:
            elementList.append((element, element.label()))
        return elementList
                    
    def __getTableHeading(self, rowType, tableVars):
        # Create the column label line(s) from the colList information
        # Return the column lengths

        # The table heading can be multiple lines.
        # The rowType is the leftmost heading followed by the
        # headings for the columns.
        # The heading for a Weather element will contain slashes to denote
        # how the heading is split between lines.
        #  E.g. AREAL/COVERAGE  will appear on two lines: 
        #  AREAL centered over COVERAGE

        colLengths = []

        # Find the maximum rowValue length
        maxlen = len(rowType)
        for rowValue, rowLabel in tableVars.rowList():
            if len(rowLabel) > maxlen:
                maxlen = len(rowLabel)

        # Set up the row type label e.g. City
        colLengths.append(maxlen + 2)
        rowHeading = string.ljust(rowType, colLengths[0])

        # Set up the column labels            
        entries = [rowHeading]
        for colValue, colLabel in tableVars.colList():
            # Split label into list of words separated by slashes,
            # one word for each line, padded and centered relative to 
            # longest word
            entries.append(colLabel)
            words = string.split(colLabel, "|")
            maxlen = len(words[0])
            maxlen = maxlen + 2
            if maxlen < self.__minColWidth: 
                maxlen = self.__minColWidth
            colLengths.append(maxlen)

        # Compose the lines of the table heading
        tableHeading = self.__createRow(entries, colLengths)
        tableHeading = tableHeading + "\n"

        return tableHeading, colLengths
        
    def __getNextEntry(self, tableVars, colValue, rowValue, rowEntries):

        constVal = tableVars.constVal()
        # Set up variables: period, area, element
        exec tableVars.setPeriod()
        exec tableVars.setArea()
        exec tableVars.setElement()

        # Analyze the entry given the area, element, and period
        analysisList = [(element.name(), element.analysis())]
        analysisList = self.convertAnalysisList(analysisList)
        statDict = self.getStatDict(
            self.__sampler, analysisList, period, area)
        value = statDict[element.name()]
        rowEntries.append((colValue, value))

        # Format the analyzed value(s)
        element.setPeriod(period)
        format = element.format()
        #print "element", element.name(), value
        # If format method is in quotes, assume a method within this
        # inheritance hierarchy.
        if type(format) == types.StringType:
            exec "format = self."+format
            entry = format(element, value)
        else:
            # Check for first argument "self"
            args = format.func_code.co_varnames
            if args[0] == "self":
                entry = format(self, element, value)
            else:
                entry = format(element, value)                               
        return entry
        
    def __doHtml(self, argDict, table):
        # Generate HTML if required
        if argDict.has_key("template") and not argDict["template"] is None:
            template = argDict["template"]
            htmlFile = argDict["htmlFile"]
            htmlTable = "<pre>" + table + "</pre>"
            argDict["type"] = type
            valueDict = self.getValueDict(argDict)  
            valueDict["Table"] = htmlTable
            valueDict['AudioText'] = table
            argDict["issueRange"] = argDict["timeRange"]
            valueDict['Time'] = self.getIssueTime(argDict)
            self.generateHtml(valueDict, template, htmlFile)

    def __setUpDict(self, list):
        # Set up a dictionary from the list of tuples
        if list is None:
            return None
        dict = {}
        for name, value in list:
            dict[name] = value
        return dict

    def __setUpElements(self, list):
        # Set up a list of WeEntries from the list of tuples
        if list is None:
            self.__errorMsg = "Empty Weather Element List."
            return None
        newList = []
        for name,label,analysis,format,dataType,roundVal,conversion in list:
            if type(conversion) == types.StringType:
                exec "conversion = self."+conversion
            newList.append(WeEntry(name,label,analysis,format,dataType,
                                   roundVal, conversion))
        return newList        

    
