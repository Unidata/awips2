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

import sys, time, string, numpy
import pupynere as netcdf
import LogStream,JUtil
from java.util import ArrayList
from java.lang import String
import jep

from com.raytheon.uf.common.time.util import TimeUtil
from com.raytheon.uf.common.datastorage.records import IDataRecord
from com.raytheon.uf.common.datastorage.records import FloatDataRecord
from com.raytheon.uf.common.datastorage.records import StringDataRecord
from com.raytheon.uf.common.datastorage.records import ShortDataRecord
from com.raytheon.uf.common.datastorage.records import IntegerDataRecord
from com.raytheon.uf.common.datastorage.records import LongDataRecord
from com.raytheon.uf.common.datastorage.records import ByteDataRecord

from com.raytheon.uf.common.pointdata import PointDataContainer

#
#    Accesses the netCDF QC mesonet data
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/04/2009      #3408         bphillip       Initial Creation
#  
class NetCDFFile():

    ##
    #Initialzes the NetCDFFile object
    #@param fileName: The name of the file to open
    ##
    def __init__(self, fileName):
        self.__fileName = fileName
        self.__file = netcdf.netcdf_file(self.__fileName, "r")
        self.__vars = self.__file.variables
        self.__inventory = None
        self.__globalAttributes = None
        
    ##
    #Closes the file
    ##
    def close(self):
        self.__file.close()
    
    ##
    #Gets a variable from the file
    #@param varName: The name of the variable to retrieve
    #@return: The pupynere netcdf variable object
    ##  
    def getVariable(self, varName):
        return self.__vars[varName]
        
    ##
    #Gets an attribute for a variable from the file
    #@param varName: The variable for which to retrieve the attribute
    #@param attributeName: The attribute to retrieve
    ##
    def getAttribute(self, varName, attributeName):
        return getattr(self.getVariable(varName), attributeName)
        
    ##
    #Gets a data element for the specified variable at the specified index
    #@param varName: The variable from which to retrieve the data
    #@param index: The index of the the data to retrieve
    ##
    def getData(self, varName, index):
        return self.__vars[varName][int(index)]
        
    ##
    #Private method to get the inventory of the netCDF file.  The inventory is 
    #stored in a dictionary keyed by the station.  The values of the dictionary 
    #are another dictionary containing lists of the indices and observation times
    #for the data contained in the file.
    #
    # The inventory is stored in the self.__inventory variable
    #@see: self.__inventory
    ##
    def __getInventoryInternal(self):
        
        self.__inventory = {}
        
        dataProviders = self.getVariable('dataProvider')
        providerIds = self.getVariable('providerId')
        obsTimes = self.getVariable('observationTime')
        obsTimeFillValue = self.getAttribute('observationTime', '_FillValue')
        
        counter = 0
        for provider in dataProviders:
            providerName = self._charArrayToString(dataProviders[counter]) + ' ' + self._charArrayToString(providerIds[counter])
            if providerName in self.__inventory:
                self.__inventory[providerName]['indices'].append(counter)
            else:
                self.__inventory[providerName] = {}
                self.__inventory[providerName]['indices'] = []
            counter = counter + 1

        for key in self.__inventory.keys():
            indices = self.__inventory[key]['indices']
            self.__inventory[key]['obsTimes'] = []
            for rec in indices:
                self.__inventory[key]['obsTimes'].append(obsTimes[rec])

    ##
    #Gets the list of variables contained in the file
    #@return: The list of pupynere netcdfvariable objects
    ##
    def getVars(self):
        return self.__vars
    
    ##
    #Gets the list of variable names contained in the file
    #@return: A list of strings containing the variable names from the file
    ##
    def getVarList(self):
        return self.__vars.keys()
        
    ##
    #Gets the inventory of the file
    #@return: The inventory of the file
    #@see: __getInventoryInternal
    ##
    def getInventory(self):
        if self.__inventory is None:
            self.__getInventoryInternal()
        return self.__inventory
    
    ##
    #Gets a globally stored attribute
    #@param attribute: The attribute name to retrieve
    #@return: The attribute value
    ##
    def getGlobalAttribute(self, attribute):
        return getattr(self.__file, attribute)
 
    ##
    #Utility method to convert a char array to a string
    #@param arr: The char array to convert
    #@return: The string representation of the char array
    ##
    def charArrayToString(self, arr):
        theString = ''
        for letter in arr:
            if letter != '':
                theString = theString + letter
        return theString
    
##
#Gets the variable names from the file
#@param filename: The name of the file to examine
#@return: List of strings containing the variable names contained in the file
##
def getVars(fileName):
    file = NetCDFFile(str(fileName))
    varList = file.getVarList()
    file.close()
    return varList
    
##
#Gets the data for the specified attributes from the netCDF file filtered by the 
#query parameters provided
#@param fileName: The file from which to retrieve the data
#@param queryParameters: The parameters to use to filter the data
#@param ptDataDescription: The description of the parameters in the file
#@return: A PointDataContainer containing the requested attributes
##
def getDataSets(fileName, attributes, queryParameters, ptDataDescription):
    
    #Converts the attributes from a java.util.List to a python list
    attributes = JUtil.javaStringListToPylist(attributes)
    
    #Converts the query parameters from a java.util.List to a python list
    queryParameters = JUtil.javaStringListToPylist(queryParameters)
    
    dataSets = {}
    file = NetCDFFile(str(fileName))
    
    #Parses the query parameters into a more easily managed form 
    queryParameters = parseQueryParams(file, queryParameters) 
    
    #Initializes the lists to be constructed
    for attr in attributes:
        dataSets[attr] = []

    #Gets the data from the netCDF.  The data retrieved is filtered according to 
    #the query parameters provided.  This is essentially an inefficient query procedure
    for recordIndex in range(0, file.getVariable('observationTime').shape[0]):
        if _checkConditions(file, recordIndex, queryParameters):
            for attr in attributes:
                dataItem = file.getData(attr, recordIndex)
                dataSets[attr].append(dataItem)

    #If no data is retrieved, simply return an empty container
    if len(dataSets[dataSets.keys()[0]]) == 0:
        return PointDataContainer()
    
    recs = ArrayList()
    
    #Cycle through the requested attributes and format the data into a PointDataContainer
    for attr in attributes:
        
        #Get the type and size of the retrieved data
        dataType = type(dataSets[attr][0])
       
        sz = len(dataSets[attr])
        sizes = numpy.zeros(1, numpy.int32)
        sizes[0] = sz
            
        #If the dataset is an array, construct the IDataRecord accordingly
        if dataType == numpy.ndarray:
            sizes = numpy.zeros(2, numpy.int32)
            arrLen = len(dataSets[attr][0])
            sizes[0] = sz
            sizes[1] = arrLen
            arrType = type(dataSets[attr][0][0])
            
            if arrType == numpy.float32 or arrType == numpy.float64:
                arr = numpy.zeros((sz, arrLen), numpy.float32)
                for i in range(0, sz):
                    arr[0:][i] = dataSets[attr][i]
                arr = numpy.resize(arr, (1, sz * arrLen))
                rec = FloatDataRecord()
                rec.setFloatData(arr)
            elif arrType == numpy.int16 or arrType == numpy.int8 or arrType == numpy.int32:
                arr = numpy.zeros((sz, arrLen), numpy.int32)
                for i in range(0, sz):
                    arr[0:][i] = dataSets[attr][i]
                arr = numpy.resize(arr, (1, sz * arrLen))
                rec = IntegerDataRecord()
                rec.setIntData(arr)
            elif arrType == numpy.string_:
                jstr = jep.jarray(sz, String)
                for i in range(sz):
                    dataString = file.charArrayToString(dataSets[attr][i])
                    jstr[i] = String(dataString)
                rec = StringDataRecord(attr, "", jstr)
            else:
                file.close()
                LogStream.logProblem("Unsupported data type detected: "+str(arrType))
                return None
            
            if arrType != numpy.string_:
                rec.setName(attr)
                rec.setDimension(2)
                rec.setIntSizes(sizes)
                rec.setGroup("")

        #The dataset is not an array type so examine the data and create the appropriate
        #type of IDataRecord
        else:
            #Creates a FloatDataRecord
            if dataType == numpy.float32 or dataType == numpy.float64:
                arr = numpy.zeros(sz, numpy.float32)
                for i in range(0, sz):
                    arr[i] = dataSets[attr][i]
                rec = FloatDataRecord()
                rec.setFloatData(arr)
                
            #Creates an IntDataRecord
            elif dataType == numpy.int16 or dataType == numpy.int8 or dataType == numpy.int32:
                arr = numpy.zeros(sz, numpy.int32)
                for i in range(0, sz):
                    arr[i] = dataSets[attr][i]
                rec = IntegerDataRecord()
                rec.setIntData(arr)
            
            #Creates a StringDataRecord
            elif dataType == numpy.string_:
                jstr = jep.jarray(sz, String)
                for i in range(sz):
                    jstr[i] = String(dataSets[attr][i])
                rec = StringDataRecord(attr, "", jstr)
            else:
                file.close()
                LogStream.logProblem("Unsupported data type detected: "+str(dataType))
                return None
                
            # Sets the required data on the IDataRecord.
            # This is already done for for the StringDataRecord
            if dataType != numpy.string_:
                rec.setName(attr)
                rec.setDimension(1)
                rec.setIntSizes(sizes)
                rec.setGroup("")
        recs.add(rec)
    
    #Close the file
    file.close()
    #Populate the container
    return PointDataContainer.build(ptDataDescription, recs)

def getPointData(args):
    return getPointData2(args.getFileName(), args.getAttributes(), args.getIndexes(), args.getPdd())

##
#Gets the data for the specified attributes from the netCDF file filtered by the 
#query parameters provided
#@param fileName: The file from which to retrieve the data
#@param queryParameters: The parameters to use to filter the data
#@param ptDataDescription: The description of the parameters in the file
#@return: A PointDataContainer containing the requested attributes
##
def getPointData2(fileName, attributes, indexes, ptDataDescription):
    
    dataSets = {}
    file = NetCDFFile(str(fileName))
    
    #Initializes the lists to be constructed
    for attr in attributes:
        dataSets[attr] = []

    for recordIndex in indexes:
        for attr in attributes:
            dataItem = file.getData(attr, recordIndex)
            dataSets[attr].append(dataItem)

    #If no data is retrieved, simply return an empty container
    if len(dataSets) == 0 or len(dataSets[dataSets.keys()[0]]) == 0:
        return PointDataContainer()
    
    recs = ArrayList(len(indexes))
    
    #Cycle through the requested attributes and format the data into a PointDataContainer
    for attr in attributes:
        
        #Get the type and size of the retrieved data
        dataType = type(dataSets[attr][0])
       
        sz = len(dataSets[attr])
        sizes = numpy.zeros(1, numpy.int32)
        sizes[0] = sz
            
        #If the dataset is an array, construct the IDataRecord accordingly
        if dataType == numpy.ndarray:
            sizes = numpy.zeros(2, numpy.int32)
            arrLen = len(dataSets[attr][0])
            sizes[0] = sz
            sizes[1] = arrLen
            arrType = type(dataSets[attr][0][0])
            
            if arrType == numpy.float32 or arrType == numpy.float64:
                arr = numpy.zeros((sz, arrLen), numpy.float32)
                for i in range(0, sz):
                    arr[0:][i] = dataSets[attr][i]
                arr = numpy.resize(arr, (1, sz * arrLen))
                rec = FloatDataRecord()
                rec.setFloatData(arr)
            elif arrType == numpy.int16 or arrType == numpy.int8 or arrType == numpy.int32:
                arr = numpy.zeros((sz, arrLen), numpy.int32)
                for i in range(0, sz):
                    arr[0:][i] = dataSets[attr][i]
                arr = numpy.resize(arr, (1, sz * arrLen))
                rec = IntegerDataRecord()
                rec.setIntData(arr)
            elif arrType == numpy.string_:
                jstr = jep.jarray(sz, String)
                for i in range(sz):
                    dataString = file.charArrayToString(dataSets[attr][i])
                    jstr[i] = String(dataString)
                rec = StringDataRecord(attr, "", jstr)
            else:
                file.close()
                LogStream.logProblem("Unsupported data type detected: "+str(arrType))
                return None
            
            if arrType != numpy.string_:
                rec.setName(attr)
                rec.setDimension(2)
                rec.setIntSizes(sizes)
                rec.setGroup("")

        #The dataset is not an array type so examine the data and create the appropriate
        #type of IDataRecord
        else:
            #Creates a FloatDataRecord
            if dataType == numpy.float32 or dataType == numpy.float64:
                arr = numpy.zeros(sz, numpy.float32)
                for i in range(0, sz):
                    arr[i] = dataSets[attr][i]
                rec = FloatDataRecord()
                rec.setFloatData(arr)
                
            #Creates an IntDataRecord
            elif dataType == numpy.int16 or dataType == numpy.int8 or dataType == numpy.int32:
                arr = numpy.zeros(sz, numpy.int32)
                for i in range(0, sz):
                    arr[i] = dataSets[attr][i]
                rec = IntegerDataRecord()
                rec.setIntData(arr)
            
            #Creates a StringDataRecord
            elif dataType == numpy.string_:
                jstr = jep.jarray(sz, String)
                for i in range(sz):
                    jstr[i] = String(dataSets[attr][i])
                rec = StringDataRecord(attr, "", jstr)
            else:
                file.close()
                LogStream.logProblem("Unsupported data type detected: "+str(dataType))
                return None
                
            # Sets the required data on the IDataRecord.
            # This is already done for for the StringDataRecord
            if dataType != numpy.string_:
                rec.setName(attr)
                rec.setDimension(1)
                rec.setIntSizes(sizes)
                rec.setGroup("")
        recs.add(rec)
    
    #Close the file
    file.close()
    #Populate the container
    return PointDataContainer.build(ptDataDescription, recs)

##
#Check the data retrieved agains the query parameters provided to see if 
#the data should be included in the returned data set
#@param file: The netCDF file containing the data
#@param recordIndex: The index of the record in the netCDF file
#@param queryParameters: The parameters to use to filter the data
#@return: True if the data at the specified index passed the query criteria
#else false
##
def _checkConditions(file, recordIndex, queryParameters):
    
    conditionsPass = True

    for param in queryParameters:
        
        #Gets the fill value for the attribute from the file
        try:
            fillValue = file.getAttribute(param[0], "_FillValue")
        except:
            fillValue = None
            
        #Gets the missing_value value for the attribute from the file
        try:
            missingValue = file.getAttribute(param[0], "missing_value")
        except:
            missingValue = None
        
        #Get the data and determine the type
        currentData = file.getData(param[0], recordIndex)
        dataType = type(currentData)
        
        #Fail the test if the data is the fill value or missing
        if (currentData == fillValue) or (currentData == missingValue):
            conditionsPass = False;
            
        #Execute comparisons
        elif param[2] is '=':
            conditionsPass = file.getData(param[0], recordIndex) == _stringCast(param[1], dataType)
        elif param[2] is '<':
            conditionsPass = file.getData(param[0], recordIndex) < _stringCast(param[1], dataType)
        elif param[2] is '<=':
            conditionsPass = file.getData(param[0], recordIndex) <= _stringCast(param[1], dataType)
        elif param[2] is '>':
            conditionsPass = file.getData(param[0], recordIndex) > _stringCast(param[1], dataType)
        elif param[2] is '>=':
            conditionsPass = file.getData(param[0], recordIndex) >= _stringCast(param[1], dataType)
    
        if not conditionsPass:
            break;
        
    return conditionsPass

##
#Casts a string object to the desired type
#@param value: The value to convert
#@param desiredType: The numpy type to convert the value to
#@return: The converted value
##
def _stringCast(value, desiredType):
    return {
              str: lambda value: value,
              numpy.float32: lambda value:float(value),
              numpy.float64: lambda value:float(value),
              numpy.int8: lambda value:int(value),
              numpy.int16: lambda value:int(value),
              numpy.int32: lambda value:int(value)
              }[desiredType](value)

##
#Breaks the query parameters into a more manageable form
#Each query parameter is passed in as a string of the form:
#attribute  value  operatore
#@param file: The file containing the data
#@param queryParams: The list of query params in the aforementioned form
#@return: Returns a list of params
##
def parseQueryParams(file, queryParams):
    
    timeFields = file.getGlobalAttribute('timeVariables').split(',')
    params = []
    
    # Split the query parameters into a list
    for param in queryParams:
        currentParam = str(param).split("  ")
        
        # Correct the time if it is a time field
        if currentParam[0] in timeFields:
            currentParam[1] = TimeUtil.formattedDateToLong(currentParam[1]) / 1000
        
        params.append(currentParam)

    
    return params
